-- Funktionen, um besser mit Pixeln/Bildern umgehen zu können.
module PixelHandling where

import Network.MateLight.Simple
 
-- Schwarzes Bild
black_pic :: [ ( (Int,Int) , Pixel) ]
black_pic = [ ((x,y), black_p) | y <- [0 .. ydim - 1] , x <- [0 .. xdim - 1] ]


-- Variablen
-- Wird hier gebraucht, denn sonst sind die Funktionenköpfe zu groß...
-- TODO: Beheben -> Wird einfach hier definiert [?!]

-- Feldgröße
dim :: (Int,Int)
dim = (30,12)
xdim, ydim :: Int
xdim = fst dim
ydim = snd dim


------------------------------------------------------------------------------------------
 
-- Pixelfarben

red_p :: Pixel
red_p = Pixel 255 0 0

green_p :: Pixel
green_p = Pixel 0 255 0

blue_p :: Pixel
blue_p = Pixel 0 0 255

yellow_p :: Pixel
yellow_p = Pixel 255 255 0

white_p :: Pixel
white_p = Pixel 255 255 255

black_p :: Pixel
black_p = Pixel 0 0 0



colors :: [Pixel]
colors = [red_p, green_p, blue_p, white_p]

---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------

frameMix :: [( (Int,Int) , Pixel)] -> [( (Int,Int) , Pixel)] -> [( (Int,Int) , Pixel)]
-- frameMix kann mit 2 Bildern umgehen.
-- Es soll ein Bild entstehen, bei dem das erste Bild über dem zweiten liegt.
frameMix xs ys = [ ( (x,y) , if c1 /= black_p then c1 else c2 ) | ( (x,y) , c1, c2) <- helper]
 where helper = [ ((x,y), d, e) | ( ( (x,y) , d ) , ( (x',y') , e) ) <- zip xs ys ]


-- Das gleiche wie frameMix, nur nimmt es eine Liste von Bildern.
-- Dabei überdeckt das erste Bild der Liste den Rest, darunter ist das zweite Bild, 
-- was den Rest überdeckt usw.
adv_frameMix :: [ [( (Int,Int) , Pixel)] ] -> [( (Int,Int) , Pixel)]
adv_frameMix xs = foldl frameMix black_pic xs

-- Nimmt sich eine Funktion, die einer charakteristischen Funktion ähnelt und eine Farbe
-- Gibt ein Bild zurück, welches von der Funktion bestimmt ist.
frame1 :: (Int -> Int -> Pixel -> Pixel) -> Pixel -> [( (Int,Int) , Pixel) ]
frame1 f c = [ ( (x,y), f x y c ) | y <- [0 .. ydim - 1], x <- [0 .. xdim - 1] ] 

-- linke Seite ist Pixelfarbe
ch_left_side :: Int -> Int -> Pixel -> Pixel
ch_left_side x y c = if x <= div xdim 2 then c else black_p

-- rechte Seite ist Pixelfarbe
ch_right_side :: Int -> Int -> Pixel -> Pixel
ch_right_side x y c =  if x >= div xdim 2 then c else black_p

-- Oben links ist Pixelfarbe
ch_top_left :: Int -> Int -> Pixel -> Pixel
ch_top_left x y c  | x <= div xdim 2 && y <= div ydim 2 = c 
                   | otherwise                          = black_p

-- Unten links ist Pixelfarbe
ch_bot_left :: Int -> Int -> Pixel -> Pixel
ch_bot_left x y c  | x <= div xdim 2 && y >= div ydim 2 = c 
                   | otherwise                          = black_p

-- Oben rechts ist Pixelfarbe
ch_top_right :: Int -> Int -> Pixel -> Pixel
ch_top_right x y c | x >= div xdim 2 && y <= div ydim 2 = c 
                   | otherwise                          = black_p

-- Unten rechts ist Pixelfarbe
ch_bot_right :: Int -> Int -> Pixel -> Pixel
ch_bot_right x y c | x >= div xdim 2 && y >= div ydim 2 = c 
                   | otherwise                          = black_p


ch_column :: Int -> Int -> (Int,Int) -> Int -> Pixel -> Pixel
ch_column x y (x',y') h c | x == x' && y <= y' + h && y >= y' - h = c 
                          | otherwise                             = black_p
--

ch_quader :: Int -> Int -> (Int, Int) -> Int -> Int -> Pixel -> Pixel
ch_quader x y (x',y') h w c
 | x <= x' + w && x >= x' + w && y <= y' + h && y >= y' - h = c
 | otherwise                                                = black_p
 
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

-- Funktionen, um gewisse Strukturen in das Bild zu malen.
-- Je weiter nach unten, desto mehr Struktur

-- Setzt einen Pixel auf die gewünschte Farbe in das Feld
-- Bekommt das Bild, die Farbe und die gewünschte Koordinate
-- FRAGE: Geht das effizienter insofern, dass nicht das gesamte Bild durchgegangen werden muss? 
setP :: [ ( (Int, Int) , Pixel) ] -> (Int,Int) -> Pixel -> [ ( (Int, Int) , Pixel) ]
setP xs (x,y) c = [ ( (x',y') , if (x',y') == (x,y) then c else c' ) | ( (x',y') , c') <- xs ]
 
-- Malt eine halbe Kolumne nur nach oben, sonst wie setColumn
-- Für eine Kolumne nach unten einfach -h einsetzen
sethColumn :: [ ( (Int, Int) , Pixel) ] -> Pixel -> Int -> (Int,Int) -> [ ( (Int, Int) , Pixel) ]
sethColumn xs c h (x,y) = helper (y+h)
 where
 helper k | k == y    = setP xs (x,y) c
          | k > y     = setP ( helper (k-1) ) (x,k) c 
          | k < y     = setP ( helper (k+1) ) (x,k) c
--          | otherwise = setP xs (x,y) c
 
-- Färbt eine Spalte ein.
-- Bekommt das Bild, eine Farbe, die Höhe der (halben) Spalte, den Mittelpunkt 
-- Gebt das gleiche Bild zurück, wobei der Block eingezeichnet wurde.
setColumn :: [ ( (Int, Int) , Pixel )  ] -> Pixel -> Int -> (Int,Int) -> [ ( (Int, Int) , Pixel) ]
setColumn xs c h (x,y) = helper (y + h)
 where 
 helper k | k <= (y-h)  = setP xs (x,y-h) c
          | otherwise   = setP ( helper (k-1) ) (x,k) c
--

--alternativer Code für setColumn. TODO ausprobieren
setColumn2 :: [ ( (Int, Int) , Pixel )  ] -> Pixel -> Int -> (Int,Int) -> [ ( (Int, Int) , Pixel) ]
setColumn2 xs c h (x,y) = helper (y + h)
 where 
 helper k | k == y    = helper2 y
          | otherwise = setP ( helper (k-1) ) (x,k) c
 helper2 i | i <= (y-h) = setP xs (x,y-h) c
           | otherwise  = setP ( helper2 (i-1) ) (x,i) c

-- Erstellt ein Rechteck
-- Bekommt ein Bild, eine Farbe, eine Höhe, eine Breite und den (x,y) - Punkt unten links.

setRectangle_nadv :: [ ( (Int,Int) , Pixel) ] -> Pixel -> Int -> Int -> (Int,Int) -> [ ( (Int,Int) , Pixel ) ]
setRectangle_nadv xs c h w (x,y) = helper (x + w)
 where 
 helper k | k == x    = sethColumn xs c h (x, y)
          | k > x     = sethColumn ( helper (k-1) ) c h (k,x)
          | k < x     = sethColumn ( helper (k+1) ) c h (k,x)
--
-- Erstellt ein Rechteck
-- Erhält zwei (x,y)-Koordinaten und versucht ein Rechteck dazwischen zu zeichnen, sowie auszufüllen.
setRectangle :: [ ( (Int,Int) , Pixel) ] -> Pixel -> (Int,Int) -> (Int,Int) -> [ ( (Int,Int) , Pixel ) ]
setRectangle xs c (x1,y1) (x2,y2) = setRectangle_nadv xs c (y2-y1) (x2-x1) (x1,y1)


--vergleiche :: [ ( (Int,Int) , Pixel) ]
-- vergleiche xs ys = undefined

{-
-- So wie oben. Nur bekommt es den Mittelpunkt und nicht den Punkt unten links
-- Wobei die Höhe eigentlich nur die hälfte der eigentlichen Höhe ist. 
-- Das heißt es wird bei gegebenem h, h Schritte nach oben gegangen.
setRectangle2 :: [ ( (Int, Int) , Pixel ) ] -> Pixel -> Int -> Int -> (Int,Int) -> [ ( (Int, Int), Pixel ) ]
setRectangle2 xs c h w (x,y) = helper (x + w)
 where 
 helper k | k <= (x-w) = setColumn xs c h ( (x-w) , y)
          | otherwise  = setColumn ( helper (k-1) ) c h (k,x)
-}
 
-- Erstellt ein rechtwinkliges Dreieck
-- Bekommt ein Bild, eine Farbe, die Höhe, die Breite und den Punkt, an dem sich der rechte Winkel aufhalten soll.
-- Die Höhe wird von der gegebenen Koordinate gegangen. So auch die Breite
-- Höhe geht nach oben, Breite nach rechts.
-- Für ein Dreieck, mit einer Spitze links setzt man eine negative Breite ein.
-- Analog die Höhe negativ bei gewünschter Spitze nach unten.
setrTriangle :: [ ( (Int, Int) , Pixel ) ] -> Pixel -> Int -> Int -> (Int,Int) -> [ ( (Int,Int) ,Pixel ) ]
setrTriangle xs c h w (x,y) = helper (x + w) h
 where
 helper k h' | h' == y = undefined
             | otherwise = undefined
--








{-
map f xs = [ f x | x <- xs]
    f xs = 
	for y aus [y-h bis y+h]
	wende setP an. <- schlecht
	wende auf das Bild setP an.
	
	

quader :: [ ( (Int,Int) , Pixel ) ] -> (Int, Int) -> Int -> Int ->[ ( (Int,Int) , Pixel ) ]
quader xs (x,y) 0 wide = xs 
quader xs (x,y) height 0 = setColumn (x,y) height 0
quader xs (x,y) height wide = 
-- quader xs (x,y) height wide = setColumn (x,y) height wide
-}
































{-
-- Wird nicht gebraucht 
-- plus
-- Nimmt zwei Pixel und addiert di:rae einzelnen Farbkomponenten miteinander
-- Problem ist z.b 255 + 255 = 254
plus_p :: Pixel -> Pixel -> Pixel
plus_p (Pixel ra ga ba) (Pixel rb gb bb) = Pixel (ra + rb) (ga + gb) (ba + bb)


-- max_p nimmt zwei Pixel und erstellt einen neuen Pixel, 
-- wobei komponentenweise die größere Zahl genommen wird.
max_p :: Pixel -> Pixel -> Pixel 
max_p (Pixel ra ga ba) (Pixel rb gb bb) = Pixel (max ra rb) (max ga gb) (max ba bb)
-}







{-


-- Erstellt ein blaues rechtwinkliges Dreieck, dessen rechter Winkel links ist.
-- Benötigt eine Startkoordinate, eine Höhe, und Breite.
-- Wobei der Ausgabewert noch nicht eine Listframe ist (und auch nicht funktioniert).
dreieck :: (Int, Int) -> (Int, Int) -> Int -> Int -> [[Pixel]]
dreieck (xdim, ydim) (xStart, yStart) height wide 
 = map (\x -> map (\y -> if elem y [yStart .. min (f x) (ydim - 1)] then blue_p else white_p) [0 .. ydim -1]) [0 .. xdim -1]
 where f x = round $ (toRational height) / (toRational wide) * toRational ( -x + height + yStart + xStart)
-}

{-
funcMix :: (Int -> Int -> Pixel -> Pixel) -> (Int -> Int -> Pixel -> Pixel) -> (Int -> Int -> Pixel -> Pixel)

-- funcMix kann mit 2 charakteristischen Funktionen umgehen, und gibt eine Funktion 
-- zurück, die eine gewisse Kombination der beiden Funktionen ist.
-- Dabei wird überprüft, ob die zweite Funktion bei einem Pixel bereits eine Farbe hat (!= schwarz)
-- Wenn nicht, wird die Farbe der ersten Funktion eingesetzt.
-- Ziel soll sein, dass das zweite Bild über dem ersten liegt.

funcMix f g x y c | g x y c /= black_p = g x y c
                  | otherwise          = f x y c
-}


