module PixelHandling where
import Network.MateLight.Simple

{-
data MyState = MyState {
   curpos    :: (Int, Int)
  ,curcolcos :: Int
  ,gameplay  :: [((Int,Int),Pixel)]
  ,levels    :: [[((Int,Int),Pixel)]]
  ,curlevel  :: Int
  ,blinking  :: Int
} deriving (Eq, Ord, Show, Read)
-}
 
--Beispielbild 
ex_fr :: [( (Int,Int) , Pixel) ]
ex_fr = (take 2  $ frame1 ch_left_side green_p) ++ (drop 358 $ frame1 ch_left_side green_p)

black_pic :: [ ( (Int,Int) , Pixel) ]
black_pic = [ ((x,y), black_p) | y <- [0 .. ydim - 1] , x <- [0 .. xdim - 1] ]
-- Funktionen, um besser mit Pixeln/Bildern umgehen zu können.

--Variablen
-- Wird hier gebraucht, denn sonst sind die Funktionenköpfe zu groß...
-- TODO: Beheben
dim' :: (Int,Int)
dim' = (30,12)
xdim,ydim :: Int
xdim = fst dim'
ydim = snd dim'


-- plus
-- Nimmt zwei Pixel und addiert di:rae einzelnen Farbkomponenten miteinander
-- Problem ist z.b 255 + 255 = 254
plus_p :: Pixel -> Pixel -> Pixel
--plus a b = Pixel (pixR a + pixR b) (pixG a + pixG b) (pixB a + pixB b)
plus_p (Pixel ra ga ba) (Pixel rb gb bb) = Pixel (ra + rb) (ga + gb) (ba + bb)


-- max_p nimmt zwei Pixel und erstellt einen neuen Pixel, 
-- wobei komponentenweise die größere Zahl genommen wird.
max_p :: Pixel -> Pixel -> Pixel 
max_p (Pixel ra ga ba) (Pixel rb gb bb) = Pixel (max ra rb) (max ga gb) (max ba bb)


-- Erstellt ein blaues rechtwinkliges Dreieck, dessen rechter Winkel links ist.
-- Benötigt eine Startkoordinate, eine Höhe, und Breite.
-- Wobei der Ausgabewert noch nicht eine Listframe ist (und auch nicht funktioniert).
dreieck :: (Int, Int) -> (Int, Int) -> Int -> Int -> [[Pixel]]
dreieck (xdim, ydim) (xStart, yStart) height wide 
 = map (\x -> map (\y -> if elem y [yStart .. min (f x) (ydim - 1)] then blue_p else white_p) [0 .. ydim -1]) [0 .. xdim -1]
 where f x = round $ (toRational height) / (toRational wide) * toRational ( -x + height + yStart + xStart)


 
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



-- Grundfiguren




frameMix :: [( (Int,Int) , Pixel)] -> [( (Int,Int) , Pixel)] -> [( (Int,Int) , Pixel)]
-- frameMix kann mit 2 Bildern umgehen.
-- Es soll ein Bild entstehen, bei dem das erste Bild über dem zweiten liegt.
frameMix xs ys = [ ( (x,y) , if c1 /= black_p then c1 else c2 ) | ( (x,y) , c1, c2) <- helper]
 where helper = [ ((x,y), d, e) | ( ( (x,y) , d ) , ( (x',y') , e) ) <- zip xs ys ]


-- Das gleiche wie frameMix, nur nimmt es eine Liste von Bildern.
-- Dabei überdeckt das erste Bild der Liste den Rest, darunter ist das zweite Bild usw.
adv_frameMix :: [ [( (Int,Int) , Pixel)] ] -> [( (Int,Int) , Pixel)]
adv_frameMix xs = foldl frameMix black_pic xs


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
ch_top_left x y c = if x <= div xdim 2 && y <= div ydim 2 then c else black_p

-- Unten links ist Pixelfarbe
ch_bot_left :: Int -> Int -> Pixel -> Pixel
ch_bot_left x y c = if x <= div xdim 2 && y >= div ydim 2 then c else black_p

-- Oben rechts ist Pixelfarbe
ch_top_right :: Int -> Int -> Pixel -> Pixel
ch_top_right x y c = if x >= div xdim 2 && y <= div ydim 2 then c else black_p

-- Unten rechts ist Pixelfarbe
ch_bot_right :: Int -> Int -> Pixel -> Pixel
ch_bot_right x y c = if x >= div xdim 2 && y >= div ydim 2 then c else black_p



--level3 = frame1 funcMix ch_left_side ch_right_side green_p






