module PixelHandling where
import Network.MateLight.Simple

-- Funktionen, um besser mit Pixeln/Bildern umgehen zu können.

-- plus
-- Nimmt zwei Pixel und addiert die einzelnen Farbkomponenten miteinander
-- Problem ist z.b 255 + 255 = 254
plus_p :: Pixel -> Pixel -> Pixel
--plus a b = Pixel (pixR a + pixR b) (pixG a + pixG b) (pixB a + pixB b)
plus_p (Pixel ra ga ba) (Pixel rb gb bb) = Pixel (ra + rb) (ga + gb) (ba + bb)


-- max_p nimmt zwei Pixel und erstellt einen neuen Pixel, 
-- wobei komponentenweise die größere Zahl genommen wird.
max_p :: Pixel -> Pixel -> Pixel 
max_p (Pixel ra ga ba) (Pixel rb gb bb) = Pixel (max ra rb) (max ga gb) (max ba bb)



-- Figuren

frame1 :: (Int,Int) -> [[Pixel]] -- Linke Seite grün, rechte Seite rot
frame1 (xdim,ydim) = map (\y -> map (\x -> if x <= xdim `div` 2 then green_p else red_p) [0 .. xdim - 1]) [0.. ydim - 1]

-- Erstellt ein blaues rechtwinkliges Dreieck, dessen rechter Winkel links ist.
-- Benötigt eine Startkoordinate, eine Höhe, und Breite.
-- Wobei der Ausgabewert noch nicht eine Listframe ist.
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

white_p :: Pixel
white_p = Pixel 255 255 255

black_p :: Pixel
black_p = Pixel 0 0 0
