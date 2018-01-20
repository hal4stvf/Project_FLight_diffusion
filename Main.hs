module Main where
import Network.MateLight.Simple
import Data.Word
import Data.Maybe
import qualified Network.Socket as Sock

--------------------------------------------------------------------------
--
--
--

move :: (Int, Int) -> String -> ((Int, Int), Int) -> ((Int, Int), Int)
move (xdim, ydim) "\"j\"" ((x, y), c) = ((x, (y + 1) `mod` ydim), c)
move (xdim, ydim) "\"k\"" ((x, y), c) = ((x, (y - 1) `mod` ydim), c)
move (xdim, ydim) "\"h\"" ((x, y), c) = (((x - 1) `mod` xdim, y), c)
move (xdim, ydim) "\"l\"" ((x, y), c) = (((x + 1) `mod` xdim, y), c)
move _ _ x = x

--------------------------------------------------------------------------
-- input: alle Status (also aktuelles Curser-Pixel und Farbe)
-- erhöht den Color-Status um 1, darf nur aufgerufen werden, wenn c gedrückt wurde
-- output: alle Status (also aktuelles Curser-Pixel und Farbe)

changeColor :: ((Int, Int), Int) -> ((Int, Int), Int)
changeColor (x,c) = (x, c+1)

--------------------------------------------------------------------------
-- aktueller Stand: bekommt dim, Curser-Status und level
-- färbt Pixel entsprechend der Farbe, den Rest schwarz
-- gibt ein Pixelfeld zurück (Listframe)

toFrame :: (Int, Int) -> ((Int, Int), Int) -> [[(Int,Int)]] -> ListFrame
toFrame (xdim, ydim) ((x', y'), col) level
 = ListFrame $
  map (\y -> map (\x -> if x == x' && y == y'
    then Pixel ((colors !! (col `mod` 4)) !! 0) ((colors !! (col `mod` 4)) !! 1) ((colors !! (col `mod` 4)) !! 2)
    else (if any (==(x,y)) (level !! 0) then Pixel 0xff 0 0
      else (if any (==(x,y)) (level !! 1) then Pixel 0 0xff 0
        else Pixel 0 0 0xff))) [0 .. xdim - 1]) [0 .. ydim - 1]

--------------------------------------------------------------------------
-- bekommt eine Liste von Events (siehe Simple.hs) und einen Status (Pixel, Farbe)
-- pixel' wendet eine Fkt. nach und nach auf das Pixel und alle bekommenen Events an
--   wenn etwas eingelesen wird, setzt runMateM (siehe MateLight.hs) das Event "KEYBOARD" [c] (Z. 86/87);
--   dementsprechend checkt die Funktion, ob das aktuell ausgelesene Event ein KEYBOARD-Event ist
--   falls ja: checke, ob ein c eingelesen wurde
--       falls ja: übergebe changeColor den Status (neuer, farnveränderter Status kommt zurück)
--       falls nein: übergebe move dim, die eingelesene Taste und den Status aufgerufen (neuer, bewegter Status kommt zurück)
--   falls nein: Pixelposition bleibt gleich
-- gibt ein Tupel aus dem entsprechend der Events veränderten Pixelfeld (aufgerufen mit toFrame dim pixel' level) und dem Status zurück

eventTest :: [Event String] -> ((Int, Int), Int) -> (ListFrame, ((Int, Int), Int))
eventTest events (pixel, color) = (toFrame dim pixel' level, helper pixel')
  where
    pixel' = foldl (\(acc,c) (Event mod ev) -> if mod == "KEYBOARD" then (if ev == "\"c\"" then changeColor (acc,c) else move dim ev (acc,c)) else (acc,c)) (pixel, color) events
    level = [[(x,y)| y <- [0..11], x <- [0..y]++[29-y..29]],[(x,y)| x <- [8..21], y <- [8..12]],[(x,y)| x <- [22..29], y <- [0..11]]]
    helper = id
--------------------------------------------------------------------------

--------------------------------------------------------------------------
-- globale, statische Variablen:

-- Farben
colors :: [[Word8]]
colors = [[0xff,0,0],[0,0xff,0],[0,0,0xff],[0xff,0xff,0xff]]

-- Feldgröße
dim :: (Int, Int)
dim = (30, 12)

--------------------------------------------------------------------------

main :: IO ()
main = Sock.withSocketsDo $ runMate (Config (fromJust $ parseAddress "134.28.70.172") 1337 dim (Just 500000) False []) eventTest ((0, 0), 3)
