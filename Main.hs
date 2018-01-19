module Main where
import Network.MateLight.Simple
import Data.Word
import Data.Maybe
import qualified Network.Socket as Sock

--------------------------------------------------------------------------
--
--
--

move :: (Int, Int) -> String -> (Int, Int) -> (Int, Int)
move (xdim, ydim) "\"j\"" (x, y) = (x, (y + 1) `mod` ydim)
move (xdim, ydim) "\"k\"" (x, y) = (x, (y - 1) `mod` ydim)
move (xdim, ydim) "\"h\"" (x, y) = ((x - 1) `mod` xdim, y)
move (xdim, ydim) "\"l\"" (x, y) = ((x + 1) `mod` xdim, y)
move _ _ x = x

--getcolor :: [Int] -> ([Int],Int)
--getcolor x:xs = (xs,x)

--------------------------------------------------------------------------
-- aktueller Stand: bekommt Feldgröße, Pixel und Farbe
-- färbt Pixel entsprechend der Farbe, den Rest schwarz
-- gibt ein Pixelfeld zurück (Listframe)

toFrame :: (Int, Int) -> (Int, Int) -> Int -> ListFrame
toFrame (xdim, ydim) (x', y') col
 = ListFrame $
  map (\y -> map (\x -> if x == x' && y == y' then Pixel ((colors !! (col `mod` 4)) !! 0) ((colors !! (col `mod` 4)) !! 1) ((colors !! (col `mod` 4)) !! 2) else Pixel 0 0 0) [0 .. xdim - 1])
  [0 .. ydim - 1]

--------------------------------------------------------------------------
-- bekommt eine Liste von Events (siehe Simple.hs) und ein Pixel
-- setzt
-- gibt ein Tupel aus Pixelfeld und Pixel zurück

eventTest :: [Event String] -> (Int, Int) -> (ListFrame, (Int, Int))
eventTest events pixel = (toFrame dim pixel' 2, helper pixel')
  where
    pixel' = foldl (\acc (Event mod ev) -> if mod == "KEYBOARD" then move dim ev acc else acc) pixel events
    helper = id

--------------------------------------------------------------------------

--------------------------------------------------------------------------
-- globale, statische Variablen:

colors :: [[Word8]]
colors = [[0xff,0,0],[0,0xff,0],[0,0,0xff],[0xff,0xff,0xff]]

dim :: (Int, Int)
dim = (30, 12)

--------------------------------------------------------------------------

main :: IO ()
main = Sock.withSocketsDo $ runMate (Config (fromJust $ parseAddress "134.28.70.172") 1337 dim (Just 500000) False []) eventTest (0, 0)
