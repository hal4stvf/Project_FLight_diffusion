module Main where
import Network.MateLight.Simple
import Data.Word
import Data.Maybe
import qualified Network.Socket as Sock

--------------------------------------------------------------------------
--
--
--
move :: (Int, Int) -> String -> MyState -> MyState
move (xdim, ydim) "\"j\"" myState 
  = myState {curpos = (\ (x,y) -> (x, (y + 1) `mod` ydim)) (curpos myState)}
move (xdim, ydim) "\"k\"" myState 
  = myState {curpos = (\ (x,y) -> (x, (y - 1) `mod` ydim)) (curpos myState)}
move (xdim, ydim) "\"h\"" myState 
  = myState {curpos = (\ (x,y) -> ((x - 1) `mod` xdim, y)) (curpos myState)}
move (xdim, ydim) "\"l\"" myState 
  = myState {curpos = (\ (x,y) -> ((x + 1) `mod` xdim, y)) (curpos myState)}
move _ _ x = x

--------------------------------------------------------------------------
-- input: alle Status (also aktuelles Curser-Pixel und Farbe)
-- erhöht den Color-Status um 1, darf nur aufgerufen werden, wenn c gedrückt wurde
-- output: alle Status (also aktuelles Curser-Pixel und Farbe)

changeColor :: MyState -> MyState
changeColor myState = myState {curcolcos = ((curcolcos myState) + 1) `mod` 4} 

--------------------------------------------------------------------------
-- aktueller Stand: bekommt dim, Curser-Status und level
-- färbt Pixel entsprechend der Farbe, den Rest schwarz
-- gibt ein Pixelfeld zurück (Listframe)

toFrame :: (Int, Int) -> MyState -> ListFrame
toFrame (xdim, ydim) myState 
 = ListFrame $ con2frame (topicture (levels myState !! curlevel myState))
  where 
    topicture [] = []
    topicture (((x,y), pixelcol): xs)
      | curpos myState == (x,y)    = ((x,y),colors !! curcolcos myState) : (topicture xs) 
      | otherwise                   = ((x,y), pixelcol) : (topicture xs)  
--    con2frame xs = [[c | ((x,y), c) <- xs, x < xdim, y == x2] | ((x1,x2), d) <- xs, x2 < ydim ]
    con2frame [] = []
    con2frame xs = [[ c | ((x,y), c) <- (take (xdim -1) xs) ]] ++ con2frame (drop (xdim -1) xs) 

-- = ListFrame $
--  map (\y -> map (\x -> if x == x' && y == y'
--    then Pixel ((colors !! (col `mod` 4)) !! 0) ((colors !! (col `mod` 4)) !! 1) ((colors !! (col `mod` 4)) !! 2)
--    else (if any (==(x,y)) (level !! 0) then Pixel 0xff 0 0
--      else (if any (==(x,y)) (level !! 1) then Pixel 0 0xff 0
--        else Pixel 0 0 0xff))) [0 .. xdim - 1]) [0 .. ydim - 1]

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

eventTest :: [Event String] -> MyState -> (ListFrame, MyState)
eventTest events myState = (toFrame dim (helper events myState), (helper events myState))
  where 
  helper [] x = x
  helper ((Event mod ev):events) myState 
    | mod == "KEYBOARD" && ev == "\"c\""      = helper events (changeColor myState) 
    | mod == "KEYBOARD" && ev == "\"p\""      = helper events (id myState)
    | mod == "KEYBOARD"                       = helper events (move dim ev myState )
    | otherwise                               = helper events (id myState)
        
--    pixel' = foldl 
--     (\(acc,c, xs) (Event mod ev) -> if mod == "KEYBOARD" then 
--       (if ev == "\"c\"" then changeColor (acc,c,xs) else move dim ev (acc,c,xs)) 
--      else (acc,c,xs)) (pixel, color, xs) events
    

--level = [[(x,y)| y <- [0..11], x <- [0..y]++[29-y..29]],[(x,y)| x <- [8..21], y <- [8..12]],[(x,y)| x <- [22..29], y <- [0..11]]]
--------------------------------------------------------------------------

--------------------------------------------------------------------------
-- globale, statische Variablen:
--
data MyState = MyState {
   curpos    :: (Int, Int)  
  ,curcolcos :: Int
  ,gameplay  :: [((Int,Int),Pixel)]
  ,levels    :: [[((Int,Int),Pixel)]]
  ,curlevel  :: Int
} deriving (Eq, Ord, Show, Read)

-- Farben
colors :: [Pixel]
colors = [Pixel 0xff 0 0, Pixel 0 0xff 0, Pixel 0 0 0xff, Pixel 0xff 0xff 0xff]

-- Feldgröße
dim :: (Int, Int)
dim = (12, 30)
--dim = (30, 12) [TODO]

--levels
level1 = [((x,y), Pixel 0 0 255) | x <- [0 .. (fst dim) - 1], y <- [0 .. (snd dim) - 1]]

-- Anfangsstatus
anStatus = MyState (0, 0) 3 [((0,0),colors !! 3)] [level1] 0
--------------------------------------------------------------------------

main :: IO ()
main = Sock.withSocketsDo $ runMate (Config (fromJust $ parseAddress "134.28.70.172") 1337 dim (Just 500000) False []) eventTest anStatus

