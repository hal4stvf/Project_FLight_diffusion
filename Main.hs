module Main where
import Network.MateLight.Simple
import Data.Word
import Data.Maybe
import Control.Concurrent
import qualified Network.Socket as Sock

import PixelHandling

--------------------------------------------------------------------------
-- Tut was move tun sollte.
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

--------------------------------------------------------------------------------
-- input: alle Status
-- erhöht den Color-Status um 1; Aufruf, wenn c gedrückt wurde
-- output: alle Status (also aktuelles Curser-Pixel und Farbe)

changeColor :: MyState -> MyState
changeColor myState = myState {curcolcos = ((curcolcos myState) + 1) `mod` 4}

--------------------------------------------------------------------------------
-- erste Idee für die Diffusion
--

diffusion :: MyState -> MyState
diffusion myState = myState {curlevelstatus = actualldiffusion $ curlevelstatus myState}
  where
    actualldiffusion xs = [ ((x,y), colors !! curcolcos myState) | ((x,y), c) <- xs ]

diffQ :: [(Int,Int)]
diffQ = [(-1,0),(0,-1),(1,0),(0,1)]    

diffR :: [(Int,Int)]
diffR = drop 4 $ diffMatrix1 diffQ

mat :: [(Int,Int)]
mat = diffQ ++ diffR

diffofpoint ::  [(Int,Int)] -> (Int, Int) -> [(Int,Int)]
diffofpoint = 
  (\ ys x -> [x $+ y | y <- ys, not $ any ((x $+ y) ==) ((0,0):ys++mat), sct (x $+ y) ]) 
 where
  ($+) (x1,y1) (x2,y2) = (x1+x2,y1+y2)
  sct :: (Int, Int) -> Bool
  sct (x,y) | abs (2*x) > fst dim || abs (2*y) > snd dim    = False
            | otherwise                     = True

diffMat ms = makeuniq (mat ++ foldr (++) [] (mapQ ms ++ mapR ms))
  where
    mapQ = map (diffofpoint diffQ) 
    mapR = map (diffofpoint diffR) 
    makeuniq [] = []
    makeuniq (x:xs) 
      | sum [if x==y then 1 else 0 | y <-xs] /= 0  = x : makeuniq [z | z <-xs, x /= z]
      | otherwise                                  = x : makeuniq xs 

diffMatRek ms = iterate diffMat ms !! 15 

---- Fehlerhafte zu manueller Versuch
diffMatrix1 xs = xs ++ helper (xs ++ take 1 xs)  
  where
  helper xs | length xs < 2 = [] 
  helper xs = add2ListDup xs : (helper $ tail xs)
  add2ListDup ys = (\ ((x1,y1):(x2,y2):xs) -> (x1+x2,y1+y2)) ys
diffMatrix2 xs = [(x+x,y+y) | (x,y) <- xs]
diffMatrix3 xs = (take 4 xs) +++  (drop 4 xs)  

diffMatrix xs = diffMatrix1 xs ++ diffMatrix2 xs

(+++) _ [] = []
(+++) [] _ = []
(+++) xs ys = (\ ((x1,y1):qs) ((x2,y2):ws) -> (x1+x2,y1+y2): (qs +++ ws)) xs ys   
--------------------------------------------------------------------------------
-- bekommt dim und Status
-- färbt Cursor entsprechend der Cursor-Farbe
-- rest bleibt momentan noch gleich
-- gibt ein Pixelfeld zurück (Listframe)

toFrame :: (Int, Int) -> MyState -> ListFrame
toFrame (xdim, ydim) myState
 = ListFrame $ con2frame (topicture (levels myState !! curlevel myState))
  where
    topicture [] = []
    topicture (((x,y), pixelcol): xs)
      | curpos myState == (x,y)    = ((x,y),colors !! curcolcos myState) : (topicture xs)
      | otherwise                   = ((x,y), pixelcol) : (topicture xs)
    con2frame [] = []
    con2frame xs = [[ c | ((x,y), c) <- (take (xdim -1) xs) ]] ++ con2frame (drop (xdim -1) xs)

--------------------------------------------------------------------------------
-- bekommt eine Liste von Events (siehe Simple.hs) und einen Status myState
-- helper wendet alle bekommenen Events nach und nach auf Status an
--   wenn etwas eingelesen wird, setzt runMateM das Event "KEYBOARD"
--   Funktion checkt, ob das aktuell ausgelesene Event ein KEYBOARD-Event ist
--     falls ein c eingelesen wurde, rufe changeColor auf
--     falls ein ?? ...
--     ansonsten: rufe move auf
--   ansonsten: Status bleibt gleich
-- gibt Tupel aus ListFrame und Status zurück

eventTest :: [Event String] -> MyState -> (ListFrame, MyState)
eventTest events myState = (toFrame dim (helper events myState), (helper events myState))
  where
  helper [] x = x
  helper ((Event mod ev):events) myState
    | mod == "KEYBOARD" && ev == "\"c\""      = helper events (changeColor myState)
    | mod == "KEYBOARD" && ev == "\"p\""      = helper events (diffusion myState)
    | mod == "KEYBOARD"                       = helper events (move dim ev myState )
    | otherwise                               = helper events (id myState)

--  blink myState
--    | curcolcos myState == blinking myState   = myState
--    | curcolcos myState == 3                  = myState {curcolcos = blinking myState, blinking = 3}
--    | otherwise                               = myState {curcolcos = 3, blinking = curcolcos myState}

--level = [[(x,y)| y <- [0..11], x <- [0..y]++[29-y..29]],[(x,y)| x <- [8..21], y <- [8..12]],[(x,y)| x <- [22..29], y <- [0..11]]]
--------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- globale, statische Variablen:
--
data MyState = MyState {
   curpos    :: (Int, Int)
  ,curcolcos :: Int
  ,gameplay  :: [((Int,Int),Pixel)]
  ,curlevelstatus :: [((Int,Int), Pixel)]
  ,levels    :: [[((Int,Int),Pixel)]]
  ,curlevel  :: Int
  ,blinking  :: Int
} deriving (Eq, Ord, Show, Read)

-- Farben
colors :: [Pixel]
colors = [red_p, green_p, blue_p, white_p]
--colors = [Pixel 0xff 0 0, Pixel 0 0xff 0, Pixel 0 0 0xff, Pixel 0xff 0xff 0xff]

-- Feldgröße
dim :: (Int, Int)
dim = (30, 12)

--levels
-- alles blau
level1 = [((x,y), Pixel 0 0 255) | y <- [0 .. (snd dim) - 1], x <- [0 .. (fst dim) - 1]]
--linke Seite grün
level2 = frame1 ch_left_side green_p
-- linke Seite grün recht Seite rot.
level3 = frameMix (frame1 ch_left_side green_p) (frame1 ch_right_side red_p)
-- links oben grün, rechts oben blau, links unten rot, unten rechts gelb.
level4 = adv_frameMix hlevel4

hlevel4 = [frame1 ch_top_left green_p, frame1 ch_top_right blue_p,
 frame1 ch_bot_left red_p, frame1 ch_bot_right yellow_p]


-- Anfangsstatus
anStatus = MyState (0, 0) 3 [((0,0),colors !! 3)] level4 [level4] 0 3
--------------------------------------------------------------------------

main :: IO ()
main = Sock.withSocketsDo $ runMate (Config (fromJust $ parseAddress "134.28.70.172") 1337 dim (Just 500000) False []) eventTest anStatus
