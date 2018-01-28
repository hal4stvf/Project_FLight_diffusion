module Main where
import Network.MateLight.Simple
import Data.Word
import Data.Maybe
import Control.Concurrent
import qualified Network.Socket as Sock

import Daten
import PixelHandling
import Diffusion

{------------------------------------------------------------------------------}
--------------------------------------------------
-- [Purpose]
-- Tut was move tun sollte.
--------------------------------------------------
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
--------------------------------------------------
--------------------------------------------------------------------------

{------------------------------------------------------------------------------}
-- [Purpose]
---------------------------------------------------
-- input: alle Status
-- erhöht den Color-Status um 1; Aufruf, wenn c gedrückt wurde
-- output: alle Status (also aktuelles Curser-Pixel und Farbe)
---------------------------------------------------

changeColor :: MyState -> MyState
changeColor myState = myState {curcolcos = ((curcolcos myState) + 1) `mod` 4}

---------------------------------------------------
--------------------------------------------------------------------------------


{------------------------------------------------------------------------------}
-- [Purpose]
---------------------------------------------------

------------------------------
-- Another Version.					 |
-- Remove in working version |
------------------------------

--  blink myState
--    | curcolcos myState == blinking myState   = myState
--    | curcolcos myState == 3                  = myState {curcolcos = blinking myState, blinking = 3}
--    | otherwise                               = myState {curcolcos = 3, blinking = curcolcos myState}
--
--------------------------------------------------

---------------------------------------------------
--------------------------------------------------------------------------------

{------------------------------------------------------------------------------}
-- |||| toFrame ||||
---------------------------------------------------
-- [Purpose]
---------------------------------------------------
-- bekommt dim und Status
-- färbt Cursor entsprechend der Cursor-Farbe
-- rest bleibt momentan noch gleich
-- gibt ein Pixelfeld zurück (Listframe)
---------------------------------------------------

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

---------------------------------------------------
--------------------------------------------------------------------------------

{------------------------------------------------------------------------------}
-- [Purpose]
---------------------------------------------------
-- bekommt eine Liste von Events (siehe Simple.hs) und einen Status myState
-- helper wendet alle bekommenen Events nach und nach auf Status an
--   wenn etwas eingelesen wird, setzt runMateM das Event "KEYBOARD"
--   Funktion checkt, ob das aktuell ausgelesene Event ein KEYBOARD-Event ist
--     falls ein c eingelesen wurde, rufe changeColor auf
--     falls ein ?? ...
--     ansonsten: rufe move auf
--   ansonsten: Status bleibt gleich
-- gibt Tupel aus ListFrame und Status zurück
---------------------------------------------------

------------------------------
-- Another Version.					 |
-- Remove in working version |
------------------------------

--eventTest :: [Int] -> [Event String] -> MyState -> (ListFrame, MyState)
--eventTest ints events myState = (toFrame dim (helper ints events myState), (helper ints events myState))
--
--------------------------------------------------

eventTest :: [Event String] -> MyState -> (ListFrame, MyState)
eventTest events myState = (toFrame dim (helper events myState), (helper events myState))
  where
  helper [] x | null $ diffusionstatus myState  = x
              | otherwise                       = oneStepDiffusion x  
  helper ((Event mod ev):events) myState
    | mod == "KEYBOARD" && ev == "\"c\""      = helper events (changeColor myState)
    | mod == "KEYBOARD" && ev == "\"p\""      = helper events (diffusion myState)
    | mod == "KEYBOARD"                       = helper events (move dim ev myState )
    | otherwise                               = helper events (id myState)

---------------------------------------------------
--------------------------------------------------------------------------------

{------------------------------------------------------------------------------}
-- |||| main |||| 
---------------------------------------------------
-- [Purpose]
---------------------------------------------------
main :: IO ()
main = Sock.withSocketsDo $ runMate (Config (fromJust $ parseAddress "127.0.0.1") 1337 dim (Just 90000) False []) eventTest anStatus

------------------------------
-- Another Version.					 |
-- Remove in working version |
------------------------------

--main = Sock.withSocketsDo $ runMate (Config (fromJust $ parseAddress "134.28.70.172") 1337 dim (Just 500000) False []) eventTest anStatus
--main = Sock.withSocketsDo $ runMateRandom (Config (fromJust $ parseAddress "127.0.0.1") 1337 dim (Just 500000) False []) eventTest anStatus
--
--------------------------------------------------------------------------------
