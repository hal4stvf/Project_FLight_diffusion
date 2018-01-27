module Daten where

import Network.MateLight.Simple 
import PixelHandling
--import Diffusion


--------------------------------------------------------------------------------
-- [Purpose]
---------------------------------------------------
-- globale, statische Variablen:
---------------------------------------------------

-- Zustand des Feldes der übergeben werden soll
data MyState = MyState {
   curpos    :: (Int, Int)
  ,curcolcos :: Int
  ,gameplay  :: [((Int,Int),Pixel)]
  ,diffusionstatus :: [[(Int,Int)]]
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
anStatus = MyState (9, 9) 3 [] [] [level4] 0 3
-- Teststatus
--anStatus1 = MyState (9, 9) 3 [((9,9), Pixel 255 255 255)] (diffusionstatus $ diffusion anStatus) [level4] 0 3

---------------------------------------------------
--------------------------------------------------------------------------------
