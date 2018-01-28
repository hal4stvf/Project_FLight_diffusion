module Daten where

import Network.MateLight.Simple 


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

