module Level where

import PixelHandling
import Network.MateLight.Simple


-- alles blau
level1 = [((x,y), blue_p) | y <- [0 .. (snd dim) - 1], x <- [0 .. (fst dim) - 1]]

-- linke Seite grün
level2 = frame1 ch_left_side green_p

-- linke Seite grün recht Seite rot.
level3 = frameMix (frame1 ch_left_side green_p) (frame1 ch_right_side red_p)

-- links oben grün, rechts oben blau, links unten rot, unten rechts gelb.
level4 = adv_frameMix ch_level4

ch_level4 = [frame1 ch_top_left green_p, 
             frame1 ch_top_right blue_p,
             frame1 ch_bot_left red_p, 
		     frame1 ch_bot_right yellow_p]
--



{-
getDivByAny :: [Int] -> [Int] -> [Int]
getDivByAny xs ys = filter helper xs
 where helper x = any (\y -> mod x y == 0) ys
 
getDivByAny2 :: [Int] -> [Int] -> [Int]
getDivByAny2 xs ys = filter (\x -> any (\y -> mod x y == 0) ys ) xs

mytest xs ys = getDivByAny xs ys == getDivByAny2 xs ys
-}