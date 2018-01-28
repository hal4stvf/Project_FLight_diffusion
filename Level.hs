module Level where

import PixelHandling
import Network.MateLight.Simple


-- alles blau
level1 = [((x,y), blue_p) | y <- [0 .. (snd dim) - 1], x <- [0 .. (fst dim) - 1]]

-- linke Seite grün rechte Seite rot.
level2 = frameMix (frame1 ch_left_side green_p) (frame1 ch_right_side red_p)

-- links oben grün, rechts oben blau, links unten rot, unten rechts gelb.
level3 = adv_frameMix ch_level3
 where
 ch_level3 = [frame1 ch_top_left green_p, 
             frame1 ch_top_right blue_p,
             frame1 ch_bot_left red_p, 
             frame1 ch_bot_right yellow_p]

-- Level 3 nur mit einem weißen Rechteck irgendwo in der Mitte
level4 = setRectangle level3 white_p (4,4) (24,9)

-- Wie Level 5. Nur gibt es einen weiteren grünen Pixel an der Position (20,2)
level5 = setRectangle pic1 green_p (15,6) (17,8)
 where 
 pic1 = setRectangle pic2 blue_p (12,5) (19,8)
 pic2 = setRectangle pic3 red_p (11,4) (20,9)
 pic3 = setRectangle pic4 green_p (9,2) (23,11)
 pic4 = setRectangle black_pic blue_p (0,0) (30,12)

-- Gelber Hintergrund. Blauer Kasten drinnen. und eine grüne Kolumne irgendwo drinnen.
level6 = setColumn pic1 blue_p 4 (26,6)
 where 
 pic1 = sethColumn pic2 red_p 4 (3,2)
 pic2 = setRectangle pic3 blue_p (2,0) (26,11)
 pic3 = setRectangle black_pic yellow_p (0,0) (30,12)

-- Irgendetwas
level7 = setRectangle pic1 yellow_p (6,2) (8,4)
 where
 pic1 = setRectangle pic2 yellow_p (12,2) (14,4)
 pic2 = setRectangle pic3 yellow_p (9,4) (11,6)
 pic3 = setRectangle pic4 yellow_p (6,6) (6,10)
 pic4 = setRectangle pic5 yellow_p (6,8) (14,10)
 pic5 = setRectangle black_pic yellow_p (12,8) (14,10)
 
 
-- Theoretisch soll da ein WON stehen.
level8 = setRectangle pic1 white_p (4,1) (5,8)
 where 
 pic1 = setRectangle pic2 black_p (3,1) (6,8)
 pic2 = setRectangle pic3 white_p (2,1) (7,9)
 pic3 = setRectangle pic4 black_p (12,3) (14,7)
 pic4 = setRectangle pic5 white_p (10,1) (16,9)
 pic5 = setRectangle pic6 white_p (19,1) (21,9)
 pic6 = setRectangle pic7 white_p (21,1) (22,6)
 pic7 = setRectangle pic8 white_p (22,2) (23,7)
 pic8 = setRectangle pic9 white_p (23,3) (23,9)
 pic9 = setRectangle black_pic white_p (24,1) (26,9)
 

level_list = [level1, level2, level3, level4, level5, level6, level7, level8]


{-
getDivByAny :: [Int] -> [Int] -> [Int]
getDivByAny xs ys = filter helper xs
 where helper x = any (\y -> mod x y == 0) ys
 
getDivByAny2 :: [Int] -> [Int] -> [Int]
getDivByAny2 xs ys = filter (\x -> any (\y -> mod x y == 0) ys ) xs

mytest xs ys = getDivByAny xs ys == getDivByAny2 xs ys
-}