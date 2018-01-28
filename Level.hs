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

-- Ein roter Pixel an der Position (12,12)
level5 = setP black_pic (12,12) red_p

-- Wie Level 5. Nur gibt es einen weiteren grünen Pixel an der Position (20,2)
level6 = setP level5 (20,2) green_p

-- Ein roter Balken für x = 3, y = 2 mit der Höhe 4.
-- Dazu weiter rechts eine ganze blaue Kolumne
level7 = setColumn pic1 blue_p 4 (26,6)
 where pic1 = sethColumn black_pic red_p 4 (3,2)

-- Ein halber grüner Balken nach unten
level8 = sethColumn black_pic green_p (-4) (10,10)

-- Ein blaues Rechteck mit (2,2) als Startwert und (10,11) als Endwert
level9 = setRectangle black_pic blue_p (2,2) (10,11)

-- Ein blaues Rechteck mit (10,11) als Starkoordinate und (2,2) als Endkoordinate
level10 = setRectangle black_pic blue_p (10,11) (2,2)

-- Ein blaues Rechteck mit (2,2) als Startwert, einer Breite von 8 und einer Höhe von 9
level11 = setRectangle_nadv black_pic blue_p 9 8 (2,2)

-- Ein blaues Recht mit (10,11) als Startwert, einer Breite von -8 und einer Höhe von -9
level12 = setRectangle_nadv black_pic blue_p (-9) (-8) (10,11)

-- Eine blaue Kolumne mit (5,5) als Start und Höhe 3
level13 = setRectangle_nadv black_pic red_p 3 1 (5,5)

-- Eine blaue Kolumne mit (5,8) als Start und Höhe -3
level14 = setRectangle_nadv black_pic red_p (-3) (1) (5,8)

level_list = [level1, level2, level3, level4, level5, level6, level7, level8, level9, level10, level11, level12, level13, level14]


{-
getDivByAny :: [Int] -> [Int] -> [Int]
getDivByAny xs ys = filter helper xs
 where helper x = any (\y -> mod x y == 0) ys
 
getDivByAny2 :: [Int] -> [Int] -> [Int]
getDivByAny2 xs ys = filter (\x -> any (\y -> mod x y == 0) ys ) xs

mytest xs ys = getDivByAny xs ys == getDivByAny2 xs ys
-}