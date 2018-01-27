module Diffusion where
import Daten 
import Network.MateLight.Simple
import PixelHandling

--------------------------------------------------------------------------------
-- [Purpose]
---------------------------------------------------

--------------------------------------------------------------------------------
-- erste Idee für die Diffusion (siehe auch commit)
-- Diese Zeilen produzieren wesentlich einen diskreten 
-- "Diffusionoperator"
--


diffQ :: [(Int,Int)]
diffQ = [(-1,0),(0,-1),(1,0),(0,1)]    

diffMatrix1 xs = xs ++ helper (xs ++ take 1 xs)  
  where
  helper xs | length xs < 2 = [] 
  helper xs = add2ListDup xs : (helper $ tail xs)
  add2ListDup ys = (\ ((x1,y1):(x2,y2):xs) -> (x1+x2,y1+y2)) ys

diffR :: [(Int,Int)]
diffR = drop 4 $ diffMatrix1 diffQ

mat :: [(Int,Int)]
mat = diffQ ++ diffR

diffofpoint ::  [(Int,Int)] -> (Int, Int) -> [(Int,Int)]
diffofpoint = 
  (\ ys x -> [x $+ y | y <- ys, not $ any ((x $+ y) ==) ((0,0):ys++mat), sct (x $+ y) ]) 
 where
  sct :: (Int, Int) -> Bool
  sct (x,y) | abs (x) > fst dim || abs (y) > snd dim    = False
            | otherwise                                 = True

($+) (x1,y1) (x2,y2) = (x1+x2,y1+y2)

diffMat ms = makeuniq (((0,0):mat) ++ foldr (++) [] (mapQ ms ++ mapR ms))
  where
    mapQ = map (diffofpoint diffQ) 
    mapR = map (diffofpoint diffR) 
    makeuniq [] = []
    makeuniq (x:xs) 
      | sum [if x==y then 1 else 0 | y <-xs] /= 0  = x : makeuniq [z | z <-xs, x /= z]
      | otherwise                                  = x : makeuniq xs 

diffMatRek ms n = iterate diffMat ms !! n 

diffOP  = diffMatRek mat 6
diffOPvar n = diffMatRek mat n 

diffOperator :: (Int,Int) -> Pixel -> [((Int,Int),Pixel)] -> [[(Int,Int)]]
diffOperator point color picture = h $ g diffOP
  where
    g [] = []
    g (x:xs) 
      | not $ null [1 | (p,c) <- picture, p == (x $+ point), c /= color] 
                  = (x $+ point)  : g xs  
      | otherwise = g xs 
    h [] = []
    h ws = (take 4 ws) : (h $ drop 4 ws)

oneStepDiffusion :: MyState -> MyState
oneStepDiffusion myState = myState {diffusionstatus = tail $ diffusionstatus myState
  ,levels = [if (curlevel myState) == i then f x else  x | (x,i) <- zip (levels myState) [0..3]]}
  where 
      f = h diffstat   
      h [] x = x
      h (d:ds) s = h ds (l d s)
      l d = map (\(p,c) -> if (p /= d) then (p,c) else (p,newc)) 
      diffstat = head $ diffusionstatus myState
      newc = snd $ head $ gameplay myState

---- Fehlerhafte zu manueller Versuch
--diffMatrix2 xs = [(x+x,y+y) | (x,y) <- xs]
--diffMatrix3 xs = (take 4 xs) +++  (drop 4 xs)  
--
--diffMatrix xs = diffMatrix1 xs ++ diffMatrix2 xs
--
--(+++) _ [] = []
--(+++) [] _ = []
--(+++) xs ys = (\ ((x1,y1):qs) ((x2,y2):ws) -> (x1+x2,y1+y2): (qs +++ ws)) xs ys   
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Hier geht es jetzt um Diffusion
-- Am Anfang wird erstmal nur das Level als solches überschrieben
diffusion :: MyState -> MyState
diffusion myState = myState {
              gameplay        = (curpos myState, colors !! curcolcos myState) : gameplay myState 
             ,diffusionstatus = diffOperator 
                (curpos myState) 
                (colors !! curcolcos myState)   
                (levels myState !! curlevel myState)
         }

---------------------------------------------------
--------------------------------------------------------------------------------
