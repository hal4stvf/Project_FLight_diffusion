module Diffusion where
import Daten 
import Network.MateLight.Simple
import PixelHandling

{------------------------------------------------------------------------------}
{----------------------------- Funktionsname ----------------------------------}
{------------------------------------------------------------------------------}
-- [Purpose]
-- Diese Funktion wird durch die Taste 'p' aufgerufen. Sie berechnet die 
-- Diffusion für den aktuellen Cursorpunkt
-- 
---------------------------------------------------

diffusion :: MyState -> MyState
diffusion myState = myState {
 gameplay        = (curpos myState, colors !! curcolcos myState) : gameplay myState 
,diffusionstatus = diffOperator (curpos myState) 
                   (colors !! curcolcos myState)   
                   (levels myState !! curlevel myState)
         }

---------------------------------------------------
--------------------------------------------------------------------------------

{------------------------------------------------------------------------------}
{----------------------------- Funktionsname ----------------------------------}
{------------------------------------------------------------------------------}
-- [Purpose]
-- Zweite Idee für die Diffusion 
-- Diese Zeilen produzieren wesentlich einen diskreten 
-- (hoffentlich gleich) dynamischen "Diffusionoperator"
---------------------------------------------------

--   1
-- 1   1
--   1 
diffQ :: [(Int,Int)]
diffQ = [(-1,0),(0,-1),(1,0),(0,1)]    

-- 2 1 2
-- 1   1
-- 2 1 2 
diffR :: [(Int,Int)]
diffR = diffQ $* (sigma0 diffQ) 

($*) xs ys = [x $+ y | (x,y) <- zip xs ys]

-- permutate
sigma0 xs = (tail xs ++ head xs:[])
sigmaN xs n = iterate sigma0 xs !! n
sigma xs = sigmaN xs 3

-- Nutzlos?
faltM :: [[(Int,Int)]]
faltM = [diffQ,diffR] 

-- Nutzlos?
faltIt1 :: [[(Int,Int)]]
faltIt1 = [diffQ $* diffQ, diffQ $* diffR, diffQ $* (sigmaN diffR 3), diffR $* diffR]
faltIt2 = [diffQ $* x | x <- faltIt1 ] 
faltIt3 = [(sigmaN diffR 3) $* x | x <- faltIt1, x /= last faltIt1 ] ++ ((diffR $* (last faltIt1) : []))

-- [Purpose] 
-- definiert die Bewegungen
ita f g x y p = iterate g (iterate f p !! x) !! y

ne = ita north east 
nw = ita north west 
sw = ita south west
se = ita south east 

-- hier über muss gecyclt werden
movingList = [ne,nw,sw,se]
movingList2 = [ne,ne,nw,nw,sw,sw,se,se]

-- generierte eine Liste an Duppeln, die es gerade braucht um mit der movingliste
-- eine "Diffusion" zu simmulieren
pointGenerate n = [ f d (i,n) | i <- [0..n], d <- [0..7], not (i == 0 || i ==n) || (d < 4) ]
  where 
    f d (i,n) |not $ odd d = (i,n)
              | otherwise = (n,i)

difmat = [ f n (pointGenerate n) | n <- [0..]]
  where
    f n xs = [take 4 xs] ++ [[x | x <- take 8 (drop (8*i) (drop 4 xs))] | i <- [0..n-1]]

-- eigentlich lösung?
-- [TODO] unbedingt an den Abbruchbedinugen arbeiten, diese sind nur hart implementiert
--
trial k point picture = rekur k
  where  
   rekur k  | k == 3 = []
   rekur k = (foldr (++) [] [ g ys point | ys <- (head $ drop k difmat)]) : (rekur (k + 1))
   g ys point 
      | length ys == 4     
        = [mv n m point | (mv, (n,m)) <- zip movingList ys, (mv n m point) `inPic` picture]
      | otherwise 
        = [mv n m point | (mv, (n,m)) <- zip movingList2 ys, (mv n m point) `inPic` picture]
   inPic p xs = any (p ==) xs 

--------------------------------------------------

  
-- berechnet die nte "faltung" (lazy ;)) 
-- [TODO] Hier fehlt die berücksichtigung, dass DR == RD ist 
-- und mann eigentlich D3(R) schreiben muss. nur für gerades n?
faltIteration xs n =  iterate falt_each xs !! n
  where
    falt_each xs = [f m x | m <- faltM, x <- xs]
    f = ($*)
--    f m x | m == diffQ    = m $* x
--          | m == diffR && x == diffR   = m $* x
--          | odd n               = m $* (sigmaN x 1)
--          | even n              = m $* (sigmaN x 3)
    

-- speziell für faltM
-- [TODO] hier noch die mittleren Listen in einer sinnvolleren Art zusammen-
-- führen. Das heißt auf 2 mal 4*(n-2) große Liste ... nur in richtig(z.B.)
nteFaltIt n = f $ faltIteration faltM n
  where 
    f = id
--    f x  = (head x) : foldr (++) [] (tailnoLast x) : last x :[] 
--    tailnoLast xs = [x | x<- xs, x /= last xs]
{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
{----------------------------- Funktionsname ----------------------------------}
{------------------------------------------------------------------------------}
-- [Purpose]
-- Gibt am Ende all die Pixel aus, die gefärbt werden sollen
---------------------------------------------------

-- [TODO] Hier eine Funktion schreiben, die ausgehend von einem Punkt
-- das Spielfeld abgeht ... 

thesePixel = selectColor 

data SelectColor = SelectColor {
   lastpoint    :: (Int, Int) 
  ,currentpoint :: (Int, Int) 
  ,sellevel     :: [((Int,Int),Pixel)]
  ,startpoint   :: (Int,Int)
  ,color        :: Pixel
  } deriving (Eq, Ord, Show, Read)

-- [TODO] Hier muss wohl die Abbruchbedingung noch einmal überarbeitet werden
--selectColor :: (Int,Int) -> Pixel -> [((Int, Int),Pixel)] -> SelectColor 
selectColor point color level = [p | (p,c) <- sellevel $ rekursion curSelCol]
  where  
    rekursion x | startpoint x == f (currentpoint x) x       = x
    rekursion x = rekursion  x { lastpoint = currentpoint x, currentpoint = f (currentpoint x) x, 
      sellevel = g (sellevel x) x}
    f p x
      | (north p /= p) && ((level $. (north p)) == color) && (h x (north p)) = north p
      | (west  p /= p) && ((level $. (west  p)) == color) && (h x (west  p)) = west  p
      | (south p /= p) && ((level $. (south p)) == color) && (h x (south p)) = south p
      | (east  p /= p) && ((level $. (east  p)) == color) && (h x (east  p)) = east  p
      | otherwise = startpoint curSelCol
    g xs x= (currentpoint x, color) : xs
    h x a = not $ any (\ (p,c) -> p == a) (sellevel x)
    firstedge point | gofurhter point      = firstedge $ north point
                    | otherwise            = point 
    gofurhter point | north point /= point && level $. point == color = True 
                    | otherwise                                       = False
    curSelCol :: SelectColor
    curSelCol = SelectColor (firstedge point) (firstedge point) [] (firstedge point) color

-- Moves zum abgehen des Feldes
--
north (x,y) | y-1 >= 0          = (x,y-1)
            | otherwise         = (x,y)
west  (x,y) | x+1 <= fst dim    = (x+1,y)
            | otherwise         = (x,y)
south (x,y) | y+1 <= snd dim    = (x,y+1)
            | otherwise         = (x,y)
east  (x,y) | x-1 >= 0          = (x-1,y)
            | otherwise         = (x,y)

{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
{----------------------------- Funktionsname ----------------------------------}
{------------------------------------------------------------------------------}
-- [Purpose]
-- Hier muss nun die dynamische Diffusion umgesetzt werden.
-- Dabei müssen auch noch die Randbedingungen eingearbeite werden.
---------------------------------------------------

-- [TODO] -- irgendwie clever nteFaltIt und lazyevaluation nutzen ...   
diffOperator :: (Int,Int) -> Pixel -> [((Int,Int),Pixel)] -> [[(Int,Int)]]
diffOperator point color picture = trial 0 point (thesePixel point (picture $. point) picture)

------------------------------
-- Another Version.					 |
-- Remove in working version |
------------------------------

--mat = diffQ ++ diffR 
--diffMat ms = makeuniq (((0,0):mat) ++ foldr (++) [] (mapQ ms ++ mapR ms))
--  where
--    mapQ = map (diffofpoint diffQ) 
--    mapR = map (diffofpoint diffR) 
--    makeuniq [] = []
--    makeuniq (x:xs) 
--      | sum [if x==y then 1 else 0 | y <-xs] /= 0  = x : makeuniq [z | z <-xs, x /= z]
--      | otherwise                                  = x : makeuniq xs 
--    diffofpoint ::  [(Int,Int)] -> (Int, Int) -> [(Int,Int)]
--    diffofpoint = 
--      (\ ys x -> [x $+ y | y <- ys, not $ any ((x $+ y) ==) ((0,0):ys++mat), sct (x $+ y) ]) 
--    sct :: (Int, Int) -> Bool
--    sct (x,y) | abs (x) > fst dim || abs (y) > snd dim    = False
--              | otherwise                                 = True
--
--diffMatRek ms n = iterate diffMat ms !! n 
--
--diffOP  = diffMatRek mat 6
--diffOPvar n = diffMatRek mat n 
--
--diffOperator :: (Int,Int) -> Pixel -> [((Int,Int),Pixel)] -> [[(Int,Int)]]
--diffOperator point color picture = h $ g diffOP
--  where
--    g [] = []
--    g (x:xs) 
--      | not $ null [1 | (p,c) <- picture, p == (x $+ point), c /= color] 
--                  = (x $+ point)  : g xs  
--      | otherwise = g xs 
--    h [] = []
--    h ws = (take 4 ws) : (h $ drop 4 ws)
----
--------------------------------------------------

---------------------------------------------------
--------------------------------------------------------------------------------


------------------------------
-- Reduante Version.	  		 |
-- Remove in working version |
------------------------------

--diffMatrix1 xs = xs ++ helper (xs ++ take 1 xs)  
--  where
--  helper xs | length xs < 2 = [] 
--  helper xs = add2ListDup xs : (helper $ tail xs)
--  add2ListDup ys = (\ ((x1,y1):(x2,y2):xs) -> (x1+x2,y1+y2)) ys

---- Fehlerhafte zu manueller Versuch
--diffMatrix2 xs = [(x+x,y+y) | (x,y) <- xs]
--diffMatrix3 xs = (take 4 xs) +++  (drop 4 xs)  
--
--diffMatrix xs = diffMatrix1 xs ++ diffMatrix2 xs
--
--(+++) _ [] = []
--(+++) [] _ = []
--(+++) xs ys = (\ ((x1,y1):qs) ((x2,y2):ws) -> (x1+x2,y1+y2): (qs +++ ws)) xs ys   
--
--------------------------------------------------
--------------------------------------------------------------------------------


{------------------------------------------------------------------------------
--------------------------------- Funktionsname -------------------------------
-------------------------------------------------------------------------------}
-- [Purpose]
-- Hiermit wird das Level Schritt für Schritt überschritten,
-- so dass ein dynamischer Effekt entsteht.
---------------------------------------------------

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

---------------------------------------------------
--------------------------------------------------------------------------------


{------------------------------------------------------------------------------}
{----------------------------- Funktionsname ----------------------------------}
{------------------------------------------------------------------------------}
-- [Purpose]
-- addierte zwei Tuppel
---------------------------------------------------

($+) (x1,y1) (x2,y2) = (x1+x2,y1+y2)

-- Definiert im wesentlichen eine punktweise Auswertung eines levels
($.) :: [((Int,Int),Pixel)] -> (Int, Int) -> Pixel 
($.) level point = headE [c | (p,c) <- level, p == point] 
  where 
    headE [] = Pixel 255 255 255
    headE xs = head xs
---------------------------------------------------
--------------------------------------------------------------------------------

