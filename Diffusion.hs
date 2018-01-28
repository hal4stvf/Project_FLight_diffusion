module Diffusion where
import Network.MateLight.Simple
import PixelHandling
import Daten

{------------------------------------------------------------------------------}
{----------------------------- Funktionsname ----------------------------------}
{------------------------------------------------------------------------------}
-- [Purpose]
-- Diese Funktion wird durch die Taste 'p' aufgerufen. Sie berechnet die 
-- Diffusion für den aktuellen Cursorpunkt
-- 
---------------------------------------------------

-- Nutz zur Zeit den Algorithmus, das eigentlich nur das Farbfeld detekten soll.

--diffusion :: MyState -> MyState
--diffusion myState = myState {
-- gameplay        = (curpos myState, colors !! curcolcos myState) : gameplay myState 
--,diffusionstatus = teile $ thesePixel (curpos myState) 
--                   ((levels myState !! curlevel myState) $. (curpos myState))
--                   (levels myState !! curlevel myState)
--         }

------------------------------
-- Diffusion Version.				 |
-- Use    in working version |
------------------------------

diffusion :: MyState -> MyState
diffusion myState = myState {
 gameplay        = (curpos myState, colors !! curcolcos myState) : gameplay myState 
,diffusionstatus = diffOperator (curpos myState) 
                   (colors !! curcolcos myState)   
                   (levels myState !! curlevel myState)
         }
--
--------------------------------------------------

---------------------------------------------------
{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
{----------------------------- Funktionsname ----------------------------------}
{------------------------------------------------------------------------------}
-- [Purpose]
-- Zweite Idee für die Diffusion 
-- Diese Zeilen produzieren wesentlich einen diskreten 
-- (hoffentlich gleich) dynamischen "Diffusionoperator"
---------------------------------------------------

-- generierte eine Liste an Duppeln, die es gerade braucht um mit der movingliste
-- eine "Diffusion" zu simmulieren
pointGenerate n = [ f d (i,n) | i <- [0..n], d <- [0..7],  if (i == 0 || i == n ) then (d < 4) else True ]
  where 
    f d (i,n) | not $ odd d = (i,n)
              | otherwise = (n,i)

-- diffOP für ein ganzes Bild ausgehend von einem Punkt
-- für eine ganzes Bild
--pointBild = foldr (++) [] [pointGenerate n | n <- [0..29]]
diffOP point  = filter (not . null) [g ys point | ys <- (difmat 29)]
  where 
  g ys point 
     | length ys == 4     
       = [mv n m point | (mv, (n,m)) <- zip movingList ys, f mv n m point]
     | otherwise 
       = [mv n m point | (mv, (n,m)) <- zip movingList2 ys, f mv n m point]
--  f _ _ _ _ = True
  f mv n m p | (n == 0 && m == 0) = True
             | mv n m p == p      = False
             | otherwise          = True

-- aufteilungs 
difmat k = foldr (++) [] [ f n (pointGenerate n)  | n <- [0..k]]
  where
    f n xs = [take 4 xs] ++ [[x | x <- take 8 (drop (8*i) (drop 4 xs))] | i <- [0..n-1]]

getSubset xs yss = map (f xs) yss
  where
    f xs ys = [y | y <- ys, y `isIn` xs]
    (isIn) y xs = any (y==) xs

--
------------------------------
-- Another Version.					 |
-- Remove in working version |
------------------------------

--
--diffOP point =  [ g ys point | ys <- diffmat]) : (rekur (k + 1))
--g ys point 
--   | length ys == 4     
--     = [mv n m point | (mv, (n,m)) <- zip movingList ys, (mv n m point) `inPic` picture]
--   | otherwise 
--     = [mv n m point | (mv, (n,m)) <- zip movingList2 ys, (mv n m point) `inPic` picture]
--inPic p xs = any (p ==) xs 
--
--------------------------------------------------
--
--

-- [TODO] für den Effekt noch klein teiliger

teilek _ []  = []
teilek k xs = [take k xs] ++ teilek k (drop k xs)
teile = teilek 2

------------------------------
-- Another Version.					 |
-- Remove in working version |
------------------------------

--checkForthisnumber n _ j | j >= n = [] 
--checkForthisnumber n i j  = [j,j+i] ++ checkForthisnumber n (i*2) (j+i+1) 
--isThisTrueList = [0,1,2] ++ checkForthisnumber 900  2 3
----
------ Hier muss noch eine ifabfrage rein
--helperTeileThis n xs = [[x | x <- selectRight i xs] | i <- [0..n-1]]
--  where 
--     selectRight i | any (i==) isThisTrueList  = (take 4 . drop (i*4))
--                   | otherwise = (take 8 . drop (4*i))
--teileThis = helperTeileThis 900 
--
------------------------------------------------
--------------------------------------------------


--diffopPic point = [mv n m point | (mv, (n,m)) <- zip movingList2 ys ]

-- eigentlich lösung?
-- [TODO] unbedingt an den Abbruchbedinugen arbeiten, diese sind nur hart implementiert
--
--trial k point picture = rekur k
--  where  
--   rekur k  | k == 80 = []
--   rekur k = (foldr (++) [] [ g ys point | ys <- teile $ pointGenerate k]) : (rekur (k + 1))
--   g ys point 
--      | length ys == 4     
--        = [mv n m point | (mv, (n,m)) <- zip movingList ys, (mv n m point) `inPic` picture]
--      | otherwise 
--        = [mv n m point | (mv, (n,m)) <- zip movingList2 ys, (mv n m point) `inPic` picture]
--   inPic p xs = any (p ==) xs 

--trial point color 
--------------------------------------------------
--------------------------------------------------
{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
{----------------------------- Funktionsname ----------------------------------}
{------------------------------------------------------------------------------}
-- [Purpose]
-- Gibt am Ende all die Pixel aus, die gefärbt werden sollen
---------------------------------------------------

-- das Spielfeld abgeht ... 

data SelectColor = SelectColor {
   lastpoint    :: (Int, Int) 
  ,currentpoint :: (Int, Int) 
  ,sellevel     :: [((Int,Int),Pixel)]
  ,startpoint   :: (Int,Int)
  ,color        :: Pixel
  ,level        :: [((Int,Int),Pixel)] 
  } deriving (Eq, Ord, Show, Read)

-- [TODO] Hier muss wohl die Startbedingung noch einmal überarbeitet werden
-- Übergibt ein SelectColorRecord an getField mit initaliziertem Startvektor
-- und den Farben etc.
-- 
-- 
thesePixel point color level = [p | (p,c) <- sellevel $ getField curSelCol] 
 where
    curSelCol :: SelectColor
    curSelCol = SelectColor (point) (point) [(point, color)] (point) color level
--    curSelCol = SelectColor (firstedge point) (firstedge point) [(firstedge point, color)] (firstedge point) color level
--    firstedge point | gofurhter point      = firstedge $ north point
--                    | otherwise            = point 
--    gofurhter point | north point /= point && level $. north point == color = True 
--                    | otherwise                                       = False

-- [TODO] Hier muss wohl die Abbruchbedingung noch einmal überarbeitet werden
-- Versucht alle Felder ausgehend von einem Startvektor zu erreichen 
-- [TODO] Es muss in alle Richtung gleich gegangen werden.
getField mySC | endOfField $ currentpoint mySC  = mySC { sellevel = savePoint (sellevel mySC)} 
              | otherwise 
    = getField mySC { lastpoint = g , currentpoint = f (currentpoint mySC), 
      sellevel = savePoint (sellevel mySC)}
  where
    f p 
      | goFurther north p  = north p
      | goFurther east p  = east p
      | goFurther south p  = south p
      | goFurther west p  = west p
    savePoint xs = xs ++ [(currentpoint mySC, color mySC)]
    visitBefore a = not $ any (\ (p,c) -> p == a) $ sellevel mySC
    goFurther :: ((Int,Int) -> (Int,Int)) -> (Int,Int) -> Bool
    goFurther moveto p 
      = (moveto p /= p) && ((level mySC $. (moveto p)) == color mySC) && (visitBefore (moveto p))
    endOfField p =  (not $ goFurther north p )
                 && (not $ goFurther east p ) 
                 && (not $ goFurther south p) 
                 && (not $ goFurther west p )
    g = currentpoint mySC          

------------------------------
-- Another Version.					 |
-- Remove in working version |
------------------------------

---- [TODO] Hier muss wohl die Abbruchbedingung noch einmal überarbeitet werden
----selectColor :: (Int,Int) -> Pixel -> [((Int, Int),Pixel)] -> SelectColor 
--selectColor point color level = [p | (p,c) <- sellevel $ rekursion curSelCol]
--  where  
--    rekursion x | startpoint x == f (currentpoint x) x       = x
--    rekursion x = rekursion  x { lastpoint = currentpoint x, currentpoint = f (currentpoint x) x, 
--      sellevel = g (sellevel x) x}
--    f p x
--      | goFurther north p x = north p
--      | goFurther west  p x = west  p
--      | goFurther south p x = south p
--      | goFurther east  p x = east  p
--      | otherwise = startpoint curSelCol
--    g xs x= (currentpoint x, color) : xs
--    h x a = not $ any (\ (p,c) -> p == a) (sellevel x)
--    firstedge point | gofurhter point      = firstedge $ north point
--                    | otherwise            = point 
--    gofurhter point | north point /= point && level $. north point == color = True 
--                    | otherwise                                       = False
--    curSelCol :: SelectColor
--    curSelCol = SelectColor (firstedge point) (firstedge point) [(firstedge point, color)] (firstedge point) color
--    goFurther moveto p x
--      = (moveto p /= p) && ((level $. (moveto p)) == color) && (h x (moveto p))
--
--------------------------------------------------
    

-- Moves zum abgehen des Feldes in k Schritten

northk k (x,y)  | y-k >= 0          = (x,y-k)
                | otherwise         = (x,y)
westk  k (x,y)  | x-k >= 0          = (x-k,y)
                | otherwise         = (x,y)
southk k (x,y)  | y+k <= snd dim    = (x,y+k)
                | otherwise         = (x,y)
eastk  k (x,y)  | x+k <= fst dim    = (x+k,y)
                | otherwise         = (x,y)

-- Moves in einer Schritten

north = northk 1
west  = westk 1
south = southk 1
east  = eastk 1


-- Hilfsfunktion zum "iterieren" über die moves in verschiedene Richtung
ita f g x y p = f x (g y p) 

ne n m p 
  | checkFormv northk eastk n m p  = ita northk eastk n m p 
  | otherwise                          = p
nw n m p 
  | checkFormv northk westk n m p  = ita northk westk n m p 
  | otherwise                        = p
ws n m p 
  | checkFormv southk westk n m p  = ita westk southk n m p
  | otherwise                        = p
se n m p 
  | checkFormv southk eastk n m p  = ita southk eastk n m p 
  | otherwise                          = p

checkFormv f g n m p | (f n p /= p || n == 0) && (g m p /= p || m == 0) = True
                     | otherwise                                        = False

-- hier über muss gecyclt werden
movingList = [nw,ne,ws,se]
movingList2 = [nw,nw,ne,ne,ws,ws,se,se]

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
diffOperator point color picture = filter (not . null) $ getSubset (thesePixel point (picture $. point) picture) (diffOP point )
--diffOperator point color picture = trial 0 point (thesePixel point (picture $. point) picture)

{------------------------------------------------------------------------------}


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
      h (d:ds) s = h ds (selectpixelmap d s)
      selectpixelmap d = map (\(p,c) -> if (p /= d) then (p,c) else (p,newcolor)) 
      diffstat = head $ diffusionstatus myState
      newcolor = snd $ head $ gameplay myState

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
-- Schwarz falls der Pixel nicht gefunden wurde
($.) :: [((Int,Int),Pixel)] -> (Int, Int) -> Pixel 
($.) level point = headE [c | (p,c) <- level, p == point] 
  where 
    headE [] = Pixel 0 0 0 
    headE xs = head xs
---------------------------------------------------
--------------------------------------------------------------------------------

