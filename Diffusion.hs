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

mouse :: MyState -> MyState
mouse myState = myState {
 gameplay        = (curpos myState, colors !! curcolcos myState) : gameplay myState 
,diffusionstatus = teile $ thesePixel (curpos myState) ((levels myState !! curlevel myState) $. curpos myState)   
                   (levels myState !! curlevel myState)
 }

linear :: MyState -> MyState
linear myState = myState {
 gameplay        = (curpos myState, colors !! curcolcos myState) : gameplay myState 
,diffusionstatus = teile $ justallPixel (curpos myState) ((levels myState !! curlevel myState) $. curpos myState)   
                   (levels myState !! curlevel myState)
 }

diffusion :: MyState -> MyState
diffusion myState = myState {
 gameplay        = (curpos myState, colors !! curcolcos myState) : gameplay myState 
,diffusionstatus = diffOperator (curpos myState) ((levels myState !! curlevel myState) $. curpos myState)   
                   (levels myState !! curlevel myState)
 }

diffusion2 :: MyState -> MyState
diffusion2 myState = myState {
 gameplay        = (curpos myState, colors !! curcolcos myState) : gameplay myState 
,diffusionstatus = diffOperator2 (curpos myState) ((levels myState !! curlevel myState) $. curpos myState)   
                   (levels myState !! curlevel myState)
 }

-- [Purpose]
-- Hier muss nun die dynamische Diffusion umgesetzt werden.
-- Dabei müssen auch noch die Randbedingungen eingearbeite werden.
---------------------------------------------------
-- 
diffOperator :: (Int,Int) -> Pixel -> [((Int,Int),Pixel)] -> [[(Int,Int)]]
diffOperator point color picture 
 = filter (not . null) $ getSubset (thesePixel point (picture $. point) picture) (diffOP point )
diffOperator2 :: (Int,Int) -> Pixel -> [((Int,Int),Pixel)] -> [[(Int,Int)]]
diffOperator2 point color picture 
 = filter (not . null) $ getSubset (justallPixel point (picture $. point) picture) (diffOP point )
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

{--
 -- generierte eine Liste an Duppeln, die es gerade braucht um mit der movingliste
 -- eine "Diffusion" zu simmulieren
--}

pointGenerate n 
 = [ f d (i,n) | i <- [0..n], d <- [0..7],  if (i == 0 || i == n ) then (d < 4) else True ]
  where 
    f d (i,n) | not $ odd d = (i,n)
              | otherwise = (n,i)

-- Teilt die Matrix auf  
difmat k = foldr (++) [] [ f n (pointGenerate n)  | n <- [0..k]]
  where
    f n xs = [take 4 xs] ++ [[x | x <- take 8 (drop (8*i) (drop 4 xs))] | i <- [0..n-1]]

-- diffOP für ein ganzes Bild ausgehend von einem Punkt
-- für eine ganzes Bild

diffOP point  = filter (not . null) [g ys point | ys <- (difmat 29)]
  where 
  g ys point 
     | length ys == 4     
       = [mv n m point | (mv, (n,m)) <- zip movingList ys, f mv n m point]
     | otherwise 
       = [mv n m point | (mv, (n,m)) <- zip movingList2 ys, f mv n m point]
  f mv n m p | (n == 0 && m == 0) = True
             | mv n m p == p      = False
             | otherwise          = True


{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
{----------------------------- Funktionsname ----------------------------------}
{------------------------------------------------------------------------------}
-- [Purpose]
-- Gibt am Ende all die Pixel aus, die gefärbt werden sollen
---------------------------------------------------
-- Hier einfache Version. 
-- wählt einfach alle Felder mit dieser Farbe aus
justallPixel point color = map (\ (p,c) -> p) . filter (\(p,c) -> c == color) 
--
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
 
thesePixel point color level = [p | (p,c) <- sellevel $ getField curSelCol] 
 where
    curSelCol :: SelectColor
    curSelCol = SelectColor (point) (point) [(point, color)] (point) color level

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

{------------------------------------------------------------------------------}
{----------------------------- Funktionsname ----------------------------------}
{------------------------------------------------------------------------------}
-- [Purpose]
-- Moves zum abgehen des Feldes in k Schritten
---------------------------------------------------

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
  | checkFormv westk southk n m p  = (westk n . southk m) p 
  | otherwise                       = p
se n m p 
  | checkFormv southk eastk n m p  = ita eastk southk n m p 
  | otherwise                          = p

checkFormv f g n m p | (f n p /= p || n == 0) && (g m p /= p || m == 0) = True
                     | otherwise                                        = False

-- hier über muss gecyclt werden
movingList = [nw,ne,ws,se]
movingList2 = [nw,nw,ne,ne,ws,ws,se,se]

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
{----------------------------- Hilfsfunktion ----------------------------------}
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

-- [TODO] für den Effekt noch klein teiliger

teilek _ []  = []
teilek k xs = [take k xs] ++ teilek k (drop k xs)
teile = teilek 2

getSubset xs yss = map (f xs) yss
  where
    f xs ys = [y | y <- ys, y `isIn` xs]
    (isIn) y xs = any (y==) xs
---------------------------------------------------
--------------------------------------------------------------------------------

