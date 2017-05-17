import Control.Monad
import Control.Comonad -- cabal install comonad

--- 1 dimensional cell automaton with zipper
data LineZipper a = LineZipper [a] a [a]

toLeft :: LineZipper a -> LineZipper a
toLeft (LineZipper (l:ls) cursor r) = LineZipper ls l (cursor:r)

toRight :: LineZipper a -> LineZipper a
toRight (LineZipper l cursor (r:rs)) = LineZipper (cursor:l) r rs

readCursor :: LineZipper a -> a
readCursor (LineZipper _ cursor _) = cursor

writeCursor :: LineZipper a -> a -> LineZipper a
writeCursor (LineZipper l _ r) cursor = LineZipper l cursor r

toList :: LineZipper a -> Int -> [a]
toList (LineZipper l cursor r) n = reverse (take n l) ++ [cursor] ++ take n r

move' :: (z a -> z a) -> (z a -> z a) -> z a -> LineZipper (z a)
move' a b z = LineZipper (iterate' a z) z (iterate' b z)

iterate' :: (a -> a) -> a -> [a]
iterate' f = tail . iterate f

instance Functor LineZipper where
	fmap f (LineZipper l cursor r) = LineZipper (map f l) (f cursor) (map f r)

instance Comonad LineZipper where
	extract = readCursor
	duplicate = move' toLeft toRight


--- 2 dimensional cell automaton with zipper of zippers
data PlaneZipper a = PlaneZipper (LineZipper (LineZipper a))


up :: PlaneZipper a -> PlaneZipper a
up (PlaneZipper x) = PlaneZipper (toLeft x)

down :: PlaneZipper a -> PlaneZipper a
down (PlaneZipper x) = PlaneZipper (toRight x)

left :: PlaneZipper a -> PlaneZipper a
left (PlaneZipper x) = PlaneZipper (fmap toLeft x)
 
right :: PlaneZipper a -> PlaneZipper a
right (PlaneZipper x) = PlaneZipper (fmap toRight x)

horizontal :: PlaneZipper a -> LineZipper (PlaneZipper a)
horizontal = move' left right
 
vertical :: PlaneZipper a -> LineZipper (PlaneZipper a)
vertical = move' up down

readLine :: PlaneZipper a -> a
readLine (PlaneZipper z) = readCursor $ readCursor z
 
writeLine :: a -> PlaneZipper a -> PlaneZipper a
writeLine x (PlaneZipper z) = 
	PlaneZipper $ writeCursor z newLine
    	where
       		newLine = writeCursor oldLine x
       		oldLine = readCursor z

instance Functor PlaneZipper where
   fmap f (PlaneZipper z) = PlaneZipper (fmap (fmap f) z)

instance Comonad PlaneZipper where
   extract = readLine 
   duplicate x = PlaneZipper $ fmap horizontal $ vertical x

---Game of life
neighbours :: [PlaneZipper a -> PlaneZipper a]
neighbours =
  horiz ++ vert ++ liftM2 (.) horiz vert
    where
      horiz = [left, right]
      vert  = [up, down]

aliveNeighbours :: PlaneZipper Bool -> Int
aliveNeighbours z =
  card $ map (\ dir -> extract $ dir z) neighbours 

card :: [Bool] -> Int
card = length . filter (==True)

rule :: PlaneZipper Bool -> Bool
rule z =
   case aliveNeighbours z of
     2 -> extract z
     3 -> True
     _ -> False

evolve :: PlaneZipper Bool -> PlaneZipper Bool
evolve = extend rule

---Display

dispLine :: LineZipper Bool -> String
dispLine z =
  map dispC $ toList z 6
    where
      dispC True  = '*'
      dispC False = ' '

disp :: PlaneZipper Bool -> String
disp (PlaneZipper z) =
  unlines $ map dispLine $ toList z 6


---Glider
glider :: PlaneZipper Bool
glider =
  PlaneZipper $ LineZipper (repeat fz) fz rs
    where
      rs = [ line [f, t, f]
           , line [f, f, t]
           , line [t, t, t]
           ] ++ repeat fz
      t = True
      f = False
      fl = repeat f
      fz = LineZipper fl f fl
      line l =
        LineZipper fl f (l ++ fl)


---Main

main = do
  putStr.disp $ glider
  putStr.disp $ evolve $ glider
  putStr.disp $ evolve.evolve $ glider
  putStr.disp $ evolve.evolve.evolve $ glider
  putStr.disp $ evolve.evolve.evolve.evolve $ glider


---Duals: Comonad and Monad
--class Functor w => Comonad w where
---extract :: w a -> a
---extend :: (w a -> b) -> w a -> w b
---duplicate :: w a -> w (w a)

--class Functor m => Monad m where
---return :: a -> m a
---(=<<) :: (a -> m b) -> m a -> m b
---join :: m (m a) -> m a



