import Data.List (sort,sortBy,nub,nubBy)
import Data.Ord (comparing)

data Expr = Const Double
          | Var
          | Expr :+: Expr
          | Expr :-: Expr
          | Expr :*: Expr
          | Expr :/: Expr
          | Expr   :^^: Double
          | Double :**: Expr
          deriving Show
infixl 6 :+:
infixl 6 :-:
infixl 7 :*:
infixl 7 :/:
infixr 8 :^^:
infixr 8 :**:

eval :: Expr -> Double -> Double
eval (Const a) _ = a
eval Var x       = x
eval (e1 :+: e2) x = eval e1 x + eval e2 x
eval (e1 :-: e2) x = eval e1 x - eval e2 x
eval (e1 :*: e2) x = eval e1 x * eval e2 x
eval (e1 :/: e2) x = eval e1 x / eval e2 x
eval (e :^^: c)  x = (eval e x)**c
eval (c :**: e)  x = c**(eval e x)

derive :: Expr -> Expr
derive (Const _) = Const 0
derive Var       = Const 1
derive (e1 :+: e2) = (derive e1) :+: (derive e2)
derive (e1 :-: e2) = (derive e1) :-: (derive e2)
derive (e1 :*: e2) = ((derive e1) :*: e2) :+: (e1 :*: (derive e2))
--derive ...

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ el | el<-xs, el<x ]
                ++ [x]
                ++ quicksort (filter (>=x) xs)

-- Сортиране на двойки (String,Int) по остатъци
-- на втората компонента при деление на 5.
data Pair = Pair String Int

instance Eq Pair where
    (Pair x1 y1) == (Pair x2 y2) = y1`mod`5 == y2`mod`5
instance Ord Pair where
    (Pair x1 y1) <= (Pair x2 y2) = y1`mod`5 <= y2`mod`5
instance Show Pair where
    show (Pair x y) = "{{" ++ x ++ ": " ++ show y ++ "}}"

specialSort :: [Pair] -> [Pair]
specialSort = quicksort

lst :: [Pair]
lst = [Pair "iei" 3, Pair "abc" 2, Pair "x" 11, Pair "yz" 48]

type Pair2 = (String,Int)
quicksortBy :: Ord b => (a -> b) -> [a] -> [a]
quicksortBy _ [] = []
quicksortBy _ [x] = [x]
quicksortBy f (x:xs) = quicksortBy f [ el | el<-xs, f el < f x ]
                    ++ [x]
                    ++ quicksortBy f [ el | el<-xs, f el >= f x]

specialSort2 :: [Pair2] -> [Pair2]
specialSort2 lst = sortBy (comparing f) lst
  where f = ((`mod`6).snd)