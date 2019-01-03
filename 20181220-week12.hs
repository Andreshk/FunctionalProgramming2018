data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

t :: Tree Int
t = Node 5 (Node 2 (Node 10 Empty Empty)
                   (Node 4 (Node 3 Empty Empty)
                           Empty))
           (Node 3 (Node 1 Empty Empty)
                   Empty)

-- можем да избягваме използването на такива функции,
-- и да използваме само pattern matching
isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _     = False

-- В тази функция се налага да разгледаме няколко случая
maxSumPath :: (Num a, Ord a) => Tree a -> a
maxSumPath Empty = 0
maxSumPath (Node val Empty Empty) = val -- листо
maxSumPath (Node val Empty r) = val + maxSumPath r
maxSumPath (Node val l Empty) = val + maxSumPath l
maxSumPath (Node val l r) = val + max (maxSumPath l) (maxSumPath r)

prune, bloom :: Tree a -> Tree a
prune Empty = Empty
prune (Node _ Empty Empty) = Empty
prune (Node val l r) = Node val (prune l) (prune r)
bloom Empty = Empty
bloom (Node val Empty Empty) = Node val newLeaf newLeaf
  where newLeaf = Node val Empty Empty
bloom (Node val l r) = Node val (bloom l) (bloom r)

-- Обикновено когато използваме ротации като част от алгоритми
-- върху по-сложни структури от данни, няма нужда да проверяваме
-- за други случаи (напр. дали ротацията е позволена).
rotateLeft, rotateRight :: Tree a -> Tree a
rotateLeft  (Node p a (Node q b c)) = Node q (Node p a b) c
rotateRight (Node q (Node p a b) c) = Node p a (Node q b c)

-- treeMap :: (a -> b) -> Tree a -> Tree b
-- treeMap _ Empty = Empty
-- treeMap f (Node val l r) = Node (f val) (treeMap f l) (treeMap f r)

-- Дърво с произволен брой наследници... не особено полезно
data VTree a = VEmpty | VNode a [VTree a]
instance Functor VTree where
  fmap f VEmpty = VEmpty
  fmap f (VNode val subtrees) = VNode (f val) (map (fmap f) subtrees)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Node val l r) = Node (f val) (fmap f l) (fmap f r)

-- Използваме друга структура, за да можем да я строим само със
-- специални функции. Щом е друга структура, трябва имената на
-- конструкторите да са други, също.
data BST a = BEmpty | BNode a (BST a) (BST a)
-- Ако искаме pretty-printing, трябва да инстанцираме Show
-- и (в случая) функцията по подразбиране show да извиква нашата
instance Show a => Show (BST a) where
  show t = show' 0 t
    where show' pad BEmpty = replicate pad ' ' ++ "#"
          show' pad (BNode val l r) = show' (pad+2) l
                                   ++ "\n"
                                   ++ replicate pad ' ' ++ show val
                                   ++ "\n"
                                   ++ show' (pad+2) r

-- Да кажем, че повтарянето на стойности е позволено
bstInsert :: Ord a => a -> BST a -> BST a
bstInsert x BEmpty = BNode x BEmpty BEmpty
bstInsert x (BNode val l r)
  | x <= val  = BNode val (bstInsert x l) r
  | otherwise = BNode val l (bstInsert x r)

bstFromList :: Ord a => [a] -> BST a
bstFromList lst = foldr bstInsert BEmpty lst

bstValues :: BST a -> [a]
bstValues BEmpty = []
bstValues (BNode val l r) = bstValues l ++ [val] ++ bstValues r

bstSort :: Ord a => [a] -> [a]
bstSort = bstValues . bstFromList

data Map k v = MEmpty | MNode k v (Map k v) (Map k v)
-- data Maybe a = Nothing | Just a
mapSearch :: Ord k => k -> Map k v -> Maybe v
mapSearch _ MEmpty = Nothing
mapSearch k' (MNode k v l r)
  | k' == k   = Just v
  | k' < k    = mapSearch k' l
  | otherwise = mapSearch k' r

data Direction = L | R deriving Show

bstPath :: Ord a => a -> BST a -> Maybe [Direction]
bstPath _ BEmpty = Nothing
bstPath x (BNode val l r)
  | x == val  = Just []
  | x < val   = (L:) <$> (bstPath x l) -- <$> = fmap, в случая инстанциран за Maybe
  | otherwise = (R:) <$> (bstPath x r)
{- Горното е съкращаване на следния pattern:
  | x < val   = case bstPath x l of Nothing -> Nothing
                                    Just path -> Just (L:path)
  | otherwise = case bstPath x r of Nothing -> Nothing
                                    Just path -> Just (R:path)
-}

-- Подобрение: отделните валути не са различни типове,
-- но всяко число може да върви с валута като "етикет"
data Currency = USD | BGN | EUR deriving Eq
exchangeRate :: Currency -> Currency -> Float
exchangeRate USD BGN = 1.6
exchangeRate BGN USD = 1 / exchangeRate USD BGN
exchangeRate EUR BGN = 1.95
exchangeRate BGN EUR = 1 / exchangeRate BGN EUR
exchangeRate USD EUR = exchangeRate USD BGN * exchangeRate BGN EUR

type Sum = (Float,Currency)
exchange :: Sum -> Currency -> Sum
exchange (x,c) c'
  | c == c'   = (x,c)
  | otherwise = (x*r, c')
  where r = exchangeRate c c'
