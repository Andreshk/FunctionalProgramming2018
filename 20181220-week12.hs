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

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Node val l r) = Node (f val) (fmap f l) (fmap f r)

-- Използваме друга структура, за да можем да я строим само със
-- специални функции. Щом е друга структура, трябва имената на
-- конструкторите да са други, също.
data BST a = BEmpty | BNode a (BST a) (BST a)

-- Да кажем, че повтарянето на стойности е позволено
bstInsert :: Ord a => a -> BST a -> BST a
bstInsert x BEmpty = BNode x BEmpty BEmpty
bstInsert x (Node val l r)
  | x <= val  = Node val (bstInsert x l) r
  | otherwise = Node val l (bstInsert x r)
