-- iei
kol :: [Int]
kol = 1:2:2:gen 1 [2]
  where gen x (1:q) =   x:gen (3-x) (q++[x])
        gen x (2:q) = x:x:gen (3-x) (q++[x,x])

countGroups :: [Int] -> [Int]
countGroups (x:y:xs)
  | x == y    = 2 : countGroups xs
  | otherwise = 1 : countGroups (y:xs)

type Segment = (Int,Int,Int) -- брой тактове, размер, темпо
-- typedef (Int,Int,Int) Segment
-- using Segment = (Int,Int,Int)
type Piece = [Segment]

duration :: Fractional a => Piece -> a
duration p = sum [ broi1*razm1/tempo1 | (broi,razmer,tempo)<-p,
    let broi1  = fromIntegral broi,
    let razm1  = fromIntegral razmer,
    let tempo1 = fromIntegral tempo ]

type Tact = Float
type Party = [Tact]
--template<int N>
--using Party = std::array<float,N> <- не е възможно в Хаскел
type Partiture = [Party]
--template<int N>
--using Partiture = std::vector<Party<N>> <- не е възможно в Хаскел

hasContraPunkt :: Partiture -> Bool
hasContraPunkt pt = or [ isContraPunkt x y | x<-pt, y<-pt ]
  where isContraPunkt :: Party -> Party -> Bool
        isContraPunkt p1 p2 = all (\x -> x == first || x == 1/first) res
          where res = zipWith (/) p1 p2
                first = head res

setUnion :: (Eq a, Ord a) => [a] -> [a] -> [a]
setUnion [] lst2 = lst2
setUnion lst1 [] = lst1
setUnion lst1@(x:xs) lst2@(y:ys)
  | x == y    = x : (setUnion xs ys)
  | x < y     = x : (setUnion xs lst2)
  | otherwise = y : (setUnion lst1 ys)

