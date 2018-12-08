--replicate' n x = map (const x) [1..n]
replicate' :: Int -> a -> [a]
replicate' n x = [ x | i<-[1..n] ]

-- използваме главата на списъка като неутрален
-- елемент и "обхождаме" само опашката
minimum' :: Ord a => [a] -> a
minimum' [] = error "Oops!!!1!"
minimum' (x:xs) = foldr min x xs

reverse' lst = foldr (\el res -> res ++ [el]) [] lst

length' :: [a] -> Int
length' lst = foldr (\_ res -> res+1) 0 lst
--length' lst = sum [ 1 | _<-lst ]
--length' lst = sum (map (const 1) lst)
--length' = sum . map (const 1)

all' p lst = foldr (\el res -> p el && res) True  lst
any' p lst = foldr (\el res -> p el || res) False lst

append' lst1 lst2 = foldr (:) lst2 lst1

-- Това много прилича на начина, по който се имплементира filter с foldr
makeSet' lst = foldr (\el res -> if el `elem` res then res else el:res) [] lst

-- За феновете: за жалост GHCi иска да напишем изрично сигнатурата
length1 :: [a] -> Int
length1 = foldr ((succ.).(flip const)) 0
all1 :: (a -> Bool) -> [a] -> Bool
all1 p = foldr ((&&).p) True
append1 :: [a] -> [a] -> [a]
append1 = flip $ foldr (:)

-- Брой делители на число
divisorsCount :: Int -> Int
divisorsCount n = length [ i | i<-[1..n], n `mod` i == 0 ]

prime :: Integer -> Bool
prime 1 = False
prime n = null [ i | i<-[2..sqn], n `mod` i == 0]
  -- заради строгата типова система използваме два помощни
  -- функции за кастване от/към число с плаваща запетая
  where sqn = floor . sqrt . fromIntegral $ n

primes :: [Integer]
primes = filter prime [1..]

-- Ситото на Ератостен
primes' :: [Integer]
primes' = sieve [2..]
  where sieve (x:xs) = x : sieve (dropEveryMultipleOf x xs)
        dropEveryMultipleOf n lst = filter (\x -> x `mod` n /= 0) lst


descartes :: [a] -> [b] -> [(a,b)]
descartes lst1 lst2 = [(x,y) | x<-lst1, y<-lst2]

-- Проблем: ще се генерират (1,1),(1,2),(1,3),...
-- и никога няма да се стигне до наредена двойка
-- с друга първа компонента
--natPairs = descartes [1..] [1..]

natPairs :: [(Integer,Integer)]
natPairs = [ (x,sum-x) | sum<-[2..],
                         x<-[1..sum-1] ]

-- Максимално ефективно решение
pythTriples :: [(Integer,Integer,Integer)]
pythTriples = [ (a,b,c) | c<-[5..],
                          let sht = floor (sqrt' (c^2 `div` 2)),
                          a<-[1..sht],
                          let f = sqrt'(c^2-a^2),
                          let b = floor f,
                          floor f == ceiling f]
  where sqrt' n = sqrt (fromIntegral n)

-- Помощна функция за компресията
countMyHead :: Eq a => [a] -> Int
countMyHead lst = length (takeWhile (== head lst) lst)

-- a.k.a. run-length encoding
compress :: Eq a => [a] -> [(a,Int)]
compress [] = []
compress lst = (head lst, count) : compress (drop count lst)
  where count = countMyHead lst

compress' :: Eq a => [a] -> [(a,Int)]
compress' [] = []
compress' lst = (head lst, length heads) : compress' rest
  where (heads,rest) = span (== head lst) lst

-- Композицията на функции се изразява както в математиката
maxRepeated :: Eq a => [a] -> Int
--maxRepeated lst = maximum (map snd (compress lst))
maxRepeated = maximum . (map snd) . compress

makeSet :: Eq a => [a] -> [a]
makeSet [] = []
makeSet (x:xs) = x : makeSet (filter (/=x) xs)

histogram lst = map (\el -> (el, count el lst)) (makeSet lst)
  where count x lst = length $ filter (==x) lst