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
  where sqn = floor (sqrt (fromIntegral n))

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
natPairs = [ (x,sum-x) | sum<-[2..], x<-[1..sum-1] ]

-- Аналогично генерираме Питагоровите тройки, фиксирайки a<b<c за пригледност
pythTriples :: [(Integer,Integer,Integer)]
pythTriples = [ (a,b,c) | c<-[5..], b<-[1..c], a<-[1..b], a^2+b^2==c^2 ]

