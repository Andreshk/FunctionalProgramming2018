import Data.List (nub)
prime 1 = False
prime n = null divisors
  where divisors = [ d | d<-[2..n-1], n `mod` d == 0]

primes = [ p | p<-[2..], prime p ]

-- Декартово произведение
desc xs ys = [ (a,b) | a<-xs, b<-ys ]
pi = 3.14
pyth = [ (a,b,c)| c<-[1..],
                  b<-[1..c],
                  a<-[1..b],
                  a^2 + b^2 == c^2 ]
main :: IO ()
main = do
    print (f 5)

f n = (n > 0)

compress [] = []
compress lst = (head lst, count) : compress (drop count lst)
  where count = countMyHead lst
        countMyHead [x] = 1
        countMyHead (x:y:xs)
          | x /= y    = 1
          | otherwise = 1 + countMyHead (y:xs)

--maxRepeated lst = maximum (map snd (compress lst))
maxRepeated lst = maximum [ snd pair | pair <- compress lst ]

histogram lst = [ (x, count x lst) | x <- uniques ]
  where uniques = nub lst
        count x lst = length [ y | y<-lst, x == y]

dist (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

maxDistance lst = maximum [ dist p1 p2 | p1<-lst, p2<-lst ]

zadrask i j m = [ mahni j row | row<-(mahni i m) ]
  where mahni i lst = (take i lst) ++ (drop (i+1) lst)

cross_out m = [ zadrask i j m | i<-[0..rows-1], j<-[0..cols-1] ]
  where rows = length m -- брой редове
        cols = length (head m) -- брой колони <=> брой елементи в първия ред