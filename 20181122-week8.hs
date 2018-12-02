-- Calculates n!
fact :: Integral a => a -> a
fact 0 = 1
fact n = n * fact (n-1)

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

fibonacci' n =
    if n == 0
    then 0
    else if n == 1
         then 1
         else fibonacci' (n-1) + fibonacci' (n-2)

fibonacci'' n
  | n == 0    = 0
  | n == 1    = 1
  | otherwise = fibonacci'' (n-1) + fibonacci'' (n-2)
--   where x = 5
--         y = fact x
--         z = ...

foo x
  | x < 0     = abs x
  | otherwise = y * 2
  where y = fact x

fib n = loop 0 0 1
  where loop i curr next
          | i == n    = curr
          | otherwise = loop (i+1) next (curr+next)

countRoots :: (Num a, Ord a, Eq a) => a -> a -> a -> String
countRoots a b c
  | d < 0     = "No roots"
  | d == 0    = "One root"
  | otherwise = "Two roots"
  where d = b^2 - 4*a*c

gcd' :: Integral a => a -> a -> a
gcd' 0 b = b
gcd' a 0 = a
gcd' a b
  | a > b     = gcd' (a `mod` b) b
  | a == b    = a
  | otherwise = gcd' a (b `mod` a)

ackermann :: Integral a => a -> a -> a
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m-1) 1
ackermann m n = ackermann (m-1)
                          (ackermann m (n-1))

-- distance p q = sqrt ((fst p - fst q)^2
--                    + (snd p - snd q)^2)

distance :: Floating a => (a,a) -> (a,a) -> a
distance (x1,y1) (x2,y2) = sqrt ((x1-x2)^2+(y1-y2)^2)

modulus :: Floating a => (a,a) -> a
modulus = distance (0,0)
