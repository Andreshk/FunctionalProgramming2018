-- iei
import Prelude
  hiding ((++), (!!), map, filter)

(++) :: [a] -> [a] -> [a]
[]   ++ lst = lst
lst1 ++ lst2 = (head lst1) : (tail lst1 ++ lst2)

(!!) :: [a] -> Int -> a
[]    !! _ = error "Empty list!!!!!!!!!!"
(h:_) !! 0 = h
(_:t) !! n = t !! (n-1)

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:y:xs) = x < y && sorted (y:xs)

sorted' lst = case lst of [] -> True
                          [_] -> True
                          (x:y:xs) -> x < y && sorted (y:xs)

map f lst = [ f x | x<-lst ]
filter p lst = [ x | x<-lst, p x ]

--foo връща сумата на третите степени на всички
-- четни числа в интервала от а до б
foo a b = sum [ x^3 | x<-[a..b], even x ]


