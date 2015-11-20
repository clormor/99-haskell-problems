-- Exercise 1

> myLast :: [a] -> a
> myLast [] = error "No last element n empty list"
> myLast [x] = x
> myLast (x:xs) = myLast (x:xs)

> myLast' = last

> myLast'' = head . reverse

-- Exercise 2

> lastButOne = head . tail . reverse

> lastButOne' :: [a] -> a
> lastButOne' [] = error "emtpy list"
> lastButOne' [x] = error "singleton list"
> lastButOne' (x:xs) = if length xs == 1 then x else lastButOne' xs

> lastButOne'' = last . init

-- Exercise 3

> elementAt :: [a] -> Int -> a
> elementAt xs x = (last . take x) xs 

-- Exercise 4

> myLength :: [a] -> Int
> myLength = length

> myLength' :: [a] -> Int
> myLength' [] = 0
> myLength' [x] = 1
> myLength' (x:xs) = 1 + myLength' xs

-- Exercise 5

> myReverse = reverse

> myReverse' :: [a] -> [a]
> myReverse' [] = []
> myReverse' xs = myReverse' (tail xs) ++ [head xs]
