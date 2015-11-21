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

-- Exercise 6

> isPallindrome :: (Eq a) => [a] -> Bool
> isPallindrome [] = True
> isPallindrome [x] = True
> isPallindrome xs = 
>	let newList = (take (length xs - 2) . tail) xs in
>		if head xs == last xs then isPallindrome newList else False

> isPallindrome' xs =
>    let midpoint = length xs `div` 2
>        split xs = [take midpoint xs, (take midpoint . reverse) xs]
>    in (head . split) xs  == (last . split) xs

-- Exercise 7

> data NestedList a = Elem a | List [NestedList a]

> flatten :: NestedList a -> [a]
> flatten (Elem x) = [x]
> flatten (List []) = []
> flatten (List [(Elem x)]) = [x]
> flatten (List (x:xs)) = flatten x ++ flatten (List (xs))

-- Exercise 8

> compress :: (Eq a) => [a] -> [a]
> compress [] = []
> compress [x] = [x]
> compress (x:y:xs) = if x==y then compress (y:xs) else [x] ++ compress (y:xs)

-- Exercise 9 - 13

-- TODO

-- Exercise 14

> dupe [] = []
> dupe xs = [head xs] ++ [head xs] ++ dupe (tail xs)

-- Exercise 15

> replica [] n = []
> replica xs n = (replicate n . head) xs ++ (replica . tail) xs n

-- Exercise 17

-- TODO 
-- mySplit xs n = [take n xs, 
