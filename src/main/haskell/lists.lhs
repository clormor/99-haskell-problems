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

-- This function is a good candidate for a foldl because
-- -- 1. it takes a list and returns a single value (foldr is better when building up a new list)
-- -- 2. if the list is empty then there is a default (non-error) value

> myLength'' xs = foldl (\n xs -> n+1) 0 xs

-- Exercise 5

> myReverse = reverse

-- This solution works but using append (++) is expensive

> myReverse' :: [a] -> [a]
> myReverse' [] = []
> myReverse' xs = myReverse' (tail xs) ++ [head xs]

-- Using a fold, we can use prepend (:), which is cheaper than append (++)

> myReverse'' xs = foldl (\xs y -> y:xs) [] xs

-- Use the flip function => flip(f(x,y)) = flip(f(y,x))

> myReverse''' xs = foldl (flip (:)) [] xs

-- Exercise 6

> isPallindrome :: (Eq a) => [a] -> Bool
> isPallindrome [] = True
> isPallindrome [x] = True
> isPallindrome xs = 
>	let newList = (take (length xs - 2) . tail) xs in
>		if head xs == last xs then isPallindrome newList else False

> isPallindrome'' :: (Eq a) => [a] -> Bool
> isPallindrome'' [] = True
> isPallindrome'' [x] = True
> isPallindrome'' xs = if head xs == last xs then (\ys -> (isPallindrome'' . take (length ys -2) . tail) ys) xs else False

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
> flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- Exercise 8

> compress :: (Eq a) => [a] -> [a]
> compress [] = []
> compress [x] = [x]
> compress (x:y:xs) = if x==y then compress (y:xs) else [x] ++ compress (y:xs)

-- Exercise 9

> pack :: (Eq a) => [a] -> [[a]]
> pack xs =
>     let pack' [] [] = []
>         pack' [] (x:xs) = pack' [x] xs
>         pack' xs [] = [xs]
>         pack' (x:xs) (y:ys) = if (x==y) then pack' (x:y:xs) ys else [(x:xs)] ++ pack' [y] ys
>     in pack' [] xs

> pack' [] = [] -- if you know the span function, then this exercise is a little easier
> pack' (x:xs) = let (first, second) = span (==x) xs
>     in ((x:first) : pack' second)

-- Exercise 10

> encode :: (Eq a) => [a] -> [(Int,a)]
> encode [] = []
> encode xs =
>     let encode' (n,x) [] = [(n,x)]
>         encode' (n,x) (y:ys) = if (x==y) then encode' (n+1,x) ys else [(n,x)] ++ encode' (1,y) ys
>     in encode' (0, head xs) xs

-- TODO

-- Exercise 14

> dupe [] = []
> dupe xs = [head xs] ++ [head xs] ++ dupe (tail xs)

> dupe'' xs = foldr (\x xs -> x:x:xs) [] xs

> dupe''' xs = foldr ((++) . replicate 2) [] xs

-- Exercise 15

> replica [] n = []
> replica xs n = (replicate n . head) xs ++ (replica . tail) xs n

> replica' xs n = foldr ((++) . replicate n) [] xs

-- Exercise 17

-- TODO 
-- mySplit xs n = [take n xs, 
