> myLast :: [a] -> a
> myLast [] = error "No last element n empty list"
> myLast [x] = x
> myLast (x:xs) = myLast (x:xs)

> myLast' :: [a] -> a
> myLast' x = last x

> myLast'' :: [a] -> a
> myLast'' x = (head . reverse) x
