> lastButOne :: [a] -> a
> lastButOne x = (head . tail . reverse) x

> lastButOne' :: [a] -> a
> lastButOne' [] = error "emtpy list"
> lastButOne' [x] = error "singleton list"
> lastButOne' (x:xs) = if length xs == 1 then x else lastButOne' xs

> lastButOne'' :: [a] -> a
> lastButOne'' = last . init
