sumeven1:: [Int] -> Int
sumeven1 xs = if xs == []
    then 0
    else if  (mod (head xs) 2) == 0
        then (head xs) + (sumeven1 (tail xs))
        else (sumeven1 (tail xs))

sumeven2:: [Int] -> Int
sumeven2 [] = 0
sumeven2 (x:xs)
    | (mod x 2) == 0 = x + sumeven2 xs
    | otherwise = sumeven2 xs

sumeven3::[Int] -> Int
sumeven3 xs = foldr f 0 xs
    where
        f  x y
            | (mod x 2) == 0 = x+y
            | otherwise = y

sumeven4::[Int] -> Int
sumeven4 xs = sum (map (\x -> if (mod x 2)==1 then 0 else x)  xs)

sumeven5:: [Int] -> Int
sumeven5 = (foldr (+) 0) . (filter (\x -> mod x 2 == 0 ) )

filtersum :: Num a => (a -> Bool) -> [a] -> a
filtersum p = (sum) . (filter p)

sumeven6 :: [Int] -> Int
sumeven6 = filtersum (\x -> (mod x 2)==0)

data List =
    Cons Int List | Null

sumeven7 :: List -> Int
sumeven7 Null = 0
sumeven7 (Cons x xs)
    | (mod x 2) == 0 = x + sumeven7 xs
    | otherwise = sumeven7 xs
