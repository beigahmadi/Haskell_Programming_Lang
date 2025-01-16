--Mahdi Beigahmadi, 301570853, mba188@sfu.ca
--CMPT383_H1
--Spring 2025

--Question #1
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

--Question #2
listReverse :: [l] -> [l]
listReverse [] = []
listReverse(l:ls) = listReverse ls ++ [l]

--Question #3
listAdd :: [Int] -> [Int] -> [Int]
listAdd [] ys = ys
listAdd xs [] = xs
listAdd (x:xs) (y:ys) = (x + y) : listAdd xs ys


--Question #4
inList :: Eq l => [l] -> l -> Bool
inList [] _ = False                   
inList (x:xs) v
    | x == v    = True                
    | otherwise = inList xs v     

--Question #5
sumTailRec :: [Float] -> Float
sumTailRec xs = sumTailRecAux xs 0
sumTailRecAux :: [Float] -> Float -> Float
sumTailRecAux [] r = r
sumTailRecAux (x:xs) r = sumTailRecAux xs (r + x)


 