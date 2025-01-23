--Mahdi Beigahmadi, 301570853, mba188@sfu.ca
--CMPT383_H2
--Spring 2025

--Question #1
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ myFoldlInitialValue [] = myFoldlInitialValue
myFoldl f myFoldlInitialValue (x:xs) = myFoldl f (f myFoldlInitialValue x) xs

--Question #2
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ myFoldrInitialValue [] = myFoldrInitialValue
myFoldr f myFoldrInitialValue (x:xs) = f x (myFoldr f myFoldrInitialValue xs)

--Question #3
alternativeMap :: (a -> b) -> (a -> b) -> [a] -> [b]
alternativeMap _ _ [] = []
alternativeMap f _ [x] = [f x]
alternativeMap f g (x:y:xs) = f x : g y : alternativeMap f g xs

--Question #4
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

--Question #5
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter pred [] = []
myFilter pred (x:xs) | pred x = x : myFilter pred xs
                     | otherwise = myFilter pred xs

--Question #6
sumsqeven :: [Int] -> Int
sumsqeven xs = sum [x^2 | x <- xs, even x]