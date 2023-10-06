import Data.List (partition)

doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x < 100
                        then x * 2
                        else x

quickSort :: (Ord a) => [a] -> [a]

quickSort []     = []
quickSort (x:xs) = quickSort [a | a <- xs, a <= x] ++
                   [x] ++
                   quickSort [a | a <- xs, a > x]

quickSort' :: (Ord a) => [a] -> [a]

quickSort' []     = []
quickSort' (x:xs) = pSort xs [] []
    where pSort [] l g = quickSort' l ++ [x] ++ quickSort' g
          pSort (h:t) l g
                | x < h     = pSort t l (h:g)
                | otherwise = pSort t (h:l) g


quickSort'' :: (Ord a) => [a] -> [a]

quickSort'' []     = []
quickSort'' (x:xs) = quickSort'' l ++ [x] ++ quickSort'' g
    where (l, g) = partition (<x) xs

φ :: (Ord a) => [a] -> [a]
φ []     = []
φ (x:xs) = φ lt ++ [x] ++ φ ge
    where (lt, ge) = partition (<x) xs
