import Data.Char ( isUpper, isLower )

allTrue :: [Bool] -> Bool
allTrue [] = True
allTrue (x:xs) = x && allTrue xs

allTrue2 :: [Bool] -> Bool
allTrue2 = foldr (&&) True

longestLength :: [[a]] -> Int
longestLength [x] = length x
longestLength (x:xs) = max (length x) (longestLength xs) 

longestLength2 :: [[a]] -> Int
longestLength2 = foldr (max . length) 0 

sumOddSquares :: [Int] -> Int
sumOddSquares xs = sum [ x^2 | x <- xs , odd x ]

sumOddSquares2 :: [Int] -> Int
sumOddSquares2 = foldr ((+) . (^2)) 0 . filter odd

sumOddSquares3 :: [Int] -> Int
sumOddSquares3 = foldr (\x acc -> if odd x then x^2 + acc else acc) 0

sumOddSquares4 :: [Int] -> Int
sumOddSquares4 = foldr addOddSquare 0
    where addOddSquare x acc | odd x = x^2 + acc
                             | otherwise = acc

-- Recursive
shortFWords :: [String] -> Bool
shortFWords [] = False
shortFWords (x:xs) | isUpper (head x) && length x == 4 = True
                   | otherwise = shortFWords xs

-- List comprehension
shortFWords2 :: [String] -> Bool
shortFWords2 xs = not (null [x | x <- xs, isUpper (head x), length x == 4])

-- foldr
shortFWords3 :: [String] -> Bool
shortFWords3 = foldr (\x acc -> (isUpper (head x) && length x == 4) || acc) False


