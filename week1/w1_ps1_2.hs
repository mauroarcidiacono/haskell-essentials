--Lists

halving :: Int -> [Int]
halving n | n == 0 = []
          | odd n  = n : halving (n-1)
          | even n = n : halving (div n 2)

collatz :: Int -> [Int]
collatz n | n == 1 = [1]
          | even n = n : collatz (div n 2)
          | odd n  = n : collatz (3*n + 1)

colLength :: Int -> Int
colLength n = length (collatz n)

-- Pattern Matching

maxList :: [Int] -> Int
maxList [] = 0
maxList [x] = x
maxList (x:xs) = max x (maxList xs)


allDucks :: [String] -> Bool
allDucks [] = True
allDucks (x:xs) = x == "duck" && allDucks xs

duckDuckGoose :: [String] -> Bool
duckDuckGoose [x] = x == "goose"
duckDuckGoose (x:xs) = x == "duck" && duckDuckGoose xs

--Pairs

ducks :: [(String,Int)]
ducks = [("Donald",6),("Daisy",5),("Huey",2),("Louie",2),("Dewey",2)]

noDDucks :: [(String,Int)] -> [String]
noDDucks [] = []
noDDucks ((name,age):xs) | head name == 'D' = noDDucks xs
                         | otherwise = name : noDDucks xs

youngOrShort :: [(String,Int)] -> Bool
youngOrShort [] = False
youngOrShort ((name, age):xs) | age < 3 || length name <= 3 = True
                              | otherwise = youngOrShort xs 

describeDucks :: [(String,Int)] -> String
describeDucks [] = "No ducks"
describeDucks ((name,age):xs) = "The duck " ++ name ++ " is a duck who is" ++ show age ++ " years old. " ++ describeDucks xs

