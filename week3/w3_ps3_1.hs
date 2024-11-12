import Data.Char

---Maps

toLowerSt' :: String -> String
toLowerSt' [] = []
toLowerSt' (c:cs) = toLower c : toLowerSt cs

toLowerSt :: String -> String
toLowerSt = map toLower

toLowerCons :: Char -> Char
toLowerCons c | c `notElem` "aeiouAEIOU" = toLower c
              | otherwise = c

toLowerConsSt :: String -> String
toLowerConsSt = map toLowerCons

---Filters

onlyLetters' :: String -> String
onlyLetters' [] = []
onlyLetters' (c:cs) | isLetter c  = c : onlyLetters cs
                    | otherwise   =     onlyLetters cs

onlyLetters :: String -> String
onlyLetters = filter isLetter

onlyNumsOrLetters :: String -> String
onlyNumsOrLetters = filter isNumOrLetter
    where 
        isNumOrLetter c
            | isDigit c || isLetter c = True
            | otherwise = False


onlyLettersToLower1 :: String -> String
onlyLettersToLower1 st = map toLower (filter isLetter st) 

onlyLettersToLower2 :: String -> String
onlyLettersToLower2 st = filter isLetter (map toLower st)

---Zips

firstNames :: [String]
firstNames = ["Adam","Brigitte","Charlie","Dora"]

secondNames :: [String]
secondNames = ["Ashe","Brown","Cook","De Santis"]

wholeNames :: [(String, String)]
wholeNames = zip firstNames secondNames

countNames :: [(Int, String)]
countNames = zip [1..] firstNames  

wholeNames2 = zipWith (\f s -> f ++ " " ++ s) firstNames secondNames



rollCall :: [String] -> [String]
rollCall names = zipWith call xs names
  where
    call n name = show n ++ ": " ++ name ++ "? 'Present!'"
    xs = [1..]


