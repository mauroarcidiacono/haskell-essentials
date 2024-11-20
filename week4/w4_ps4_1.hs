import Data.Char

----List Comprehension

toLowerSt :: String -> String
toLowerSt s = [toLower c | c <- s]

onlyLetters :: String -> String
onlyLetters s = [c | c <- s, isLetter c]

onlyNumsOrLetters :: String -> String
onlyNumsOrLetters s = [c | c <- s, isLetter c || isDigit c]

onlyLettersToLower :: String -> String
onlyLettersToLower s = [toLower c | c <- s, isLetter c]
-- It corresponds to onlyLettersToLower1