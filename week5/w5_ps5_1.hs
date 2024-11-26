import Data.Char

---IO

quiz :: IO Person
quiz = do
  putStrLn "What is your first name?"
  firstName <- getLine
  putStrLn "What is your second name?"
  secondName <- getLine
  putStrLn "What is your age?"
  age <- getLine
  if not (isNumber' age)
    then do 
    putStrLn "Sorry, that's not a number."
    quiz
    else do
      putStrLn "What is your job?"
      job <- getLine
      putStrLn ("Results: your name is " ++ firstName ++ " " ++ secondName ++ ", your age is " ++ age ++ " and your job is " ++ job ++ ".")
      putStrLn "Is this data correct? Answer Y/N"
      correct <- yesNo
      if not correct
      then quiz
      else return (Person (firstName ++ " " ++ secondName) (read age :: Int) job)


isNumber' :: String -> Bool
isNumber' = all isDigit

yesNo :: IO Bool
yesNo = do
  option <- getLine
  if option == "Y" 
    then return True
  else do
    if option == "N" then return False
      else do
        putStrLn "Please answer Y/N"
        yesNo

data Person = Person String Int String
  deriving Show

survey :: IO [Person]
survey = do
    person <- quiz
    putStrLn "Enter another person? Answer Y/N"
    continue <- yesNo
    if continue
      then do
        additionalPerson <- survey
        return (person : additionalPerson)
      else 
        return [person]

