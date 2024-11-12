data Duck = Duck String Int Float | Duckling String Int Float
  deriving Show

donald :: Duck
donald = Duck "Donald" 6 0.63

daisy :: Duck
daisy = Duck "Daisy" 5 0.56

huey :: Duck
huey = Duckling "Huey" 2 0.23

dewey :: Duck
dewey = Duckling "Dewey" 2 0.25

duckFamily :: [Duck]
duckFamily = [donald,daisy,huey,dewey]

birthday :: Duck -> Duck
birthday (Duck s n h) = Duck s (n+1) h
birthday (Duckling s n h) | n <= 1 = Duckling s (n+1) h
                          | otherwise = Duck s (n+1) h

tall :: Duck -> Bool 
tall (Duck s n h) | h > 0.6   = True
                  | otherwise = False
tall (Duckling s n h) | h > 0.25   = True
                      | otherwise = False
