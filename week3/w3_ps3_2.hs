data IntTree = Empty | Node Int IntTree IntTree
  -- deriving Show

t :: IntTree
t = Node 4 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 5 Empty (Node 6 Empty Empty))


------------------------- Exercise 1

isEmpty :: IntTree -> Bool
isEmpty Empty = True
isEmpty _     = False

rootValue :: IntTree -> Int
rootValue Empty        = 0
rootValue (Node i _ _) = i

height :: IntTree -> Int
height Empty = 0
height (Node i l r) = 1 + max (height l) (height r)

find :: Int -> IntTree -> Bool
find _ Empty = False
find n (Node i l r) | i == n = True
                    | otherwise = find n l || find n r


-------------------------


instance Show IntTree where
    show = unlines . aux ' ' ' '
      where
        aux _ _ Empty = []
        aux c d (Node x s t) = 
          [ c:' ':m | m <- aux ' ' '|' s ] ++ 
          ['+':'-':show x] ++ 
          [ d:' ':n | n <- aux '|' ' ' t ]


------------------------- Exercise 2

member :: Int -> IntTree -> Bool
member _ Empty = False
member n (Node i l r) | n == i = True
                      | n > i = member n r
                      | n < i = member n l   

largest :: IntTree -> Int
largest Empty            = error "No largest element of an empty tree"
largest (Node x l Empty) = x
largest (Node x l r)     = largest r

deleteLargest :: IntTree -> IntTree
deleteLargest Empty = error "No element to delete"
deleteLargest (Node i l Empty) = l
deleteLargest (Node i l r) = Node i l (deleteLargest r) 

delete :: Int -> IntTree -> IntTree
delete _ Empty = error "No element to delete since the tree is Empty"
delete y (Node x l r)
    | y < x     = Node x (delete y l) r
    | y > x     = Node x r (delete y r)
    | isEmpty l = r
    | otherwise = Node (largest l) (deleteLargest l) r

