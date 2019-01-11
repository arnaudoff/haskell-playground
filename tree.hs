data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

tree = Node 1 
        (Node 12 
            (Node 4 
                Empty 
                Empty
            ) 
            (Node 2 
                Empty 
                (Node 4 
                    Empty 
                    Empty
                )
            )
        ) 
        (Node 21 
            (Node 6 
                Empty 
                Empty
            ) 
            (Node 3 
                Empty 
                Empty
            )
        )

sumElements :: (Num a) => Tree a -> a
sumElements Empty = 0
sumElements (Node x left right) = x + sumElements left + sumElements right

depth :: (Num a, Ord a) => Tree a -> a
depth Empty = 0
depth (Node _ left right) = 1 + max (depth left) (depth right)

maxPath :: (Num a, Ord a) => Tree a -> a
maxPath Empty = 0
maxPath (Node x left right) = x + max (maxPath left) (maxPath right)

cloneLeaf :: (Num a) => Tree a -> Tree a
cloneLeaf Empty = Empty
cloneLeaf (Node x Empty Empty) = Node x (Node x Empty Empty) (Node x Empty Empty)
cloneLeaf (Node x left right) = Node x (cloneLeaf left) (cloneLeaf right)

isLeaf :: (Num a) => Tree a -> Bool
isLeaf (Node x Empty Empty) = True
isLeaf _ = False

removeLeaves :: (Num a) => Tree a -> Tree a
removeLeaves Empty = Empty
removeLeaves (Node x left right)
    | isLeaf left && not (isLeaf right) = Node x Empty (removeLeaves right)
    | not (isLeaf left) && isLeaf right = Node x (removeLeaves left) Empty
    | isLeaf left && isLeaf right = Node x Empty Empty
    | otherwise = Node x (removeLeaves left) (removeLeaves right)

insertElement :: (Ord a) => Tree a -> a -> Tree a
insertElement Empty x = Node x Empty Empty
insertElement (Node rootValue left right) x
  | x < rootValue = Node rootValue (insertElement left x) right
  | x > rootValue = Node rootValue left (insertElement right x)
  | otherwise = Node x left right

traverseTree :: Tree a -> [a]
traverseTree Empty = []
traverseTree (Node x left right) = traverseTree left ++ [x] ++ traverseTree right

sortList :: (Ord a) => [a] -> Tree a
sortList xs = foldr (\x acc -> insertElement acc x) Empty xs
