module AbstractTree (Tree, nil, leaf, branch, cell, left, right, isLeaf, isEmpty, fringe) where
    data Tree a = Nil | Leaf a | Branch (Tree a) (Tree a) deriving (Show)

    fringe :: Tree a -> [a]
    fringe Nil = []
    fringe (Leaf x) = [x]
    fringe (Branch left right) = fringe left ++ fringe right

    nil = Nil
    leaf = Leaf
    branch = Branch
    cell (Leaf a) = a
    left (Branch l r) = l
    right (Branch l r) = r
    isLeaf (Leaf _) = True
    isLeaf _ = False
    isEmpty (Nil) = True
    isEmpty (_) = False
