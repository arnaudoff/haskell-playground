module BinarySearchTree
(
BinarySearchTree,
nil,
isNil,
isNode,
rootBinarySearchTree,
leftTree,
rightTree,
insertBinarySearchTree
) where
    data BinarySearchTree a =
        Nil | Node a (BinarySearchTree a) (BinarySearchTree a) deriving Show

    nil :: BinarySearchTree a
    nil = Nil

    isNil :: BinarySearchTree a -> Bool
    isNil Nil = True
    isNil _ = False

    isNode :: BinarySearchTree a -> Bool
    isNode Nil = False
    isNode _ = True

    rootBinarySearchTree :: BinarySearchTree a -> a
    rootBinarySearchTree Nil = error "No root"
    rootBinarySearchTree (Node x _ _) = x

    leftTree :: BinarySearchTree a -> BinarySearchTree a
    leftTree Nil = error "No left tree"
    leftTree (Node _ t _) = t

    rightTree :: BinarySearchTree a -> BinarySearchTree a
    rightTree Nil = error "No right tree"
    rightTree (Node _ _ t) = t

    insertBinarySearchTree :: (Ord a) => BinarySearchTree a -> a -> BinarySearchTree a
    insertBinarySearchTree Nil x = (Node x Nil Nil)
    insertBinarySearchTree (Node v t1 t2) val
        | val == v = Node v t1 t2
        | val > v = Node v t1 (insertBinarySearchTree t2 val)
        | val < v = Node v (insertBinarySearchTree t1 val) t2
