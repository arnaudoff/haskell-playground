module Stack (StackType, pop, push, empty, isEmpty) where
    data StackType a = EmptyStack | StackCons a (StackType a) deriving (Show)

    push :: a -> StackType a -> StackType a
    push x s = StackCons x s

    pop :: StackType a -> StackType a
    pop (StackCons _ rest) = rest
    pop EmptyStack = error "Stack is empty"

    empty :: StackType a
    empty = EmptyStack

    isEmpty :: StackType a -> Bool
    isEmpty EmptyStack = True
    isEmpty _ = False
