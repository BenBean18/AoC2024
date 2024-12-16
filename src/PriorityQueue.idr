module PriorityQueue

data BinaryHeap : a -> Type where
    Node : a -> BinaryHeap a -> BinaryHeap a -> BinaryHeap a
    Leaf : BinaryHeap a -- leaves have no data

-- referencing https://hackage.haskell.org/package/TreeStructures-0.0.2/docs/src/Data-Heap-Binary.html#insert

-- (parent,child) -> (new parent,new child)
swapIfNeeded : Ord a => (BinaryHeap a, BinaryHeap a) -> (BinaryHeap a, BinaryHeap a)
swapIfNeeded (Leaf,h) = (Leaf,h)
swapIfNeeded (h,Leaf) = (h,Leaf)
swapIfNeeded (Node d1 l1 r1, Node d2 l2 r2) = if d1 <= d2 then (Node d1 l1 r1, Node d2 l2 r2) else (Node d2 l1 r1, Node d1 l2 r2)

-- go level by level, which is weird with recursion
-- check both children, then recurse if needed
insert' : Ord a => a -> BinaryHeap a -> Nat -> (BinaryHeap a, Nat)
insert' e Leaf i = (Node e Leaf Leaf, (S i))
insert' e (Node d Leaf n) i = (Node d (Node e Leaf Leaf) n, (S i))
insert' e (Node d n Leaf) i = (Node d n (Node e Leaf Leaf), (S i))
-- this feels really hacky, but I don't know how else to handle the case where the left side is filled
insert' e (Node d l r) i = 
    let (leftTry,leftIters) = insert' e l i
        (rightTry,rightIters) = insert' e r i in
        if leftIters <= rightIters then (Node d leftTry r, (S i)) else (Node d l rightTry, (S i))
