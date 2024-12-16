module MinHeap

import Data.Nat
import Data.List
import Decidable.Decidable
import Utilities
import Debug.Trace

public export
interface MinHeap t where
    partial insert : Show a => Ord a => t a -> a -> t a
    findMin : Show a => Ord a => t a -> Maybe a
    deleteMin : Show a => Ord a => t a -> t a

data BinaryTree : Type -> Type where
    Node : Ord a => a -> Maybe (BinaryTree a) -> Maybe (BinaryTree a) -> BinaryTree a

-- BinaryHeap : Type -> Type
-- BinaryHeap a = BinaryTree a

{-
    Add the element to the bottom level of the heap at the leftmost open space.
    Compare the added element with its parent; if they are in the correct order, stop.
    If not, swap the element with its parent and return to the previous step.
 -}

-- insert' : a -> BinaryHeap a -> BinaryHeap a
-- insert' element (Node this Nothing Nothing) = Node this (Just (Node element Nothing Nothing)) Nothing -- insert on left side if at a leaf
-- insert' element (Node this left right) = 

-- I forgot that representing a heap as an array makes this much easier, but after seeing the diagram on Wikipedia it brings back DSA memories
{-
Additionally, a binary heap can be implemented with a traditional binary tree data structure, but there is an issue with finding the adjacent element on the last level on the binary heap when adding an element. This element can be determined algorithmically or by adding extra data to the nodes, called "threading" the tree—instead of merely storing references to the children, we store the inorder successor of the node as well. 
 -}

public export
BinaryHeap : (a : Type) -> Type
BinaryHeap a = List a

children : Nat -> List Nat
children idx = [2*idx + 1, 2*idx+2]

-- should be able to prove that if parent is in bounds then child is also
-- could use Fin
parent : Nat -> Nat
parent idx = (idx `minus` 1) `div` 2

insert' : Ord a => a -> BinaryHeap a -> BinaryHeap a
insert' = flip snoc

public export
heapifyUp : {auto p : Ord a} -> {auto p' : Show a} -> (bh : BinaryHeap a) -> (i : Nat) -> {auto p2 : InBounds i bh} -> BinaryHeap a
heapifyUp heap 0 = heap
heapifyUp heap idx = 
    let p = parent idx
        prf : InBounds p heap = believe_me (InBounds p heap)
        parentVal = p `index` heap
        val = idx `index` heap in --(trace $ show val ++ "------------" ++ show parentVal ++ "\n") $
            if val < parentVal then
                let int : List a = replaceAt p val heap
                    prf : InBounds idx int = believe_me (InBounds idx int)
                    prf2 : InBounds p (replaceAt idx parentVal int) = believe_me (InBounds idx (replaceAt idx parentVal int)) in
                    -- want to prove this in the future, replacing not changing the size seems obvious
                    -- WE HAVE TO UPDATE THE INDEX WHILE RECURSING TO THE PARENT INDEX UGHGHHGHGHGHHG
                --(trace $ "swapping to " ++ show (replaceAt idx parentVal int) ++ "\n") $ 
                heapifyUp (replaceAt idx parentVal int) p
            else heap

public export
heapifyDown : {auto p : Ord a} -> (bh : BinaryHeap a) -> (i : Nat) -> {auto _ : InBounds i bh} -> BinaryHeap a
heapifyDown heap idx =
    let cs' = children idx
        cs = filter (\i => isYes (inBounds i heap)) cs' in
            if isNil cs then heap
            else
                let compFn = (compare `on` (\i => 
                        let prf : InBounds i heap = believe_me (InBounds i heap) in index i heap))
                    minChild = (ne head) (sortBy compFn cs)
                    val = idx `index` heap
                    prf : InBounds minChild heap = believe_me (InBounds minChild heap)
                    childVal = minChild `index` heap in
                        if childVal < val then 
                            let int : List a = replaceAt minChild val heap
                                prf : InBounds idx int = believe_me (InBounds idx int)
                                prf2 : InBounds minChild (replaceAt idx childVal int) = believe_me (InBounds idx (replaceAt idx childVal int)) in
                                -- want to prove this in the future, replacing not changing the size seems obvious
                            heapifyDown (replaceAt idx childVal int) minChild
                        else heap

-- swap root's data with last node, remove last node
remove : {auto p : Ord a} -> (bh : BinaryHeap a) -> BinaryHeap a
remove [x] = []
remove (a::b::xs) = 
    let l = last (a::b::xs) in init (l::(tail (a::b::xs)))
remove [] = []

public export
MinHeap List where
    insert h el = 
        let tmp = insert' el h
            (S lastIdx) = length tmp in heapifyUp tmp lastIdx {p2=believe_me (InBounds lastIdx tmp)}

    findMin = head'

    deleteMin h = 
        case (remove h) of
            (x::xs) => heapifyDown (x::xs) 0
            [] => []

empty : BinaryHeap a
empty = []