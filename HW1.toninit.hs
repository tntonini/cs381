module HW1 where


-- | Integer-labeled binary trees.
data Tree
   = Node Int Tree Tree   -- ^ Internal nodes
   | Leaf Int             -- ^ Leaf nodes
  deriving (Eq,Show)


-- | An example binary tree, which will be used in tests.
t1 :: Tree
t1 = Node 1
      (Node 2
        (Node 3
          (Leaf 4)
          (Leaf 5))
        (Leaf 6))
      (Node 7
        (Leaf 8)
        (Leaf 9))

-- | Another example binary tree, used in tests.
t2 :: Tree
t2 = Node 6
        (Node 2
            (Leaf 1)
          (Node 4
            (Leaf 3)
            (Leaf 5)))
        (Node 8
          (Leaf 7)
          (Leaf 9))


-- | The integer at the left-most node of a binary tree.
--
--   >>> leftmost (Leaf 3)
--   3
--
--   >>> leftmost (Node 5 (Leaf 6) (Leaf 7))
--   6
--
--   >>> leftmost t1
--   4
--
--   >>> leftmost t2
--   1
--
leftmost :: Tree -> Int
leftmost (Leaf left_leaf)  = left_leaf
leftmost (Node _ left _)   = leftmost left



-- | The integer at the right-most node of a binary tree.
--
--   >>> rightmost (Leaf 3)
--   3
--
--   >>> rightmost (Node 5 (Leaf 6) (Leaf 7))
--   7
--
--   >>> rightmost t1
--   9
--
--   >>> rightmost t2
--   9
--
rightmost :: Tree -> Int
rightmost (Leaf right_leaf)   = right_leaf
rightmost (Node _ _ right)    = rightmost right


-- | Get the maximum integer from a binary tree.
--
--   >>> maxInt (Leaf 3)
--   3
--
--   >>> maxInt (Node 5 (Leaf 4) (Leaf 2))
--   5
--
--   >>> maxInt (Node 5 (Leaf 7) (Leaf 2))
--   7
--
--   >>> maxInt t1
--   9
--
--   >>> maxInt t2
--   9
--
--node_int = int in the node constructor
--t1 and t2 are the 2 trees in the node constructor
maxInt :: Tree -> Int
maxInt (Leaf leaf_int)         = leaf_int
maxInt (Node node_int t1 t2)   = max node_int (max (maxInt t1) (maxInt t2))

-- | Get the minimum integer from a binary tree.
--
--   >>> minInt (Leaf 3)
--   3
--
--   >>> minInt (Node 2 (Leaf 5) (Leaf 4))
--   2
--
--   >>> minInt (Node 5 (Leaf 4) (Leaf 7))
--   4
--
--   >>> minInt t1
--   1
--
--   >>> minInt t2
--   1
--
--node_int = int in the node constructor
--t1 and t2 are the 2 trees in the node constructor
minInt :: Tree -> Int
minInt (Leaf leaf_int)         = leaf_int
minInt (Node node_int t1 t2)   = min node_int (min (minInt t1) (minInt t2))


-- | Get the sum of the integers in a binary tree.
--
--   >>> sumInts (Leaf 3)
--   3
--
--   >>> sumInts (Node 2 (Leaf 5) (Leaf 4))
--   11
--
--   >>> sumInts t1
--   45
--
--   >>> sumInts (Node 10 t1 t2)
--   100
--

--node_int = int in the node constructor
--t1 and t2 are the 2 trees in the node constructor
sumInts :: Tree -> Int
sumInts (Leaf leaf_int)         = leaf_int
sumInts (Node node_int t1 t2)   = node_int + (sumInts t1) +(sumInts t2)

-- | The list of integers encountered by a pre-order traversal of the tree.
--
--   >>> preorder (Leaf 3)
--   [3]
--
--   >>> preorder (Node 5 (Leaf 6) (Leaf 7))
--   [5,6,7]
--
--   >>> preorder t1
--   [1,2,3,4,5,6,7,8,9]
--
--   >>> preorder t2
--   [6,2,1,4,3,5,8,7,9]
--
preorder :: Tree -> [Int]
preorder (Leaf leaf_int)         = [leaf_int]
preorder (Node leaf_int t1 t2)   = [leaf_int] ++ preorder t1 ++ preorder t2


-- | The list of integers encountered by an in-order traversal of the tree.
--
--   >>> inorder (Leaf 3)
--   [3]
--
--   >>> inorder (Node 5 (Leaf 6) (Leaf 7))
--   [6,5,7]
--
--   >>> inorder t1
--   [4,3,5,2,6,1,8,7,9]
--
--   >>> inorder t2
--   [1,2,3,4,5,6,7,8,9]
--
inorder :: Tree -> [Int]
inorder (Leaf leaf_int)         = [leaf_int]
inorder (Node leaf_int t1 t2)   = inorder t1 ++ [leaf_int] ++ inorder t2

-- | Check whether a binary tree is a binary search tree.
--
--   >>> isBST (Leaf 3)
--   True
--
--   >>> isBST (Node 5 (Leaf 6) (Leaf 7))
--   False
--
--   >>> isBST t1
--   False
--
--   >>> isBST t2
--   True
--
isAscending :: [Int] -> Bool
isAscending [leaf] = True
isAscending (leaf:leaf_1) = leaf <= head (leaf_1) && isAscending leaf_1

isBST :: Tree -> Bool
isBST = isAscending . inorder
-- | Check whether a number is contained in a binary search tree.
--   (You may assume that the given tree is a binary search tree.)
--
--   >>> inBST 2 (Node 5 (Leaf 2) (Leaf 7))
--   True
--
--   >>> inBST 3 (Node 5 (Leaf 2) (Leaf 7))
--   False
--
--   >>> inBST 4 t2
--   True
--
--   >>> inBST 10 t2
--   False
--
inBST :: Int -> Tree -> Bool
inBST leaf_to_find (Leaf l)
  |leaf_to_find == l = True
  |otherwise = False
inBST leaf_to_find (Node l t1 t2)
  |leaf_to_find == l = True
  |otherwise = (inBST l t1) || (inBST l t2)
