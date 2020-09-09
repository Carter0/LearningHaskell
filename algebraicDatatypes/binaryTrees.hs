module BinaryTrees where

data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right) | b == a = Node left a right
                              | b < a  = Node (insert' b left) a right
                              | b > a  = Node left a (insert' b right)

-- good job carter!
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf                = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)


testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)



-- also good job carter.
-- think of how it goes down the tree (unravels), then goes back up.
preorder :: BinaryTree a -> [a]
preorder Leaf                = []
preorder (Node left a right) = a : (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf                = []
inorder (Node left a right) = (preorder left) ++ [a] ++ (preorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf                = []
postorder (Node left a right) = (preorder left) ++ (preorder right) ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

-- preorder is [2, 1, 3]
-- postorder is [1 3 2]
-- in order is [1 2 3]

-- I needed some help with this one. 
-- Strange wrapping my head around it.
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ base Leaf = base
foldTree f base (Node left a right) =
    (foldTree f (f a (foldTree f base left)) right)
