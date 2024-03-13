module ProgActivity where

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

-- The foldTree function uses foldr, takes a function (insert) and an initial value (Lead) and then applies that function to each item in the list from the right, accumulating a result.
-- The zip function [0...] takes a list and combines it with an index list. In this case, it is combined with [0...].
-- Example: "ABCDEFGHIJ", we would get [(0, 'A'), (1, 'B'), (2, 'C'), ...].
foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf . zip [0..]

-- The insert function takes a tuple containing the index and value, as well as a tree, and returns a new tree with the inserted value.insert :: (Integer, a) -> Tree a -> Tree a
insert (_, x) Leaf = Node 0 Leaf x Leaf
insert (h, x) (Node height left value right)
    | getHeight left <= getHeight right = 
        let newLeft = insert (h + 1, x) left
        in Node (max (getHeight newLeft) (getHeight right) + 1) newLeft value right
    | otherwise = 
        let newRight = insert (h + 1, x) right
        in Node (max (getHeight left) (getHeight newRight) + 1) left value newRight

-- The getHeight function checks that each time a new value is inserted into the tree, the correct height is calculated to maintain the tree balance. 
getHeight :: Tree a -> Integer
getHeight Leaf = -1
getHeight (Node h _ _ _) = h

-- Test
main :: IO ()
main = do
    let tree = foldTree "ABCDEFGHIJ"
    print tree