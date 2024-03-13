# <p align="center">Programming Activity
</p>
In a binary tree, the height is the length of the longest path from the root to a leaf node.  For example, if there is only one node, the height is 0. If there are three nodes and the root has two children, the height is 1. A binary tree is balanced if the heights of its left and right subtrees differ by at most 1, and both subtrees are also balanced.
To represent binary trees, each node must store an additional integer indicating its height.


                                                        data Tree a = Leaf
		                                                       | Node Integer (Tree a) a (Tree a)
                                                        deriving (Show, Eq)

### Write a function that generates a balanced binary tree from a list of values using foldr

	                                                    foldTree :: [a] -> Tree a
	                                                    foldTree = …

Your solution may not place the nodes in exactly the same order, but it should result in balanced trees, with each subtree having a correct calculated height.

                                                            foldTree "ABCDEFGHIJ" ==
                                                                Node 3
                                                                (Node 2
                                                                    (Node 0 Leaf ’F’ Leaf)
                                                                    ’I’
                                                                    (Node 1 (Node 0 Leaf ’B’ Leaf) ’C’ Leaf))
                                                                ’J’
                                                                (Node 2
                                                                    (Node 1 (Node 0 Leaf ’A’ Leaf) ’G’ Leaf)
                                                                ’H’
                                                                (Node 1 (Node 0 Leaf ’D’ Leaf) ’E’ Leaf))
