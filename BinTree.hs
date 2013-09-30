module BinTree where

data BinTree a = Empty | Node a (BinTree a) (BinTree a) deriving (Eq, Show)


{-
input:
	- binary tree: BinTree (Int, Int, Int)
	- index of the node you want to find: Int
return:
	- "name" of the Node: (Int, Int, Int)
-}
findNode :: BinTree (Int, Int, Int) -> Int -> (Int, Int)
findNode (Node (x, sl, sr) l r) y 
	|x==y 			= (sl, sr)
	|y<sl+x+1		= findNode l y
	|otherwise		= findNode r y

{-
Returns V(T)
This function only makes sense if the nodes are kind of numbered.
-}
getNodes :: BinTree a -> (a -> Int) -> [Int]
getNodes Empty f = []
getNodes (Node n l r)  f= (f n):(getNodes l f) ++ (getNodes r f)

{-
Returns the root of a binary Tree
-}
getRoot :: BinTree (Int, Int, Int) -> Int
getRoot Empty = undefined
getRoot (Node (r,_,_) _ _) = r