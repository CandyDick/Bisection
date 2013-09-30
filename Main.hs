{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import RandomBinTree
import BinTree
import Control.Monad
import Data.Ord
import Data.List

{-
Computes a bisection of a binary tree. 
The nodes are numbered and have the size of the left/right subtree saved in their "names".
For all nodes the size of the left subtree has to be larger or equal than the size of the right subtree.
Input:
	- binary tree: BinTree (Int, Int, Int)
	- treesize: Int
	- current size of the set L: Int
	- list of lists of nodes: [[Int]]
	- tuple: (Int, [Int])
		- first element: number of calls of findNice
		- second element: the set L
return:
	-triple: (Int, Int, [Int])
		- first element: number of calls of findNice
		- second element: wide of the bisection
		- third element: set L
-}
binTreeBi :: BinTree (Int, Int, Int) -> Int -> Int -> [[Int]] -> (Int, [Int]) -> (Int, Int, [Int])
binTreeBi t n sizeL sep (counter, l)	
	|t==Empty	= (counter, wide, l2)
	|sep==[]	= binTreeBi  t2 n (sizeL + (length tlarge)) [[s]] (counter+1, l++tlarge)
	where
		(wide,l2) = splitSep l ((div n 2) -sizeL) sep' 0
		sep' = reverse $ sortBy (comparing length) sep	
		(s, tlarge, t2) = findNice t n sizeL
binTreeBi t n sizeL (ss:sep) (counter, l)
	|s==r		= binTreeBi  t2 n (sizeL + (length tlarge)) ((s:ss):sep) (counter+1, l++tlarge)
	|otherwise	= binTreeBi  t2 n (sizeL + (length tlarge)) ([s]:ss:sep) (counter+1, l++tlarge)
	where	
		(s, tlarge, t2) = findNice t n sizeL
		r = getRoot t

{-
Same as binTreeBi except that it returns a list of integers instead of the set L.
The integers represent the size the sets that were put into L have.
E.g.: the parts that were put into L had size 300, 49, 36, 15, 0 then the returned list looks like this: [0,15,36,49,300]
-}
binTreeBi2 :: BinTree (Int, Int, Int) -> Int -> Int -> [[Int]] -> (Int, [Int]) -> (Int, Int, [Int])
binTreeBi2 t n sizeL sep (counter, l)	
	|b1			= (counter, wide, l)
	|sep==[]	= binTreeBi2  t2 n (sizeL + tlarge) [[s]] (counter+1, tlarge:l)
	where
		b1 = t==Empty || sizeL == (div n 2)
		(wide,_) = splitSep l ((div n 2) -sizeL) sep' 0
		sep' = reverse $ sortBy (comparing length) sep
		(s, tlarge, t2, b2) = findNice2 t n sizeL
binTreeBi2 t n sizeL (ss:sep) (counter, l)		
	|s==r && b2		= binTreeBi2  t2 n (sizeL + tlarge) ((s:ss):sep) (counter+1, tlarge:l)
	|not b2			= (counter+1,  wide, tlarge:l)
	|otherwise		= binTreeBi2  t2 n (sizeL + tlarge) ([s]:ss:sep) (counter+1, tlarge:l)
	where	
		(s, tlarge, t2, b2) = findNice2 t n sizeL
		r = getRoot t
		wide = 2 + (length $ concat sep)

		

{-
Takes the nodes in the seperatorlist and assigns them to L or R.
input:
	- set L: [Int]
	- number of nodes that still "fit" in L: Int
	- seperatorlist: [[Int]]
	- current wide of the bisection
return:
	-tuple: (Int, [Int])
		- first element: wide of the bisection
		- second element: set L
-}
splitSep :: [Int] -> Int -> [[Int]] -> Int -> (Int,[Int])
splitSep l 0 sep wide = (wide + (length $ concat sep),l)
splitSep l n (s:sep) wide
	|n >= ls	= splitSep (s++l) (n - ls) sep (wide+2)
	|otherwise	= splitSep ((take n s) ++ l) 0 sep (wide+2+ls-n)
	where
		ls = length s

{-
Tests wether the binary tree can be bisected with wide 2.
input:
	- binary Tree: BinTree (Int, Int, Int)
	- counter first loop: Int
	- counter second loop:Int
	- tree size: Int
output:
	- True if there is a bisection with wide 2, else False :Bool
-}
bruteforceBi :: BinTree (Int, Int, Int) -> Int -> Int -> Int -> Bool
bruteforceBi Empty _ _ _ = False
bruteforceBi t k l n
	|or [b1,b2,b3,b4]	= True
	|k==l && k==2*n		= False
	|l==2*n				= bruteforceBi t (k+1) (k+1) n
	|otherwise			= bruteforceBi t k (l+1) n
	where
		(slk, srk) = findNode t k
		(sll, srl) = findNode t l
		b1 = (slk+srl) == n
		b2 = (slk+sll) == n
		b3 = (srk+srl) == n
		b4 = (srk+sll) == n


{-
Finds a "nice" node in a binary tree. The tree might be a subtree of a bigger tree T.
input:
	- binary tree: BinTree (Int, Int, Int)
	- size of T: Int
	- size of the set L: Int
return:
	- triple: (Int, [Int], BinTree (Int,Int,Int))
		- first element: "name" of the "nice" node
		- second element: list of nodes which will be joined to L
		- third element: right subtree of the "nice" node
-}
findNice :: BinTree (Int, Int, Int) -> Int -> Int -> (Int, [Int], BinTree (Int,Int,Int))
findNice Empty _ _ =  undefined
findNice (Node (k, sl, sr) l r) n  sizeL 
	|sizeL+sl<= div n 2		= (k, getNodes  l fst3, r)
	|otherwise				= findNice l n sizeL

findNice2 :: BinTree (Int, Int, Int) -> Int -> Int -> (Int, Int, BinTree (Int,Int,Int), Bool)
findNice2 Empty _ _ =  undefined
findNice2 (Node (k, sl, sr) l r) n  sizeL 
	|sizeL+sl+sr==div n 2	= (k, sl + sr, Empty, False)
	|sizeL+sl<= div n 2		= (k, sl, r, True)
	|otherwise				= findNice2 l n sizeL
{-
Functions for getting one of the elements of a triple.
-}
fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x

thd3 :: (a,b,c) -> c
thd3 (_,_,x) = x





test = do
	putStrLn "Please enter the requested number (Int) of vertices of the binary tree: "
	(n :: Int) <- readLn
	tree <- genBinTree2 n
	putStr "Number of times findNice is used: "
	putStrLn $ show $ fst3(binTreeBi tree n 0 [] (0,[]))
	putStr "Wide of the bisection: "
	putStrLn $ show $ snd3(binTreeBi tree n 0 [] (0,[]))
	putStrLn ""
	when (snd3(binTreeBi tree n 0 [] (0,[])) == 3) (putStrLn $ "Minimal bisection is 2: " ++ (show $ bruteforceBi tree 1 1 (div n 2)))
	putStrLn ""
	putStrLn "Type Y to generate another tree or press enter to exit."
	a <- getLine
	putStrLn ""
	if a=="Y"
		then test
	else putStrLn "Exit"
	
test2 = do
	tree <- genBinTree2 2000
	putStr $ show $ fst3(binTreeBi2 tree 2000 0 [] (0,[]))
	putStr " , "
	putStrLn $ show $ snd3(binTreeBi2 tree 2000 0 [] (0,[]))
	-- putStrLn $ "Minimal bisection is 2: " ++ (show $ bruteforceBi tree 1 1 1000)
	-- putStrLn ""
	
test3 = do
	replicateM 50 test2
	
test4 = do
	tree <- genBinTree2 10000
	putStrLn $ "Minimal bisection is 2: " ++ (show $ bruteforceBi tree 1 1 5000)

test5 n = do 
	tree <- genBinTree2 n
	return $ fst3 (binTreeBi2 tree n 0 [] (0,[]))

test6 n = replicateM 50 (test5 n)

test7 n = do
	l <- test6 n
	return $ map help (group $ sort l)
	where help xs = (head xs , length xs) 

main = do
	-- putStrLn "This program will generate 50 binary trees of the requested size and compute the wide of the bisections."
	-- putStrLn "Please enter the requested number (Int) of vertices: "
	-- (n :: Int) <- readLn
	l <- test6 100000
	-- putStrLn "50 trees were generated and bisected."
	-- putStrLn "The following list shows how often which wide occurred."
	putStrLn $ show $ map help (group $ sort l)
	-- putStrLn ""
	-- putStrLn "Type Y to generate another 50 trees or press enter to exit."
	-- a <- getLine
	-- putStrLn ""
	-- if a=="Y"
		-- then main
	-- else putStrLn "Exit"
	where help xs = (head xs , length xs) 