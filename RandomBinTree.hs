{-# LANGUAGE ScopedTypeVariables #-}
module RandomBinTree where

import Data.List
import System.Random
import Control.Monad.Random
import RandomList
import Data.Array.IO
import BinTree

{-Generates a binary tree with n vertices.
  In the tree the left subtree is allways the bigger one.
-}
genBinTree :: Int -> IO (BinTree Int)
genBinTree n = genRandList n >>= return . (dyckpathToTree 1) . genDyckpath

genBinTree2 :: Int -> IO (BinTree (Int, Int, Int))
genBinTree2 n = genRandList n >>= return . (dyckpathToTree2 1 n) . genDyckpath

{-Takes a Dyck Path and maps it on a binary tree with numbered nodes.
-}
dyckpathToTree :: Int -> [Int] -> BinTree Int
dyckpathToTree next path
	|path == []	= Empty
	|otherwise	= Node next (dyckpathToTree (next+1) left) (dyckpathToTree (next + 1 + sl) right)
	where	
		left 	= map (\x -> x-1) (tail(takeWhile (>0) path))
		right 	= tail (dropWhile (>0) path)
		sl		= div (length left) 2

{-
Takes a Dyck Path of length 2*n and maps it on a binary tree with n nodes named with a triple of Int.
The first Int enumerates the nodes, the second/third Int is the size of the left/right subtree.
For all nodes the left subtree is larger or equal than the right subtree.

-}
dyckpathToTree2 :: Int -> Int -> [Int] -> BinTree (Int,Int,Int)
dyckpathToTree2 next size path
	|path == []		= Empty
	|sizel == sl	= Node (next, sl, sr) (dyckpathToTree2 (next+1) sl  left) (dyckpathToTree2 (next + 1 + sl) sr right)
	|otherwise		= Node (next, sl, sr) (dyckpathToTree2 (next+1) sl  right) (dyckpathToTree2 (next + 1 + sl) sr left)
	where	
		left 	= map (\x -> x-1) (tail(takeWhile (>0) path))
		right 	= tail (dropWhile (>0) path)
		sizel	= div (length left) 2
		sl		= max sizel (size -1 - sizel)
		sr		= min sizel (size -1 - sizel)

{-Takes a list of (randomly chosen) integers that are pairwise disjunct and maps them onto a Dyck Path, that is represented as a list of integers.
  If the length of the Dyck Path is supposed to be 2n then the input list must have length n and may only consist of integers that are in [1..2n]
-}
genDyckpath :: [Int] -> [Int]
genDyckpath randNums = convDyckpath (genDyckpath' (length randNums) 1 0 randNums [])

genDyckpath' :: Int -> Int -> Int -> [Int] -> [Int] -> [Int]
genDyckpath' length counter partSum randNums xs
	|counter > 2*length			= reverse xs
	|randNums ==[]				= genDyckpath' length (counter+1) (partSum-1) randNums ((partSum-1):xs) 
	|counter ==	head randNums	= genDyckpath' length (counter+1) (partSum+1) (tail randNums) ((partSum+1):xs)
	|otherwise					= genDyckpath' length (counter+1) (partSum-1) randNums ((partSum-1):xs)

{-Takes a list of numbers and converts it into a Dyck Path.
  The list must have the following properties:
	- The difference between two consecutive numbers is always +1/-1
	- The last number of the list is 0
-}
convDyckpath :: [Int] -> [Int]
convDyckpath path 
	|all (>=0) path = path
	|otherwise	= fstPart ++  sndPart
				where 	
					minp	= minimum path
					posp	= map (\x -> x - minp) path
					fstPart = tail (dropWhile (>0) posp)
					sndPart = [-1 - minp] ++ map (\x -> x-1) (takeWhile (>0) posp)

bla = do
	putStrLn "Please enter the requested number (Int) of vertices of the binary tree: "
	(n :: Int) <- readLn
	tree <- genBinTree n
	putStrLn $ show tree
	putStrLn ""
	putStrLn "Type Y to generate another tree or press enter to exit."
	a <- getLine
	putStrLn ""
	if a=="Y"
		then bla
	else putStrLn "Exit"