module RandomList where

import Data.List
import System.Random (getStdGen)
import Control.Monad
import Control.Monad.State
import Control.Monad.Random
import Data.Array.IO

{- 
Generating a sorted list of numbers in seq 1..20 can be done like this:
Generate "deltas" of the values so they sum up to 19.
E.g.:	[  3 ,  4 ,  6 ,  8 ,  9 , 10 , 11 , 13 , 15 , 17 ]
		0  +3   +1   +2   +2   +1   +1   +1   +2   +2   +2   +4    21
Since we don't want to get the same value multiple times, each increment has
to be at least +1. The total increment will be 21 (2*n+1) and we have 11 (n+1)
such increments. So we could see this as: Start out with all "+1" in an array,
distribute the missing 10 ( (2*n+1)-(n+1) = n ) increments randomly among the
array elements.
We use arrays instead of lists because editing an element in a list is O(n),
where as it only takes constant time to do this in arrays.
-}

genRandList :: Int -> IO [Int]
genRandList n = do
        -- Borrow the standard RNG
        g <- getStdGen
        (arr, g') <- runRandT distGen g
        setStdGen g'
        -- Convert array as list
        liftM (take n) $ getElems arr
    where
        distGen :: RandomGen g => RandT g IO (IOArray Int Int)
        distGen = do
            -- Start out with an array of all "+1"
            arr <- lift $ newArray (0,n) 1
            -- Distribute n tokens on the array
            replicateM n (putToken arr)
            -- Sum the array
            sumList 0 0 arr
            return arr

        putToken :: RandomGen g => (IOArray Int Int) -> RandT g IO ()
        putToken arr = do
            -- Select a slot for our token
            i <- getRandomR (0, n) -- (0,n) == getBounds arr
            -- Increment the nth slot
            v <- lift $ readArray arr i
            lift $ writeArray arr i (v+1)

        sumList :: RandomGen g => Int -> Int -> (IOArray Int Int) -> RandT g IO ()
        sumList i s arr | i > n = return ()
        sumList i s arr = do
            -- read value at i
            x <- lift $ readArray arr i
            --lift $ putStrLn $ "i = " ++ show i ++ ", x = " ++ show x ++ ", s = " ++ show s
            -- add it to the running variable s
            s' <- return (s+x)
            -- store the new value
            lift $ writeArray arr i s'
            -- continue with next index
            sumList (i+1) s' arr
