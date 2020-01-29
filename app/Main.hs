module Main where

import FourierTest
import QuickSortTest

import Data.Array.Accelerate                              as A hiding (fromInteger, fromRational, fromIntegral)
import qualified Data.Array.Accelerate                    as A (fromInteger, fromRational, fromIntegral)

import Data.Array.Accelerate.LLVM.Native                  as CPU
import Data.Array.Accelerate.LLVM.PTX                     as PTX
import Data.Array.Accelerate.Interpreter                  as I

import qualified Prelude as P
import qualified Control.Monad as P
import Prelude as P (fromIntegral, fromInteger, fromRational, String, return, (>>=), (>>), IO, Maybe(..), maybe)

import Debug.Trace
import Data.Array.Accelerate.Debug as D

import Criterion.Main

import QuickSort
import System.Environment

-- Run the full benchmark suite
main :: IO ()
main = do
    PTX.registerPinnedAllocator
    defaultMain [bgroup "Fourier" FourierTest.tester, bgroup "QuickSort" QuickSortTest.tester]

-- Runs a single quicksort benchmark 10 times.
-- Uncomment `setforceIrreg` to not perform the regularity analysis
-- Can be profiled with:
-- nvprof --profile-child-processes -u ms --trace gpu --continuous-sampling-interval 1 stack run -- m n
--
-- The futhark version can be benchmarked with:
-- nvprof --profile-child-processes -u ms --trace gpu --continuous-sampling-interval 1 ../../futhark bench -r 9 --skip-compilation --backend=cuda quicksort.fut
-- Sum of 10 runs, so divide by 10
main' :: IO ()
main' = do
    -- setforceIrreg
    args <- getArgs
    if P.length args P./= 2 then
        P.putStrLn "Expected two arguments (stack run -- m n)"
    else do
        let m = P.read $ P.head args
        let n = P.read $ args P.!! 1

        let qsort = PTX.run1 quickSortVec
        qsort `P.seq` return ()

        input <- readFiles n m

        P.replicateM_ 10 (runTest qsort (return input))

{-# NOINLINE runTest #-}
runTest :: (a -> b) -> IO a -> IO ()
runTest f arg = do
    P.putStrLn "Running test"
    arg' <- arg
    f arg' `P.seq` return ()


