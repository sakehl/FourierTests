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
import ReadFile
import System.Environment

main = main3

-- Run the full benchmark suite
main1 :: IO ()
main1 = do
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
main2 :: IO ()
main2 = do
    -- setforceIrreg
    args <- getArgs
    if P.length args P.== 2 then do
        let m = P.read $ P.head args
        let n = P.read $ args P.!! 1

        let qsort = PTX.run1 quickSortVec
        
        qsort `P.seq` return ()

        input <- readFiles n m

        P.replicateM_ 10 (runTest qsort (readFiles n m))
    else
        P.putStrLn "Expected two (or one) arguments (stack run -- m n)"


-- Runs a single fourier benchmark 10 times.
-- Uncomment `setforceIrreg` to not perform the regularity analysis
-- Can be profiled with:
-- nvprof --profile-child-processes -u ms --trace gpu --continuous-sampling-interval 1 stack run -- NameOfBench m
--
-- The futhark version can be benchmarked with:
-- FUTHARK_INCREMENTAL_FLATTENING=1 nvprof --profile-child-processes -u ms --trace gpu --continuous-sampling-interval 1 futhark bench -r 9 --skip-compilation --backend=cuda fft-lib.fut
-- Sum of 10 runs, so divide by 10
main3 :: IO ()
main3 = do
    -- setforceIrreg
    args <- getArgs
    if P.length args P.== 2 then do
        let version    = P.head args
        let m = P.read $ args P.!! 1
        let fourierv   = P.lookup version fourierVersions

        case fourierv of
            Nothing -> P.putStrLn $ "Fourier version '" P.++ version P.++ "' doesn't exists."
            Just f  -> do
                P.when (version P.== "Irregular") setforceIrreg
                let fourier    = PTX.run1 $ reals f
                fourier `P.seq` return ()
                input <- readFilesFourier m 32
                --
                P.replicateM_ 10 (runTest fourier (readFilesFourier m 32))
    else
        P.putStrLn "Expected two arguments (stack run -- fourierversion m)"

{-# NOINLINE runTest #-}
runTest :: (a -> b) -> IO a -> IO ()
runTest f arg = do
    P.putStrLn "Running test"
    arg' <- arg
    f arg' `P.seq` return ()


