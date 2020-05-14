module Main where

import Data.Array.Accelerate                              as A hiding (fromInteger, fromRational, fromIntegral)
import qualified Data.Array.Accelerate                    as A (fromInteger, fromRational, fromIntegral)
import Data.Array.Accelerate.Debug as D

import Data.Array.Accelerate.LLVM.Native                  as CPU
import Data.Array.Accelerate.LLVM.PTX                     as PTX

import qualified Prelude as P
import qualified Control.Monad as P
import Prelude as P (fromIntegral, fromInteger, fromRational, String, return, (>>=), (>>), IO, Maybe(..), maybe)

import System.Environment

import FourierTest
import QuickSortTest
import ReadFile

-- `setforceIrreg`  makes sure that regularity analysis isn't executed
-- Can be profiled with:
-- nvprof --profile-child-processes -u ms --trace gpu --continuous-sampling-interval 1 stack run -- quicksort version m n
-- OR
-- nvprof --profile-child-processes -u ms --trace gpu --continuous-sampling-interval 1 stack run -- fourier version m

-- The futhark version can be benchmarked with:
-- nvprof --profile-child-processes -u ms --trace gpu --continuous-sampling-interval 1 futhark bench -r 9 --skip-compilation --backend=cuda Futhark/quicksort.fut
-- OR
-- nvprof --profile-child-processes -u ms --trace gpu --continuous-sampling-interval 1 futhark bench -r 9 --skip-compilation --backend=cuda Futhark/fft-lib.fut
-- Sum of 10 runs, so divide by 10
main :: IO ()
main = do
    args <- getArgs
    if P.length args P.== 3 then do
        let algorithm = P.head args
        if algorithm P.== "fourier" then do
            let version = args P.!! 1
            let m = P.read $ args P.!! 2
            P.when (version P.== "Irregular") setforceIrreg
            mainFourier version m
        else
            P.putStrLn "Expected fourier if we have three arguments (stack run -- fourier version m)"

    else if P.length args P.== 4 then do
        let algorithm = P.head args
        if algorithm P.== "quicksort" then do
            let version = args P.!! 1
            let m = P.read $ args P.!! 2
            let n = P.read $ args P.!! 3
            P.when (version P.== "Irregular") setforceIrreg
            mainQuicksort version m n
        else
            P.putStrLn "Expected quicksort if we have three arguments (stack run -- quicksort version m n)"
    else
        P.putStrLn "Expected three or 4 arguments ('stack run -- quicksort m n' or 'stack run -- fourier version m')"

-- Runs a single quicksort benchmark 10 times.
mainQuicksort :: String -> Int -> Int -> IO ()
mainQuicksort version m n =
    if version P.== "Regular" P.|| version P.== "Irregular" then do
        let qsort = PTX.run1 quickSortVec
        
        qsort `P.seq` return ()

        input <- readFiles n m

        P.replicateM_ 10 (runTest qsort (readFiles n m))
    else
        P.putStrLn $ "Quicksort version '" P.++ version P.++ "' doesn't exists."


-- Runs a single fourier benchmark 10 times.
mainFourier :: String -> Int -> IO ()
mainFourier version m = do
    let fourierv   = P.lookup version fourierVersions

    case fourierv of
        Nothing -> P.putStrLn $ "Fourier version '" P.++ version P.++ "' doesn't exists."
        Just f  -> do
            let fourier    = PTX.run1 $ reals f
            fourier `P.seq` return ()
            input <- readFilesFourier m 32
            --
            P.replicateM_ 10 (runTest fourier (readFilesFourier m 32))

{-# NOINLINE runTest #-}
runTest :: (a -> b) -> IO a -> IO ()
runTest f arg = do
    P.putStrLn "Running test"
    arg' <- arg
    f arg' `P.seq` return ()


