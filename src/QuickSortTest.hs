{-# LANGUAGE CPP                 #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# language FlexibleContexts    #-}
{-# language ViewPatterns        #-}
{-# language RankNTypes        #-}


module QuickSortTest where

import qualified Prelude as P
import Prelude as P (fromIntegral, fromInteger, fromRational, String, return, (>>=), (>>), IO, Maybe(..), maybe, (=<<))
import Data.Array.Accelerate                                        as A hiding (fromInteger, fromRational, fromIntegral)
import qualified Data.Array.Accelerate                              as A (fromInteger, fromRational, fromIntegral)

import Data.Array.Accelerate.LLVM.Native                            as CPU
import Data.Array.Accelerate.Interpreter                            as I
#ifdef ACCELERATE_LLVM_PTX_BACKEND
import qualified Data.Array.Accelerate.LLVM.PTX                     as GPU
#endif
import Data.Array.Accelerate.Debug hiding (Mode)
import qualified Data.Array.Accelerate.Array.Sugar                  as S

import Control.DeepSeq
import Control.Exception
import Data.Time.Clock.System
import Data.Time.Clock

import QuickSort

inputN :: Int -> Matrix (Int)
inputN n = fromList (Z :. n :. 32) [P.floor (100 * P.cos x) | x <- [0..] ]

inputN2 :: Int -> [Vector Int]
inputN2 n = P.replicate n (fromList (Z :. 1024) [P.floor (100 * P.cos x) | x <- [0..] ])

inputNAcc :: Acc (Scalar Int) -> Acc (Matrix Int)
inputNAcc n = generate (index2 (the n) 32) (\(unlift .unindex2->(i :: Exp Int,j :: Exp Int)) -> let k = A.fromIntegral (i*32+j) :: Exp Double
                                                                                                in floor $ 100 * cos k)

quickSortVec :: Acc (Matrix Int) -> Acc (Matrix Int)
quickSortVec = collect . tabulate . mapSeq (afst . quicksort) . toSeqOuter

quickSortVecB :: Acc (Matrix Int) -> Acc (Matrix Bool)
quickSortVecB = collect . tabulate . mapSeq (asnd . quicksort) . toSeqOuter

tester :: Acc (Matrix Int)
tester = collect . tabulate . mapSeq (afst . quicksort) . streamIn $ inputN2 2

testerB :: Acc (Matrix Bool)
testerB = collect . tabulate . mapSeq (asnd . quicksort) . streamIn $ inputN2 2

time_ :: IO a -> IO String
time_ a = do t1 <- getSystemTime
             a
             t2 <- getSystemTime
             return $ P.show (diffUTCTime (systemToUTCTime t2) (systemToUTCTime t1) )

myenv2 :: (Arrays a, Shape sh, Elt e, b ~ Array sh e)
       => Maybe Bool -> Int
       -> (forall a b. (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b)
       -> (Acc (Scalar Int) -> Acc a)
       -> (Acc a -> Acc b)
       -> IO (Scalar Int, Scalar Int -> b)
myenv2 reg a run1_ inputNAcc fun = do
    let inpa = fromList Z [a]
        -- inpa = inputN a
        runner = run1_ (fun . inputNAcc)
    
    
    P.putStrLn "Compiling function"
    case reg of
        Nothing    -> return ()
        Just True  -> do P.putStrLn "Compiling regular"; clearforceIrreg;
        Just False -> do P.putStrLn "Compiling irregular"; setforceIrreg;
    time_ (evaluate runner) >>= (\x -> P.putStrLn ("Compiling took " P.++ x))
    P.putStrLn "Evaluating input"
    -- evaluate (rnf inp1)
    -- evaluate (rnf inp10)
    -- evaluate (rnf inp100)
    -- evaluate (rnf inp1000)
    -- evaluate (rnf inp10000)
    -- evaluate (rnf inp100000)
    let testx = indexArray (runner inpa) S.empty
    time_ (P.putStrLn (P.show testx)) >>= (\x -> P.putStrLn ("Evaluation took " P.++ x))
    P.putStrLn "Done with setup"
    return (inpa, runner)