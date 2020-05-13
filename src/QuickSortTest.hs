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
import qualified Data.Array.Accelerate.Pattern as A

import Data.Array.Accelerate.LLVM.Native                            as CPU
import Data.Array.Accelerate.Interpreter                            as I
#ifdef ACCELERATE_LLVM_PTX_BACKEND
import qualified Data.Array.Accelerate.LLVM.PTX                     as GPU
#endif
import Data.Array.Accelerate.Debug hiding (Mode)
import qualified Data.Array.Accelerate.Array.Sugar                  as S

import Control.Lens hiding (use)
import Control.DeepSeq
import Control.Exception
import Data.Time.Clock.System
import Data.Time.Clock
import System.Environment


import Criterion.Main

import QuickSort
import ReadFile

isSort :: Vector Int -> Bool
isSort xs = isSortedBy (P.<=) . toList $ xs 
  where
    isSortedBy :: (a -> a -> Bool) -> [a] -> Bool
    isSortedBy lte = loop
        where
            loop []       = True
            loop [_]      = True
            loop (x:y:zs) = (x `lte` y) P.&& loop (y:zs)

inputN :: Int -> Matrix Int
inputN n = fromList (Z :. n :. 32) [P.floor (100 * P.cos x) | x <- [0..] ]

inputNAcc :: Acc (Scalar Int) -> Acc (Matrix Int)
inputNAcc n = generate (index2 (the n) 100) (\(unlift .unindex2->(i :: Exp Int,j :: Exp Int)) -> let k = A.fromIntegral (i*32+j) :: Exp Double
                                                                                                in floor $ 100 * cos k)

quickSortVec :: Acc (Matrix Int) -> Acc (Matrix Int)
quickSortVec = collect . tabulate . mapSeq (afst . quicksort) . toSeq2ndInner

quickSortVecB :: Acc (Matrix Int) -> Acc (Matrix Bool)
quickSortVecB = collect . tabulate . mapSeq (asnd . quicksort) . toSeq2ndInner

quickSortNor :: Acc (Matrix Int) -> Acc (Matrix Int)
quickSortNor xss = result
    where
        n = indexHead . indexTrans . A.shape $ xss
        sh = indexTail . indexTrans . A.shape $ xss
        initsh = indexTrans $ lift (sh :. constant 0)
        onesh  = indexTrans $ lift (sh :. constant 1)
        
        result = afor n step init

        init :: Acc (Matrix Int)
        init = fill initsh (constant 0)

        step :: Exp Int -> Acc (Matrix Int) -> Acc (Matrix Int)
        step n a = 
            let slix = lift (Z:.n:.All)
                theslice = slice xss slix

                transform = afst . quicksort $ theslice
                reshaped = reshape onesh transform

                res = transpose $ transpose a ++ transpose reshaped
            in res

        afor :: forall a. Arrays a => Exp Int -> (Exp Int -> Acc a -> Acc a) -> Acc a -> Acc a
        afor n f m = let
            newf :: Acc (Scalar Int, a) -> Acc (Scalar Int, a)
            newf (unlift -> (i, m) :: (Acc (Scalar Int), Acc a)) = 
                let i' = map (+1) i 
                in lift (i', f (the i) m)

            condition :: Acc (Scalar Int, a) -> Acc (Scalar Bool)
            condition (unlift -> (i, m) :: (Acc (Scalar Int), Acc a)) = map (<n) i

            initial :: Acc (Scalar Int, a)
            initial = lift (unit 0, m)
            in asnd $ awhile condition newf initial