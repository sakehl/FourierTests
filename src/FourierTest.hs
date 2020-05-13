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


module FourierTest where

import qualified Prelude as P
import Prelude as P (fromIntegral, fromInteger, fromRational, String, return, (>>=), (>>), IO, Maybe(..), maybe, (=<<))
import Data.Array.Accelerate                                        as A hiding (fromInteger, fromRational, fromIntegral)
import qualified Data.Array.Accelerate                              as A (fromInteger, fromRational, fromIntegral)
import Data.Array.Accelerate.Data.Complex                           as A

import Data.Array.Accelerate.Math.FFT                   as FFT
import Control.Lens hiding (use)

-- import FFT
-- import GPUFFT
import FFTAdhoc


type MatrixVec e = Array DIM3 e

reals :: (Acc (MatrixVec (Complex Double)) -> Acc (MatrixVec (Complex Double))) -> Acc (MatrixVec Double) -> Acc (MatrixVec Double)
reals f x = let x' = map (lift . (:+ 0) ) x
                y  = f x'
                y' = map real y
            in y'

fourierVersions :: [(P.String, Acc (MatrixVec (Complex Double)) -> Acc (MatrixVec (Complex Double)))]
fourierVersions = [ ("Regular", fourierTransformSeq)
                  , ("Irregular", fourierTransformSeq)
                  , ("cuFFT", fourierTransformSelfLiftFor)
                  , ("Normal", fourierTransformNor)
                  ]


fourierTransformSeq :: Acc (MatrixVec (Complex Double)) -> Acc (MatrixVec (Complex Double))
fourierTransformSeq = collect . tabulate . mapSeq (FFTAdhoc.fft2D Forward) . toSeqOuter

fourierTransformSelfLiftFor :: Acc (MatrixVec (Complex Double)) -> Acc (MatrixVec (Complex Double))
fourierTransformSelfLiftFor = fft2DRegular Forward

fourierTransformForGPU :: Acc (MatrixVec (Complex Double)) -> Acc (MatrixVec (Complex Double))
fourierTransformForGPU = collect . tabulate . mapSeq (fft2DFor Forward) . toSeqOuter

fourierTransformNor :: Acc (MatrixVec (Complex Double)) -> Acc (MatrixVec (Complex Double))
fourierTransformNor xss = result
    where
        n = indexHead . indexTrans . A.shape $ xss
        sh = indexTail . indexTrans . A.shape $ xss
        initsh = indexTrans $ lift (sh :. constant 0)
        onesh  = indexTrans $ lift (sh :. constant 1)
        
        result = afor n step init

        init :: Acc (MatrixVec (Complex Double))
        init = fill initsh (constant 0)

        step :: Exp Int -> Acc (MatrixVec (Complex Double)) -> Acc (MatrixVec (Complex Double))
        step n a = 
            let slix = lift (Z:.n:.All:.All)
                theslice = slice xss slix

                transform = A.transpose . ditSplitRadixLoop Forward >-> A.transpose . ditSplitRadixLoop Forward $  theslice
                reshaped = reshape onesh transform

                res = concatOn _3 a reshaped
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