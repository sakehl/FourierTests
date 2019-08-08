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

import Data.Array.Accelerate.LLVM.Native                            as CPU
import Data.Array.Accelerate.Interpreter                            as I
#ifdef ACCELERATE_LLVM_PTX_BACKEND
import qualified Data.Array.Accelerate.LLVM.PTX                     as GPU
#endif

import Debug.Trace
import Data.Array.Accelerate.Array.Sugar                            as S
import Data.Array.Accelerate.Error                                  as A
import Data.Array.Accelerate.Type                                   as A
import Data.Array.Accelerate.Array.Unique                           as A
import Data.Array.Accelerate.Array.Lifted                           as A
import Data.Array.Accelerate.Data.Complex                           as A
import Data.Array.Accelerate.Debug hiding (Mode)

import Data.Array.Accelerate.Math.FFT                   as FFT
-- import Data.Array.Accelerate.Math.FFT.Type                          as A
import Data.Array.Accelerate.Math.DFT.Centre                        as A
--import Data.Array.Accelerate.Math.FFT.Mode              as FFT
{-
import Data.Array.Accelerate.Type                                   as A
import Data.Array.Accelerate.Array.Data                             as A
-}
import Data.Array.Accelerate.LLVM.Native.Foreign

import Data.Ix                                                      ( Ix )
import Data.Array.CArray                                            ( CArray )
import qualified Data.Array.CArray                                  as C
import qualified Data.Array.CArray.Base                             as C

import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr
import System.IO.Unsafe (unsafePerformIO)
import Math.FFT                                                     as FFT
import Math.FFT.Base                                                as FFT
import Data.Bits

import Control.Lens --(lens, (_2))
import Criterion.Main
import Control.DeepSeq
import Control.Exception

import FFT
import GPUFFT
import FFTAdhoc

import qualified Data.Array.Accelerate.Math.FFT.LLVM.PTX as PTX

type MatrixVec e = Array DIM3 e

inputN :: Int -> MatrixVec (Complex Double)
inputN n = fromList (Z :. n :. 32 :. 32) [ (P.sin x :+ P.cos x) | x <- [0..] ]

input :: Matrix (Complex Double)
input = fromList (Z :. 32 :. 32) [ (P.sin x :+ P.cos x) | x <- [0..] ]

gpuTest :: Int -> MatrixVec (Complex Double)
gpuTest n = GPU.run1 (collect . tabulate . mapSeq (fft2DForGPU Forward) . toSeqOuter) (inputN n)

gpuTest2 ::Matrix (Complex Double)
gpuTest2 = GPU.run1 (fft2DForGPU Forward) input

gpuTest3 :: Int -> MatrixVec (Complex Double)
gpuTest3 n = GPU.run1 (fft2DVecW Forward) (inputN n)

gpuTest4 :: Int -> MatrixVec (Complex Double)
gpuTest4 n = GPU.run1 ( fft3DForGPU Forward) (inputN n)

gpuTest5 ::Matrix (Complex Double)
gpuTest5 = GPU.run1 (fft2DW Forward) input

gpuTest6 ::Matrix (Complex Double)
gpuTest6 = GPU.run1 (FFT.fft2D' Forward (Z:. 3 :. 2) ) input

gpuTest7 ::Matrix (Complex Double)
gpuTest7 = GPU.run1 (PTX.fft2DW Forward) input

fourierTransformSeq :: Acc (MatrixVec (Complex Double)) -> Acc (MatrixVec (Complex Double))
fourierTransformSeq = collect . tabulate . mapSeq (ditSplitRadixLoop Forward) . toSeqOuter

fourierTransformFor :: Acc (MatrixVec (Complex Double)) -> Acc (MatrixVec (Complex Double))
fourierTransformFor = collect . tabulate . mapSeq (myfft2DFor Forward) . toSeqOuter

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

                transform = ditSplitRadixLoop Forward theslice
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

myenv :: Bool -> (forall a b. (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b)
      -> (Acc (MatrixVec (Complex Double)) -> Acc (MatrixVec (Complex Double)))
      -> IO (MatrixVec (Complex Double), MatrixVec (Complex Double), MatrixVec (Complex Double), MatrixVec (Complex Double), MatrixVec (Complex Double)
            , MatrixVec (Complex Double) -> MatrixVec (Complex Double))
myenv reg run1_ f = do
    let inp1 = inputN 1
        inp10 = inputN 10
        inp100 = inputN 100
        inp1000 = inputN 1000
        inp10000 = inputN 10000
        runner = run1_ f
    
    P.putStrLn "Evaluating input"
    evaluate (rnf inp1)
    evaluate (rnf inp10)
    evaluate (rnf inp100)
    P.putStrLn "Compiling function"
    if reg then do P.putStrLn "Compiling regular"; clearforceIrreg;
           else do P.putStrLn "Compiling irregular"; setforceIrreg;
    evaluate runner
    P.putStrLn "Done with setup"
    return (inp1, inp10, inp100, inp1000, inp10000, runner)

tester :: IO ()
tester = do
    let benches' name ~(inp1, inp10, inp100, inp1000, inp10000, runner) = bgroup name [
                  bench "1"  $ nf runner inp1
                , bench "10" $ nf runner inp10
                , bench "100" $ nf runner inp100
                , bench "1000" $ nf runner inp1000
                , bench "10000" $ nf runner inp10000
                ]
        cpubenches name reg f = env (myenv reg CPU.run1 f) (benches' name)
#ifdef ACCELERATE_LLVM_PTX_BACKEND
        gpubenches name reg f = env (myenv reg GPU.run1 f) (benches' name)
#endif
    defaultMain [bgroup "CPU" [
          cpubenches "Regular"   True  fourierTransformSeq
        , cpubenches "Irregular" False fourierTransformSeq
        , cpubenches "Normal"    False fourierTransformNor
        , cpubenches "Foreign"   False fourierTransformFor
        ]
#ifdef ACCELERATE_LLVM_PTX_BACKEND
        ,
        bgroup "GPU" [
          gpubenches "Regular"   True  fourierTransformSeq
        , gpubenches "Irregular" False fourierTransformSeq
        , gpubenches "Normal"    False fourierTransformNor
        --, gpubenches "Foreign"   False fourierTransformFor
        ]
#endif
        ]