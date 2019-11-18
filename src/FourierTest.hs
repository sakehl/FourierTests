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

import Control.Lens hiding (use)
import Criterion.Main
import Data.Time.Clock.System
import Data.Time.Clock
import Control.DeepSeq
import Control.Exception

import FFT
import GPUFFT
import FFTAdhoc

import qualified Data.Array.Accelerate.Math.FFT.LLVM.PTX as PTX

type MatrixVec e = Array DIM3 e

inputN :: Int -> MatrixVec (Complex Double)
inputN n = fromList (Z :. n :. 32 :. 32) [ (P.sin x :+ P.cos x) | x <- [0..] ]

inputNAcc :: Acc (Scalar Int) -> Acc (MatrixVec (Complex Double))
inputNAcc (the -> n) = compute $ generate (index3 n 32 32) (\sh -> let x = A.fromIntegral $ 1 + indexHead sh              :: Exp Double
                                                                       y = A.fromIntegral $1 + (indexHead . indexTail) sh :: Exp Double
                                                                   in  lift $ x :+ x )

input :: Matrix (Complex Double)
input = fromList (Z :. 32 :. 32) [ (P.sin x :+ P.cos x) | x <- [0..] ]

-- gpuTest :: Int -> MatrixVec (Complex Double)
-- gpuTest n = GPU.run1 (collect . tabulate . mapSeq (PTX.fft2DForGPU Forward) . toSeqOuter) (inputN n)

-- gpuTest2 ::Matrix (Complex Double)
-- gpuTest2 = GPU.run1 (PTX.fft2DForGPU Forward) input

-- gpuTest7 ::Matrix (Complex Double)
-- gpuTest7 = GPU.run1 (PTX.fft2DW Forward) input

gpuTest :: Int -> Acc (MatrixVec (Complex Double))
gpuTest n = let
    n' = unit . lift $ n
    dat = inputNAcc n'
    fft1 = fourierTransformSeq dat
    fft2 = fourierTransformSelfLift dat
  in zipWith (-) fft1 fft2

gpuTest2 :: Int -> Acc (MatrixVec (Complex Double))
gpuTest2 n = let
    n' = unit . lift $ n
    dat = inputNAcc n'
    fft1 = fourierTransformForGPU dat
    fft2 = fourierTransformSelfLift dat
  in zipWith (-) fft1 fft2

gpuTest3 :: Int -> Acc (MatrixVec (Complex Double))
gpuTest3 n = let
    n' = unit . lift $ n
    dat = inputNAcc n'
    fft1 = fourierTransformSelfLiftFor dat
    fft2 = fourierTransformSelfLift dat
  in zipWith (-) fft1 fft2

fourierTransformSeq :: Acc (MatrixVec (Complex Double)) -> Acc (MatrixVec (Complex Double))
fourierTransformSeq = collect . tabulate . mapSeq (FFTAdhoc.fft2D Forward) . toSeqOuter

fourierTransformSelfLift :: Acc (MatrixVec (Complex Double)) -> Acc (MatrixVec (Complex Double))
fourierTransformSelfLift = FFTAdhoc.fft2DV Forward

fourierTransformSelfLiftFor :: Acc (MatrixVec (Complex Double)) -> Acc (MatrixVec (Complex Double))
fourierTransformSelfLiftFor = fft2DRegular Forward

fourierTransformFor :: Acc (MatrixVec (Complex Double)) -> Acc (MatrixVec (Complex Double))
fourierTransformFor = collect . tabulate . mapSeq (fft2DFor Forward) . toSeqOuter

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

time_ :: IO a -> IO String
time_ a = do t1 <- getSystemTime
             a
             t2 <- getSystemTime
             return $ P.show (diffUTCTime (systemToUTCTime t2) (systemToUTCTime t1) )

myenv :: Maybe Bool -> (Int, Int, Int, Int, Int, Int)
      -> (forall a b. (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b)
      -> (Acc (MatrixVec (Complex Double)) -> Acc (MatrixVec (Complex Double)))
      -> IO (MatrixVec (Complex Double), MatrixVec (Complex Double), MatrixVec (Complex Double), MatrixVec (Complex Double), MatrixVec (Complex Double), MatrixVec (Complex Double)
            , MatrixVec (Complex Double) -> MatrixVec (Complex Double))
myenv reg (a,b,c,d,e,f) run1_ fun = do
    let inpa = inputN a
        inpb = inputN b
        inpc = inputN c
        inpd = inputN d
        inpe = inputN e
        inpf = inputN f
        runner = run1_ fun
        runinp = run1_ $ map (+0) 
    
    P.putStrLn "Evaluating input"
    -- evaluate (rnf inp1)
    -- evaluate (rnf inp10)
    -- evaluate (rnf inp100)
    -- evaluate (rnf inp1000)
    -- evaluate (rnf inp10000)
    -- evaluate (rnf inp100000)
    evaluate (runinp inpa)
    evaluate (runinp inpb)
    evaluate (runinp inpc)
    evaluate (runinp inpd)
    evaluate (runinp inpe)
    evaluate (runinp inpf)
    P.putStrLn "Compiling function"
    case reg of
        Nothing    -> return ()
        Just True  -> do P.putStrLn "Compiling regular"; clearforceIrreg;
        Just False -> do P.putStrLn "Compiling irregular"; setforceIrreg;
    time_ (evaluate runner) >>= (\x -> P.putStrLn ("Compiling took " P.++ x))
    P.putStrLn "Done with setup"
    return (inpa, inpb, inpc, inpd, inpe, inpf, runner)

myenv2 :: Maybe Bool -> Int
       -> (forall a b. (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b)
       -> (Acc (MatrixVec (Complex Double)) -> Acc (MatrixVec (Complex Double)))
       -> IO (Scalar Int, Scalar Int -> MatrixVec (Complex Double))
myenv2 reg a run1_ fun = do
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
    let testx = indexArray  (runner inpa) (Z :. 0 :. 0 :. 0)
    P.putStrLn (P.show testx)
    P.putStrLn "Done with setup"
    return (inpa, runner)

myenv3 :: Maybe Bool -> (Int, Int, Int, Int, Int, Int)
       -> (forall a b. (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b)
       -> (Acc (MatrixVec (Complex Double)) -> Acc (MatrixVec (Complex Double)))
       -> IO (Scalar Int, Scalar Int, Scalar Int, Scalar Int, Scalar Int, Scalar Int
             , Scalar Int -> MatrixVec (Complex Double))
myenv3 reg (a,b,c,d,e,f) run1_ fun = do
    let inpa = fromList Z [a]
        inpb = fromList Z [b]
        inpc = fromList Z [c]
        inpd = fromList Z [d]
        inpe = fromList Z [e]
        inpf = fromList Z [f]
        runner = run1_ (fun . inputNAcc)
    
    
    P.putStrLn "Compiling function"
    case reg of
        Nothing    -> return ()
        Just True  -> do P.putStrLn "Compiling regular"; clearforceIrreg;
        Just False -> do P.putStrLn "Compiling irregular"; setforceIrreg;
    time_ (evaluate runner) >>= (\x -> P.putStrLn ("Compiling took " P.++ x))

    P.putStrLn "Evaluating input"
    --Everything goes to a max of C.
    evaluate (runner inpc)
    
    P.putStrLn "Done with setup"
    return (inpa, inpb, inpc, inpd, inpe, inpf, runner)

tester :: IO ()
tester = do
    let 
        benches' name (a,b,c,d,e,f) ~(inpa, inpb, inpc, inpd, inpe, inpf, runner) = bgroup name [
                  bench (P.show a) $ nf runner inpa
                , bench (P.show b) $ nf runner inpb
                , bench (P.show c) $ nf runner inpc
                , bench (P.show d) $ nf runner inpd
                , bench (P.show e) $ nf runner inpe
                , bench (P.show f) $ nf runner inpf
                ]
        benchesNor' name (a,b,c,d,e,f) ~(inpa, inpb, inpc, inpd, inpe, inpf, runner) = bgroup name [
                  bench (P.show a) $ nf runner inpa
                , bench (P.show b) $ nf runner inpb
                , bench (P.show c) $ nf runner inpc
                ]
        benches'' :: (forall a b. (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b) -> [Int] -> String -> Maybe Bool -> (Acc (MatrixVec (Complex Double)) -> Acc (MatrixVec (Complex Double))) -> Benchmark
        benches'' run1 xs name reg f =
            let 
                bench1 x = env (myenv2 reg x run1 f) $ \(~(inpx, runner)) -> bench (P.show x) $ nf runner inpx
            in bgroup name $ P.map bench1 xs
        
        cpunums1 = [1,100,1000,2000,5000,10000]
        gpunums1 = [1,100,1000,     5000,10000,20000]
        normbench = [1,100,1000]
        -- cpunums2 = (1,100,1000,2000,5000,10000)
        -- gpunums2 = (1,100,1000     ,5000,10000,20000)
        -- cpubenches name reg f = env (myenv3 reg cpunums2 CPU.run1 f) (benches' name cpunums2)
        -- cpubenchesNor name reg f = env (myenv3 reg cpunums2 CPU.run1 f) (benchesNor' name cpunums2)
        cpubenches name reg f = benches'' CPU.run1 cpunums1 name reg f
        cpubenchesNor name reg f = benches'' CPU.run1 normbench name reg f
#ifdef ACCELERATE_LLVM_PTX_BACKEND
        -- gpubenches name reg f = env (myenv3 reg gpunums2 GPU.run1 f) (benches' name gpunums2)
        -- gpubenchesNor name reg f = env (myenv3 reg gpunums2 GPU.run1 f) (benchesNor' name gpunums2)
        gpubenches name reg f = benches'' GPU.run1 gpunums1 name reg f
        gpubenchesNor name reg f = benches'' GPU.run1 normbench name reg f
#endif
    defaultMain [bgroup "CPU" [
          cpubenches "Regular"   (Just True)  fourierTransformSeq
        , cpubenches "Irregular" (Just False) fourierTransformSeq
        , cpubenches "Foreign"   Nothing fourierTransformFor
        , cpubenches "Lifted"   Nothing fourierTransformSelfLift
        , cpubenches "LiftedForeign"   Nothing fourierTransformSelfLiftFor
        , cpubenchesNor "Normal"    Nothing fourierTransformNor
        ]
#ifdef ACCELERATE_LLVM_PTX_BACKEND
        ,
        bgroup "GPU" [
          gpubenches "Regular"   (Just True)  fourierTransformSeq
        , gpubenches "Irregular" (Just False) fourierTransformSeq
        , gpubenches "Foreign"   Nothing fourierTransformForGPU
        , gpubenches "Lifted"   Nothing fourierTransformSelfLift
        , gpubenches "LiftedForeign"   Nothing fourierTransformSelfLiftFor
        , gpubenchesNor "Normal"    Nothing fourierTransformNor
        ]
#endif
        ]