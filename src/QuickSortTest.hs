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

time_ :: IO a -> IO String
time_ a = do t1 <- getSystemTime 
             a
             t2 <- getSystemTime
             return $ P.show (diffUTCTime (systemToUTCTime t2) (systemToUTCTime t1) )

readFiles :: Int -> Int -> IO (Matrix Int)
readFiles n m = readArrayFile ("Futhark/list_" P.++ P.show m P.++ "_" P.++ P.show n P.++ ".in") (Z:.n:.m)

readFilesV :: Int -> Int -> IO (Vector Int)
readFilesV _ m = readArrayFile ("Futhark/list_" P.++ P.show m P.++ "_" P.++ P.show 1 P.++ ".in") (Z:.m)

fileTest :: Int -> Int -> IO Int
fileTest n m = do
    inp <- readFiles n m
    P.putStrLn "Evaluating input"
    
#ifdef ACCELERATE_LLVM_PTX_BACKEND2
    P.putStrLn "GPU"
    let inprunner = GPU.run1 $ map (+1)
#else
    P.putStrLn "CPU"
    let inprunner = CPU.run1 $ map (+1)
#endif
    P.print $ indexArray (inprunner inp) (Z:.0:.0)

    P.putStrLn ("Chunks of " P.++ P.show n)
    setEnv "ACCELERATE_FLAGS" ("-chunk-size=" P.++ P.show n)

#ifdef ACCELERATE_LLVM_PTX_BACKEND2
    let runner = GPU.run1 quickSortVec
#else
    let runner = CPU.run1 quickSortVec
#endif

    P.putStrLn "Compiling function"
    time_ (evaluate runner) >>= (\x -> P.putStrLn ("Compiling took " P.++ x))
    time_ (evaluate (runner $ inputN 2)) >>= (\x -> P.putStrLn ("Small testrun took " P.++ x))

    time_ (evaluate (runner inp)) >>= (\x -> P.putStrLn ("Execution took " P.++ x))
    time_ (evaluate (runner inp)) >>= (\x -> P.putStrLn ("Execution2 took " P.++ x))
    time_ (evaluate (runner inp)) >>= (\x -> P.putStrLn ("Execution3 took " P.++ x))
    return (indexArray (runner inp) (Z:.0:.0) )

myenv :: (a ~ Array sh e, Shape sh, Elt e, e ~Int)
      => Maybe Bool -> Int
      -> (Int -> Int -> IO a)
      -> (Int, Int, Int, Int, Int, Int)
      -> (forall a b. (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b)
      -> (Acc a -> Acc a)
      -> IO (a, a, a, a, a, a
            , a -> a)
myenv reg m getInput (a,b,c,d,e,f) run1_ fun = do
    inpa <- getInput a m
    inpb <- getInput b m
    inpc <- getInput c m
    inpd <- getInput d m
    inpe <- getInput e m
    inpf <- getInput f m
    let runner = run1_ fun
        runinp = run1_ $ map (+0)
    
    P.putStrLn ("Chunks of " P.++ P.show f)
    setEnv "ACCELERATE_FLAGS" ("-chunk-size=" P.++ P.show f)

    P.putStrLn "Evaluating input"
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

myenvFlat :: (a ~ Array sh e, Shape sh, Elt e, e ~Int)
      => Maybe Bool -> Int
      -> (Int -> Int -> IO a)
      -> (Int, Int, Int, Int, Int, Int)
      -> (forall a b. (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b)
      -> (Acc a -> Acc a)
      -> IO (a, a, a, a, a, a
            , a -> a)
myenvFlat reg n getInput = myenv reg n (P.flip getInput)

myenv2 :: (a ~ Array sh e, Shape sh, Elt e, e ~Int)
      => Maybe Bool -> Int
      -> (Int -> Int -> IO a)
      -> Int
      -> (forall a b. (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b)
      -> (Acc a -> Acc a)
      -> IO (a, a -> a)
myenv2 reg n getInput m run1_ fun = do
    inp <- getInput n m
    let runner = run1_ fun
        runinp = run1_ $ map (+0)
    
    P.putStrLn ("Chunks of " P.++ P.show m)
    setEnv "ACCELERATE_FLAGS" ("-chunk-size=" P.++ P.show m)

    P.putStrLn "Evaluating input"
    evaluate (runinp inp)
    P.putStrLn "Compiling function"
    case reg of
        Nothing    -> return ()
        Just True  -> do P.putStrLn "Compiling regular"; clearforceIrreg;
        Just False -> do P.putStrLn "Compiling irregular"; setforceIrreg;
    time_ (evaluate runner) >>= (\x -> P.putStrLn ("Compiling took " P.++ x))
    P.putStrLn "Done with setup"
    return (inp, runner)

tester :: [Benchmark]
tester =
    let 
        benches' name (a,b,c,d,e,f) ~(inpa, inpb, inpc, inpd, inpe, inpf, runner) = bgroup name [
                  bench (P.show a) $ nf runner inpa
                , bench (P.show b) $ nf runner inpb
                , bench (P.show c) $ nf runner inpc
                , bench (P.show d) $ nf runner inpd
                , bench (P.show e) $ nf runner inpe
                , bench (P.show f) $ nf runner inpf
                ]
        
        benches'' :: (a ~ Array DIM2 Int, b ~ Array DIM2 Int) => (forall a b. (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b) -> [Int] -> String -> Maybe Bool -> (Acc a -> Acc b) -> Benchmark
        benches'' run1 xs name reg f =
            let 
                bench1 x = env (myenv2 reg 1000 readFiles x run1 f) $ \(~(inpx, runner)) -> bench (P.show x) $ nf runner inpx
            in bgroup name $ P.map bench1 xs

        -- benchFlat :: (forall a b. (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b) -> (Acc a -> Acc b) -> Benchmark
        -- benchFlat run1 f = 
        --     let 
        --         bench1 x 
        
        cpunums1 = [1,100,1000,2000,5000,10000]
        gpunums1 = [1,100,1000,     5000,10000,20000]
        cpunums2 = (1,100,1000,2000,5000,10000)
        gpunums2 = (1,100,1000     ,5000,10000,20000)

        normbench = [1,100,1000]
        flatbench = ( 100,1000,2000,5000,10000,20000)
        
        cpubenches name n reg f = env (myenv reg n readFiles cpunums2 CPU.run1 f) (benches' (name P.++ "/" P.++ P.show n) cpunums2)
        -- cpubenches name reg f = benches'' CPU.run1 cpunums1 name reg f
        cpubenchesNor name reg f = benches'' CPU.run1 normbench name reg f
        cpubenchesFlat name reg f = env (myenvFlat reg 1 readFilesV flatbench CPU.run1 f) (benches' name flatbench)

#ifdef ACCELERATE_LLVM_PTX_BACKEND
        -- gpubenches name reg f = benches'' GPU.run1 gpunums1 name reg f
        gpubenches name n reg f = env (myenv reg n readFiles cpunums2 GPU.run1 f) (benches' (name P.++ "/" P.++ P.show n) cpunums2)
        gpubenchesNor name reg f = benches'' GPU.run1 normbench name reg f

        gpubenchesFlat name reg f = env (myenvFlat reg 1 readFilesV flatbench GPU.run1 f) (benches' name flatbench)
#endif
    in [bgroup "CPU" [
                cpubenches "Regular" 100  (Just True)  quickSortVec
                , cpubenches "Irregular" 100 (Just False) quickSortVec
                , cpubenchesNor "Normal"    Nothing quickSortNor
                , cpubenchesFlat "Flat"    Nothing (afst. quicksort)
                ]
#ifdef ACCELERATE_LLVM_PTX_BACKEND
            ,
                bgroup "GPU" [
                gpubenches "Regular"  100 (Just True)  quickSortVec
                , gpubenches "Irregular" 100 (Just False) quickSortVec
                , gpubenchesNor "Normal"    Nothing quickSortNor
                , gpubenchesFlat "Flat"    Nothing (afst. quicksort)
                ]
#endif
            ]