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

inputN2 :: Int -> [Vector Int]
inputN2 n = P.replicate n (fromList (Z :. 1024) [P.floor (100 * P.cos x) | x <- [0..] ])

inputN3 :: Int -> [Vector Int]
inputN3 n = P.replicate n (fromList (Z :. 8) [P.floor (100 * P.cos x) | x <- [0..] ])

inputNAcc :: Acc (Scalar Int) -> Acc (Matrix Int)
inputNAcc n = generate (index2 (the n) 100) (\(unlift .unindex2->(i :: Exp Int,j :: Exp Int)) -> let k = A.fromIntegral (i*32+j) :: Exp Double
                                                                                                in floor $ 100 * cos k)

quickSortVec :: Acc (Matrix Int) -> Acc (Matrix Int)
quickSortVec = collect . tabulate . mapSeq (afst . quicksort) . toSeq2ndInner

quickSortVecB :: Acc (Matrix Int) -> Acc (Matrix Bool)
quickSortVecB = collect . tabulate . mapSeq (asnd . quicksort) . toSeq2ndInner

quickSortNor' :: Acc (Matrix Int) -> Acc (Matrix Int)
quickSortNor' xss = result
  where 
    c0 :: Exp Int
    c0 = constant 0
    slix     = lift (Z:.c0:.All)
    theslice :: Acc (Vector Int)
    theslice = slice xss slix

    sh     = indexTail . indexTrans . A.shape $ xss
    onesh  = indexTrans $ lift (sh :. constant 1)

    result = reshape onesh . afst . quicksort $ theslice

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

                res = transpose $ (transpose a) ++ (transpose reshaped)
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

readFiles :: Int -> Int -> IO (Array DIM2 Int)
readFiles n m = readArrayFile ("Futhark/list_" P.++ P.show m P.++ "_" P.++ P.show n P.++ ".in") (Z:.n:.m)

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


myenv :: Maybe Bool -> Int
      -> (Int, Int, Int, Int, Int, Int)
      -> (forall a b. (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b)
      -> (Acc (Matrix Int) -> Acc (Matrix Int))
      -> IO (Matrix Int, Matrix Int, Matrix Int, Matrix Int, Matrix Int, Matrix Int
            , Matrix Int -> Matrix Int)
myenv reg n (a,b,c,d,e,f) run1_ fun = do
    inpa <- readFiles a n
    inpb <- readFiles b n
    inpc <- readFiles c n
    inpd <- readFiles d n
    inpe <- readFiles e n
    inpf <- readFiles f n
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
    
    P.putStrLn ("Chunks of " P.++ P.show a)
    setEnv "ACCELERATE_FLAGS" ("-chunk-size=" P.++ P.show a)
    
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
                bench1 x = env (myenv2 reg x run1 inputNAcc f) $ \(~(inpx, runner)) -> bench (P.show x) $ nf runner inpx
            in bgroup name $ P.map bench1 xs
        
        cpunums1 = [1,100,1000,2000,5000,10000]
        gpunums1 = [1,100,1000,     5000,10000,20000]
        normbench = [1,100,1000]
        -- cpubenches name reg f = benches'' CPU.run1 cpunums1 name reg f
        cpubenchesNor name reg f = benches'' CPU.run1 normbench name reg f

        cpunums2 = (1,100,1000,2000,5000,10000)
        gpunums2 = (1,100,1000     ,5000,10000,20000)
        cpubenches name reg f = env (myenv reg 100 cpunums2 CPU.run1 f) (benches' name cpunums2)
#ifdef ACCELERATE_LLVM_PTX_BACKEND
        gpubenches name reg f = benches'' GPU.run1 gpunums1 name reg f
        gpubenchesNor name reg f = benches'' GPU.run1 normbench name reg f

        -- gpubenches name reg f = env (myenv reg 100 gpunums2 GPU.run1 f) (benches' name cpunums2)
#endif
    in [bgroup "CPU" [
                cpubenches "Regular"   (Just True)  quickSortVec
                , cpubenches "Irregular" (Just False) quickSortVec
                , cpubenchesNor "Normal"    Nothing quickSortNor
                , cpubenchesNor "Normal'"    Nothing quickSortNor'
                ]
#ifdef ACCELERATE_LLVM_PTX_BACKEND
            ,
                bgroup "GPU" [
                gpubenches "Regular"   (Just True)  quickSortVec
                , gpubenches "Irregular" (Just False) quickSortVec
                , gpubenchesNor "Normal"    Nothing quickSortNor
                , gpubenchesNor "Normal'"    Nothing quickSortNor'
                ]
#endif
            ]