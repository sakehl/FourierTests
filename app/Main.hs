module Main where

-- import FourierTest
import QuickSortTest

import Data.Array.Accelerate                              as A hiding (fromInteger, fromRational, fromIntegral)
import qualified Data.Array.Accelerate                    as A (fromInteger, fromRational, fromIntegral)

import Data.Array.Accelerate.LLVM.Native                  as CPU
import Data.Array.Accelerate.Interpreter                  as I

import qualified Prelude as P
import Prelude as P (fromIntegral, fromInteger, fromRational, String, return, (>>=), (>>), IO, Maybe(..), maybe)

import Debug.Trace
import Data.Array.Accelerate.Debug as D

import Criterion.Main


main :: IO ()
main = do
    defaultMain [{-bgroup "Fourier" FourierTest.tester,-} bgroup "QuickSort" QuickSortTest.tester]
    -- fileTest 1 1000
    -- fileTest 10 1000
    -- fileTest 100 1000
    return ()
    --setFlag dump_vectorisation
    --P.print result2
    --P.print "hello" -- $ CPU.run result5
  
    --P.print result5
    --P.print $ I.run result2

