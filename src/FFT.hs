{-# language FlexibleContexts    #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}

module FFT where

--import Types
--import Utility

import Data.Array.Accelerate                                        as A hiding (fromInteger, fromRational, fromIntegral)
import qualified Data.Array.Accelerate                              as A (fromInteger, fromRational, fromIntegral)
import Data.Array.Accelerate.Data.Complex                           as A
import Data.Array.Accelerate.Math.DFT.Centre                        as A
import Data.Array.Accelerate.Math.FFT                               as A 
-- import Data.Array.Accelerate.Math.FFT.Type                          as A
-- import Data.Array.Accelerate.Math.FFT.LLVM.PTX                      as GPU
--import Data.Array.Accelerate.Math.FFT.LLVM.Native.Ix
--import Data.Array.Accelerate.Math.FFT.LLVM.Native.Base

import Data.Array.Accelerate.LLVM.Native                            as CPU
import Data.Array.Accelerate.Array.Sugar                            as S hiding (shape)
import Data.Array.Accelerate.LLVM.Native.Foreign                    as CPU
import Data.Array.Accelerate.Array.Unique                           as A
import Data.Array.Accelerate.Array.Lifted                           as A
import Data.Array.Accelerate.Error                                  as A
import Data.Array.Accelerate.Type                                   as A
import Data.Array.Accelerate.Lifetime                               as A


import Data.Ix                                                      ( Ix )
--import Data.Array.CArray                                            ( CArray(..) )
import Data.Array.CArray.Base                                       ( CArray(..) )
import qualified Data.Array.CArray                                  as C
import qualified Data.Array.CArray.Base                             as C
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr

import Math.FFT                                                     as FFT
import Math.FFT.Base                                                ( FFTWReal, Sign(..), Flag, measure, destroyInput )

import qualified Foreign.CUDA.FFT                                   as FFT

import Text.Printf
import Data.Bits

import qualified Prelude as P
import Prelude as P (fromIntegral, fromInteger, fromRational, String, return, (>>=), (>>), IO, Maybe(..), maybe, fail, (=<<))
import Debug.Trace

type F = Double
type Visibility = Complex F

data NumericR a where
  NumericRfloat32 :: NumericR Float
  NumericRfloat64 :: NumericR Double

class (RealFloat a, FromIntegral Int a, FromIntegral Int32 a, Elt (Complex a)) => Numeric a where
  numericR :: NumericR a

instance Numeric Float where
  numericR = NumericRfloat32

instance Numeric Double where
  numericR = NumericRfloat64

----------------------
-- Fourier transformations
-- fft :: Acc (Matrix Visibility) -> Acc (Matrix Visibility)
-- fft = shift2D . myfft2D Forward . ishift2D

-- ifft :: Acc (Matrix Visibility) -> Acc (Matrix Visibility)
-- ifft = shift2D . myfft2D Inverse . ishift2D

myfft2D :: FFTElt e => Mode -> Acc (Array DIM2 (Complex e)) -> Acc (Array DIM2 (Complex e))
myfft2D mode = fft2D' mode (Z :. 32 :. 32) --myfft2DFor