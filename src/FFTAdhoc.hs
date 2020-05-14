{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# language FlexibleInstances, MultiParamTypeClasses, GADTs   #-}
-- |
-- Module      : Data.Array.Accelerate.Math.FFT.Adhoc
-- Copyright   : [2017] Henning Thielemann
--               [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Implementation of ad-hoc FFT stolen from the accelerate-fourier by Henning
-- Thielemann (BSD3 licensed), and updated to work with current Accelerate. That
-- package contains other more sophisticated algorithms as well.
--

module FFTAdhoc (ditSplitRadixLoop, FFTAdhoc.fft2D, FFTAdhoc.fft1D, fft2DV)
  where

import Data.Array.Accelerate                                        hiding ( transpose )
import Data.Array.Accelerate.Data.Complex
import Data.Array.Accelerate.Control.Lens.Shape

import Data.Array.Accelerate.Math.FFT hiding(ditSplitRadixLoop, fft)

import qualified Prelude as P

data NumericR a where
  NumericRfloat32 :: NumericR Float
  NumericRfloat64 :: NumericR Double

class (RealFloat a, FromIntegral Int a, FromIntegral Int32 a, Elt (Complex a)) => Numeric a where
  numericR :: NumericR a

instance Numeric Float where
  numericR = NumericRfloat32

instance Numeric Double where
  numericR = NumericRfloat64

fft1D :: (Numeric e)
    => Mode
    -> Acc (Array DIM2 (Complex e))
    -> Acc (Array DIM2 (Complex e))
fft1D mode arr =
  let
    scale = fromIntegral (size arr)
    go    = ditSplitRadixLoop mode
  in case mode of
      Inverse -> map (/scale) (go arr)
      _       -> go arr

fft2D :: (Numeric e)
    => Mode
    -> Acc (Array DIM2 (Complex e))
    -> Acc (Array DIM2 (Complex e))
fft2D mode arr =
  let
    scale = fromIntegral (size arr)
    go    = transpose . ditSplitRadixLoop mode >-> transpose . ditSplitRadixLoop mode
  in case mode of
      Inverse -> map (/scale) (go arr)
      _       -> go arr

fft2DV :: (Numeric e)
    => Mode
    -> Acc (Array DIM3 (Complex e))
    -> Acc (Array DIM3 (Complex e))
fft2DV mode arr =
  let
    scale = fromIntegral (shapeSize . indexTail . indexTrans . shape $ arr)
    go    = transpose . ditSplitRadixLoop mode >-> transpose . ditSplitRadixLoop mode
    -- go = ditSplitRadixLoop mode
  in case mode of
      Inverse -> map (/scale) (go arr)
      _       -> go arr


-- Implementations
-- ---------------

-- -- | Split-radix for power-of-two sizes
-- --
ditSplitRadixLoop
    :: forall sh e. (Shape sh, Slice sh, Numeric e)
    => Mode
    -> Acc (Array (sh:.Int) (Complex e))
    -> Acc (Array (sh:.Int) (Complex e))
ditSplitRadixLoop mode arr =
  let
      twiddleSR (fromIntegral -> n4) k (fromIntegral -> j) =
        let w = pi * k * j / (2*n4)
        -- in lift (exp w :+ w)
        in  lift (cos w :+ signOfMode mode * sin w)
      
      --Twidles with same len4, generate same shaped arrays
      twiddle len4 k =
        generate (index1 len4) (twiddleSR len4 k . indexHead)
      -- So step stays regular
      step :: Acc (Array (sh:.Int:.Int) (Complex e), Array (sh:.Int:.Int) (Complex e)) -> Acc (Array (sh:.Int:.Int) (Complex e), Array (sh:.Int:.Int) (Complex e))
      step (unlift -> (us,zs)) =
        let
            --k is same, since dependent on shape
            k           = indexHead (shape zs)
            --Regulary shaped
            tw1         = twiddle k 1
            --Regulary shaped
            tw3         = twiddle k 3
            --
            im          = lift (makeExp 0 :+ signOfMode mode) 
            twidZeven   = zipWithExtrude1 (*) tw1 (sieveV 2 0 zs) --sieveV gives regular shape
            twidZodd    = zipWithExtrude1 (*) tw3 (sieveV 2 1 zs) -- since input is regular, zipwithExtrude1 is aswell
            zsum        = zipWith (+) twidZeven twidZodd          -- input regular, thus also output
            zdiff       = map (im *) (zipWith (-) twidZeven twidZodd) --input regular, thus output
            zcomplete   = zsum ++ zdiff -- input regular, thus output
            _ :. n :. _ = unlift (shape zcomplete) :: Exp sh :. Exp Int :. Exp Int -- same for each, since regular
        in
        lift ( zipWith (+) us zcomplete ++ zipWith (-) us zcomplete -- This is regular
             , dropV n us
             )
      
      --transform stays regular, so everything stays regular
      rebase s = lift (transform2 (-1) (afst s), asnd s)
      
      --Reorder stays regular, for reasons below
      reorder (unlift -> (xs,ys)) =
        let -- Evens and odd have stay regular, since the factor (2) is always the same.
            evens = sieve 2 0 xs
            odds  = sieve 2 1 xs
        in
        -- Since evens stay regular and twist stays regular (factor is always 2), this stays regular
        lift (evens ++^ ys, twist 2 odds)
      -- New shape is comming from old shape, fully determined by it. So stays regular. 
      initial :: Acc (Array (sh :. Int :. Int) (Complex e), Array (sh :. Int :. Int) (Complex e)) 
      initial =
        let sh :. n = unlift (shape arr) :: Exp sh :. Exp Int
        in  lift ( reshape (lift (sh :. (1::Int) :. n)) arr
                 , fill    (lift (sh :. (0::Int) :. n `quot` 2)) 0
                 )
  in
  --headV also stays regular, so result is regular
  -- arr
  headV
    $ afst
    -- step stay regular, and since check function is fully determined on the shape, it will stay regular
    $ awhile (\s -> unit (indexHead (indexTail (shape (asnd s))) > 0)) step
    -- rebase stays regular
    $ rebase
    -- Since the check function is fully determined on the shape, it will stay regular
    $ awhile (\s -> unit (indexHead (shape (asnd s)) > 1)) reorder
    initial

-- Utilities
-- ---------

-- If input is regular, stays regular
headV :: (Shape sh, Slice sh, Elt e)
      => Acc (Array (sh:.Int:.Int) e)
      -> Acc (Array (sh:.Int) e)
headV xs = slice xs (lift (Any :. (0 :: Exp Int) :. All))

-- If Exp Int and array shape are same, then stays regular.
dropV :: forall sh e. (Shape sh, Slice sh, Elt e)
      => Exp Int
      -> Acc (Array (sh:.Int:.Int) e)
      -> Acc (Array (sh:.Int:.Int) e)
dropV = dropOn _2

--New shape is fully determined by old one and fac. If shape and fac same, stays regular.
sieve
    :: forall sh e. (Shape sh, Slice sh, Elt e)
    => Exp Int
    -> Exp Int
    -> Acc (Array (sh:.Int) e)
    -> Acc (Array (sh:.Int) e)
sieve fac start xs =
  let sh :. n = unlift (shape xs) :: Exp sh :. Exp Int
  in
  backpermute
    (lift (sh :. n `quot` fac))
    (\(unlift -> ix :. j :: Exp sh :. Exp Int) -> lift (ix :. fac*j + start))
    xs

--New shape is fully determined by old one and fac. If shape and fac same, stays regular.
sieveV
    :: forall sh e. (Shape sh, Slice sh, Elt e)
    => Exp Int
    -> Exp Int
    -> Acc (Array (sh:.Int:.Int) e)
    -> Acc (Array (sh:.Int:.Int) e)
sieveV fac start xs =
  let sh :. m :. n = unlift (shape xs) :: Exp sh :. Exp Int :. Exp Int
  in
  backpermute
    (lift (sh :. m `quot` fac :. n))
    (\(unlift -> ix :. j :. i :: Exp sh :. Exp Int :. Exp Int) -> lift (ix :. fac*j+start :. i))
    xs

-- If fac and shape stay the same, then twist stays regular.
twist :: forall sh e. (Shape sh, Slice sh, Elt e)
      => Exp Int
      -> Acc (Array (sh:.Int:.Int) e)
      -> Acc (Array (sh:.Int:.Int) e)
twist fac xs =
  let sh :. m :. n = unlift (shape xs) :: Exp sh :. Exp Int :. Exp Int
  in
  backpermute
    (lift (sh :. fac*m :. n `quot` fac))
    (\(unlift -> ix :. j :. i :: Exp sh :. Exp Int :. Exp Int) -> lift (ix :. j `quot` fac :. fac*i + j `rem` fac))
    xs


infixr 5 ++^
(++^) :: forall sh e. (Shape sh, Slice sh, Elt e)
      => Acc (Array (sh:.Int:.Int) e)
      -> Acc (Array (sh:.Int:.Int) e)
      -> Acc (Array (sh:.Int:.Int) e)
(++^) = concatOn _2

-- if xs and ys regulary shaped, then zipWithExtrude1 also regulary shaped
zipWithExtrude1
    :: (Shape sh, Slice sh, Elt a, Elt b, Elt c)
    => (Exp a -> Exp b -> Exp c)
    -> Acc (Array DIM1      a)
    -> Acc (Array (sh:.Int) b)
    -> Acc (Array (sh:.Int) c)
zipWithExtrude1 f xs ys =
  zipWith f (replicate (lift (indexTail (shape ys) :. All)) xs) ys

transpose
    :: forall sh e. (Shape sh, Slice sh, Elt e)
    => Acc (Array (sh:.Int:.Int) e)
    -> Acc (Array (sh:.Int:.Int) e)
transpose = transposeOn _1 _2

-- Transform2 stays regular, fully based on shape (and constant expresion)
transform2
    :: (Shape sh, Slice sh, Num e)
    => Exp e
    -> Acc (Array (sh:.Int) e)
    -> Acc (Array (sh:.Int) e)
transform2 v xs =
  generate
    (lift (indexTail (shape xs) :. (2::Int)))
    (\(unlift -> ix :. k :: Exp sh :. Exp Int) ->
        let x0 = xs ! lift (ix :. (0::Int))
            x1 = xs ! lift (ix :. (1::Int))
        in
        if k == 0 then x0+x1
                  else x0+v*x1)

makeExp :: Numeric e => Int -> Exp e
makeExp = fromIntegral . constant

signOfMode :: P.Num a => Mode -> a
signOfMode m
  = case m of
      Forward   -> -1
      Reverse   ->  1
      Inverse   ->  1