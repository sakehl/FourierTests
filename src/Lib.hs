{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# language FlexibleContexts    #-}

module Lib where

import Data.Array.Accelerate                              as A hiding (fromInteger, fromRational, fromIntegral)
import qualified Data.Array.Accelerate                    as A (fromInteger, fromRational, fromIntegral)

import Data.Array.Accelerate.LLVM.Native                  as CPU
import Data.Array.Accelerate.Interpreter                  as I

import qualified Prelude as P
import Prelude as P (fromIntegral, fromInteger, fromRational, String, return, (>>=), (>>), IO, Maybe(..), maybe, (=<<))

import Debug.Trace
import Data.Array.Accelerate.Array.Sugar                            as S
import Data.Array.Accelerate.Error                                  as A
import Data.Array.Accelerate.Type                                   as A
import Data.Array.Accelerate.Array.Unique                           as A
import Data.Array.Accelerate.Array.Lifted                           as A
import Data.Array.Accelerate.Data.Complex                           as A
import Data.Array.Accelerate.Math.DFT.Centre                        as A hiding (shift2D)
import Data.Array.Accelerate.Math.FFT                               as A 
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
{-
someFunc :: IO ()
someFunc = P.putStrLn "someFunc"

x1, x2 :: Vector Int
x1 = fromList (Z :. 5) [0..]
x2 = fromList (Z :. 6) [10..]

incr :: Acc (Vector Int) -> Acc (Vector Int)
incr = map (+1)

sequence :: Seq [Vector Int]
sequence = streamIn [x1,x2]

incrSeq :: Seq [Vector Int] -> Seq [Vector Int]
incrSeq = mapSeq incr

result0 :: Acc (Vector Int)
result0 = incr . use $ x1

result :: Acc (Vector Int)
result = collect . elements . incrSeq $ sequence

result2 :: Acc (Vector (Complex Double))
result2 = --accVec . use $ xs
  collect . elements . mapSeq accVec $ seq3
  where
    xs1 = fromList (Z :. 5) $ P.zipWith (:+) [0..] [10..]
    xs2 = fromList (Z :. 4) $ P.zipWith (:+) [6..] [16..]
    xs3 = fromList (Z :.2 :.4) $ P.zipWith (:+) [0..] [5..]
    seq1, seq2 :: Seq [Vector (Complex Double)]
    seq1 = streamIn $ [xs1, xs2]
    seq2 = toSeqInner . use $ xs3
    seq3 = toSeq (constant (Z :. (0::Int) :. All)) . use $ xs3

    accVec :: Acc (Vector (Complex Double)) -> Acc (Vector (Complex Double))
    accVec = foreignAcc vectFor $ A.map (+10)

    foreignVec :: ForeignAcc (Vector (Complex Double) -> Vector (Complex Double))
    foreignVec = ForeignAcc "testVec" testFor

    testFor :: (Vector (Complex Double) -> LLVM Native (Vector (Complex Double)))
    testFor = liftIO . liftAtoC testWithC

    foreignRMat :: ForeignAcc (Array DIM2 (Complex Double) -> Array DIM2 (Complex Double))
    foreignRMat = ForeignAcc "testMat" $ liftIO . liftAtoC testRWithC

    --foreignIRMat :: ForeignAcc (IrregularArray DIM1 (Complex Double) -> IrregularArray DIM1 (Complex Double))
    foreignIRMat :: ForeignAcc ( ((Scalar Int, Vector Int, Vector DIM1), Vector (Complex Double)) -> IrregularArray DIM1 (Complex Double))
    foreignIRMat = ForeignAcc "testMatIR" $ liftIO . liftAtoCIr testIrWithC

    vectFor :: VectorisedForeign (Vector (Complex Double) -> Vector (Complex Double))
    vectFor = VectorisedForeign $  f
      where
        f :: Arrays a' => LiftedType (Vector (Complex Double)) a' -> LiftedType (Vector (Complex Double)) b' -> ForeignAcc (a' -> b')
        f RegularT RegularT = foreignRMat
        f AvoidedT AvoidedT = foreignVec
        f IrregularT IrregularT = foreignIRMat
        f _ _ = error "Strange call to foreign function"

result3 :: IO (Array DIM2 (Complex Double))
result3 = liftAtoC testRWithC $ xs
  where
    xs = fromList (Z :. 3 :. 2) [(i :+ (i + 10))| i <- [0..]]


--result4 :: [(Complex Double, Complex Double)]
result4 :: [(Complex Double)]
result4 =  --P.zipWith (,) (toList res1A) (toList res2A P.++ toList res3A)--P.zipWith (-) res3 res2
    P.zipWith (-) (toList res2A P.++ toList res3A) (toList res2B)
    where
      inl1 = P.take 64 [i :+ (i + 10)| i <- [0..]]
      inl2 = P.take 64 [i :+ (i + 10)| i <- [64..]]
      inp :: CArray (Int, Int, Int) (Complex Double)
      inp = C.listArray ((0,0,0),(1,7,7)) (inl1 P.++ inl2)
      res = C.elems $ FFT.dftN [0,1] inp
      res2 = C.elems $ testV inp


      inp2, inp3 :: CArray (Int,Int) (Complex Double)
      inp2 = C.listArray ((0,0),(7,7)) inl1
      inp3 = C.listArray ((0,0),(7,7))  inl2
      res3 = C.elems (test inp2) P.++ C.elems (test inp3)

      inpA :: Acc (Array DIM3 (Complex Double))
      inpA = use $ fromList (Z :. 2 :. 8 :. 8) (inl1 P.++ inl2)
      inpASeq = toSeq (constant (Z :. (0::Int) :. All :. All)) inpA
      inp1A, inp2A :: Acc (Array DIM2 (Complex Double))
      inp1A = use $ fromList (Z :. 8 :. 8) inl1
      inp2A = use $ fromList (Z :. 8 :. 8) inl2

      res1A ::  Array DIM3 (Complex Double)
      res1A = CPU.run $ fft2DRegular_ inpA
      res1ASeq = CPU.run . collect . tabulate . mapSeq fft2DVect_ $ inpASeq
      res2A, res3A :: Array DIM2 (Complex Double)
      res2A = CPU.run $ fft2DVect_ inp1A
      res2B = CPU.run $ fft2DAvoid_ inp1A
      res3A = CPU.run $ fft2DVect_ inp2A
      res3B = CPU.run $ fft2DAvoid_ inp2A

      test :: CArray (Int,Int) (Complex Double) -> CArray (Int,Int) (Complex Double)
      test = FFT.dftG DFTForward flags [0,1]
      testV :: CArray (Int, Int,Int) (Complex Double) -> CArray (Int, Int,Int) (Complex Double)
      testV = FFT.dftG DFTForward flags [1,2]

      flags :: Flag
      flags = estimate .|. destroyInput

      fft2DAvoid :: ForeignAcc (Array DIM2 (Complex Double) -> Array DIM2 (Complex Double))
      fft2DAvoid = ForeignAcc "normalfft2D" $ liftIO . liftAtoC fft
        where
          fft :: CArray (Int,Int) (Complex Double) -> CArray (Int,Int) (Complex Double)
          fft = FFT.dftG DFTForward flags [0,1] --FFT.dftN [0,1]

      fft2DRegular :: ForeignAcc (Array DIM3 (Complex Double) -> Array DIM3 (Complex Double))
      fft2DRegular = ForeignAcc "regularVectfft2D" $ liftIO . liftAtoC fft
        where
          fft :: CArray (Int,Int, Int) (Complex Double) -> CArray (Int,Int, Int) (Complex Double)
          fft = FFT.dftG DFTForward flags [1,2] --FFT.dftN [1,2]

      fft2DAvoid_ :: Acc (Array DIM2 (Complex Double)) -> Acc (Array DIM2 (Complex Double))
      fft2DAvoid_ = foreignAcc fft2DAvoid (map (+0))

      fft2DRegular_ :: Acc (Array DIM3 (Complex Double)) -> Acc (Array DIM3 (Complex Double))
      fft2DRegular_ = foreignAcc fft2DRegular (map (+0))

      fft2DVect_ :: Acc (Array DIM2 (Complex Double)) -> Acc (Array DIM2 (Complex Double))
      fft2DVect_ = foreignAcc fft2DVect $ map (+666.666 )

      fft2DVect :: VectorisedForeign (Array DIM2 (Complex Double) -> Array DIM2 (Complex Double))
      fft2DVect = VectorisedForeign $ f
        where
          f :: Arrays a' => LiftedType (Array DIM2 (Complex Double)) a' -> LiftedType (Array DIM2 (Complex Double)) b' -> ForeignAcc (a' -> b')
          f AvoidedT AvoidedT = fft2DAvoid
          f RegularT RegularT = fft2DRegular
          f _ _ = error "????"

result5 :: Acc (Array DIM3 (Complex Double))--, Array DIM3 (Complex Double))
result5 = zipWith (-) res3_ res4_ --lift (res3_,res3) -- -- --lift (res3_,res3)--
    where
      inp1, inp2 :: Acc (Array DIM2 (Complex Double))
      inp1  = use $ fromList (Z :. 32 :. 32)  [i :+ (i + 10)| i <- [0..]]
      inp2 = use $ fromList (Z :. 32 :. 32)  [i :+ (i + 10)| i <- [16..]]
      inp3 :: Acc (Array DIM3 (Complex Double))
      inp3 = reshape newD inp1 `concatOuter` reshape newD inp2
      inp3Seq = toSeq (constant (Z :. (0::Int) :. All :. All)) inp3

      newD :: Exp DIM3
      newD = constant $ Z :. 1 :. 32 :. 32
      
      myfunc = myfft2D Forward
      --myfunc = map (+1)
      myfunc2 = A.fft2D' Forward (Z :. 32 :. 32)

      res1 = myfunc inp1
      res2 = myfunc inp2
      res3 = reshape newD res1 `concatOuter` reshape newD res2
      res3_= collect . tabulate . mapSeq myfunc $ inp3Seq

      res4_ = collect . tabulate . mapSeq myfunc2 $ inp3Seq

concatOuter :: Elt e => Acc (Array DIM3 e) -> Acc (Array DIM3 e) -> Acc (Array DIM3 e)
concatOuter xs ys = let
  Z :. n :. y1 :. x1        = unlift (A.shape xs)     :: Z :. Exp Int :. Exp Int :. Exp Int
  Z :. m :. y2 :. x2        = unlift (A.shape ys)     :: Z :. Exp Int :. Exp Int :. Exp Int
  minx = min x1 x2
  miny = min y1 y2
  in generate (lift $ Z :. n + m :. miny :. minx)
              (\ix -> let Z :. i :. y :. x = unlift ix :: Z :. Exp Int :. Exp Int :. Exp Int
                      in i < n ? (xs A.! ix, ys A.! lift (Z :. i-n :. y :. x)))

myfft2D :: FFTElt e => Mode -> Acc (Array DIM2 (Complex e)) -> Acc (Array DIM2 (Complex e))
myfft2D mode = foreignAcc (fft2DVect mode) $ A.map (+666.666) {-Bollocks implementation, for checking-}
    where
        fft2DAvoid :: forall e. (Elt e, IsFloating e) 
                    => Mode 
                    -> ForeignAcc (Array DIM2 (Complex e) -> Array DIM2 (Complex e))
        fft2DAvoid mode = ForeignAcc (nameOf mode (undefined::DIM2))
            $ case floatingType :: FloatingType e of
                TypeFloat{}   -> liftIO . liftAtoC go
                TypeDouble{}  -> liftIO . liftAtoC go
                TypeCFloat{}  -> liftIO . liftAtoC go
                TypeCDouble{} -> liftIO . liftAtoC go
            where
                go :: FFTWReal r => CArray (Int,Int) (Complex r) -> CArray (Int,Int) (Complex r)
                go = FFT.dftG (signOf mode) flags [0,1]

        fft2DRegular :: forall e. (Elt e, IsFloating e) 
                    => Mode 
                    -> ForeignAcc (Array DIM3 (Complex e) -> Array DIM3 (Complex e))
        fft2DRegular mode = ForeignAcc (nameOfV mode (undefined::DIM2))
            $ case floatingType :: FloatingType e of
                TypeFloat{}   -> liftIO . liftAtoC go
                TypeDouble{}  -> liftIO . liftAtoC go
                TypeCFloat{}  -> liftIO . liftAtoC go
                TypeCDouble{} -> liftIO . liftAtoC go
            where
                go :: FFTWReal r => CArray (Int,Int,Int) (Complex r) -> CArray (Int,Int,Int) (Complex r)
                go = FFT.dftG (signOf mode) flags [1,2]

        fft2DVect :: forall e. (Elt e, IsFloating e)
                => Mode -> VectorisedForeign (Array DIM2 (Complex e) -> Array DIM2 (Complex e))
        fft2DVect mode = VectorisedForeign $  f
            where
                f :: Arrays a' => LiftedType (Array DIM2 (Complex e)) a' -> LiftedType (Array DIM2 (Complex e)) b' -> ForeignAcc (a' -> b')
                f AvoidedT AvoidedT = fft2DAvoid mode
                f RegularT RegularT = fft2DRegular mode
                f IrregularT IrregularT = error "no irregular stuff"

        signOf :: Mode -> Sign
        signOf Forward = DFTForward
        signOf _       = DFTBackward

        flags :: Flag
        flags = estimate .|. destroyInput

        nameOf :: forall sh. Shape sh => Mode -> sh -> String
        nameOf Forward _ = "FFTW.dft" P.++ P.show (rank (undefined::sh)) P.++ "D"
        nameOf _       _ = "FFTW.idft" P.++ P.show (rank (undefined::sh)) P.++ "D"

        nameOfV :: forall sh. Shape sh => Mode -> sh -> String
        nameOfV Forward _ = "FFTW.dft" P.++ P.show (rank (undefined::sh)) P.++ "DV"
        nameOfV _       _ = "FFTW.idft" P.++ P.show (rank (undefined::sh)) P.++ "DV"


foreign import ccall safe "multi.cpp testingOne" c_testingOne
    :: Int -> Ptr (Complex Double) -> Ptr (Complex Double) -> IO ()

foreign import ccall safe "multi.cpp testingR" c_testingR
    :: Int -> Int -> Ptr (Complex Double) -> Ptr (Complex Double) -> IO ()

foreign import ccall safe "multi.cpp testingIr" c_testingIr
    :: Int -> Ptr Int -> Ptr Int -> Ptr (Complex Double) -> Ptr (Complex Double) -> IO ()

testWithC :: CArray Int (Complex Double) -> CArray Int (Complex Double)
testWithC a = let
    sz = C.size a
    in unsafePerformIO $ do
    wfp <- C.mallocForeignPtrArrayAligned sz
    C.withCArray a $ \ap ->
        withForeignPtr wfp $ \wp ->
            c_testingOne sz ap wp
    C.unsafeForeignPtrToCArray wfp (0, sz-1)

testRWithC :: CArray (Int,Int) (Complex Double) -> CArray (Int,Int) (Complex Double)
testRWithC a = let
    h : w : [] = C.shape a
    sz = h * w
    in unsafePerformIO $ do
    wfp <- C.mallocForeignPtrArrayAligned sz
    C.withCArray a $ \ap ->
        withForeignPtr wfp $ \wp ->
            c_testingR h w ap wp
    C.unsafeForeignPtrToCArray wfp ((0,0), (h-1,w-1))

testIrWithC :: CArray () Int -> CArray Int Int -> CArray Int Int -> CArray Int (Complex Double) -> CArray Int (Complex Double)
testIrWithC sz' offset extent input = let
  n = C.size offset
  in unsafePerformIO $ do
  sz <- C.withCArray sz' $ peek
  resfp <- C.mallocForeignPtrArrayAligned sz
  C.withCArray offset $ \offsetp ->
    C.withCArray extent $ \extentp ->
      C.withCArray input $ \inputp ->
        withForeignPtr resfp $ \resp ->
          c_testingIr n offsetp extentp inputp resp
  C.unsafeForeignPtrToCArray resfp (0, sz - 1)

liftAtoCIr ::
  (CArray () Int -> CArray Int Int -> CArray Int Int -> CArray Int (Complex Double) -> CArray Int (Complex Double))
  -> IrregularArray DIM1 (Complex Double)
  -> IO (IrregularArray DIM1 (Complex Double))
liftAtoCIr f ((size, offset, extend), values) = do
  sizeC <- a2cG size
  offsetC <- a2cG offset
  extendC <- a2cGDIM extend
  result <- liftAtoC (f sizeC offsetC extendC) values
  return ((size, offset, extend), result)

-- | Lift an operation on CArray into an operation on Accelerate arrays
--
liftAtoC
    :: (IxShapeRepr (EltRepr ix) ~ EltRepr sh, Shape sh, Ix ix, Elt ix, Elt e, IsFloating e, Storable e', ArrayPtrs e ~ Ptr e')
    => (CArray ix (Complex e') -> CArray ix (Complex e'))
    -> Array sh (Complex e)
    -> IO (Array sh (Complex e))
liftAtoC f a = c2a . f =<< a2c a


-- | Convert a multidimensional Accelerate array of complex numbers into
-- a packed CArray of complex numbers suitable for use by FFTW.
--
a2c :: forall ix sh e e'. (IxShapeRepr (EltRepr ix) ~ EltRepr sh, Ix ix, Elt ix, Shape sh, IsFloating e, Storable e', ArrayPtrs e ~ Ptr e')
    => Array sh (Complex e)
    -> IO (CArray ix (Complex e'))
a2c arr
  | FloatingDict <- floatingDict (floatingType :: FloatingType e)
  = let
        (lo,hi) = shapeToRange (arrayShape arr)
        bnds    = (fromIxShapeRepr lo, fromIxShapeRepr hi)
        n       = S.size (arrayShape arr)
    in
    C.createCArray       bnds $ \p_cs      ->
    withComplexArrayPtrs arr  $ \p_re p_im ->
      let
          -- TLM: Should we execute this in parallel using the worker threads of
          -- the current target? (Native)
          go !i | i P.>= n = return ()
          go !i            = do
            re <- peekElemOff p_re i
            im <- peekElemOff p_im i
            pokeElemOff p_cs i (re :+ im)
            go (i+1)
      in
      go 0

-- | Convert a packed CArray of complex numbers into an unzipped (SoA)
-- multidimensional Accelerate array of complex numbers.
--
c2a :: forall ix sh e e'. (IxShapeRepr (EltRepr ix) ~ EltRepr sh, Ix ix, Elt ix, Shape sh, Elt e, IsFloating e, Storable e', ArrayPtrs e ~ Ptr e')
    => CArray ix (Complex e')
    -> IO (Array sh (Complex e))
c2a carr
  | FloatingDict <- floatingDict (floatingType :: FloatingType e)
  = let
        (lo,hi) = C.bounds carr
        n       = C.rangeSize (lo,hi)
        sh      = rangeToShape (toIxShapeRepr lo, toIxShapeRepr hi)
    in do
      arr <- allocateArray sh
      C.withCArray carr        $ \p_cs      ->
        withComplexArrayPtrs arr $ \p_re p_im -> do
          let
              -- TLM: Should we execute this in parallel using the worker threads
              -- of the current target? (Native)
              go !i | i P.>= n = return ()
              go !i            = do
                  re :+ im <- peekElemOff p_cs i
                  pokeElemOff p_re i re
                  pokeElemOff p_im i im
                  go (i+1)
          --
          go 0
          return arr

-- | Convert a multidimensional Accelerate array of int numbers into
-- a packed CArray of int numbers
--
a2cG :: forall ix sh e e'. (IxShapeRepr (EltRepr ix) ~ EltRepr sh, Ix ix, Elt ix, Shape sh, EltRepr e ~ Int)
    => Array sh e
    -> IO (CArray ix Int)
a2cG arr
  = let
        (lo,hi) = shapeToRange (arrayShape arr)
        bnds    = (fromIxShapeRepr lo, fromIxShapeRepr hi)
        n       = S.size (arrayShape arr)
    in
    C.createCArray       bnds $ \p_cs      ->
      withIntArray arr  $ \p_ar ->
        let
            -- TLM: Should we execute this in parallel using the worker threads of
            -- the current target? (Native)
            go !i | i P.>= n = return ()
            go !i            = do
              x <- peekElemOff p_ar i
              pokeElemOff p_cs i x
              go (i+1)
        in
        go 0

a2cGDIM :: forall ix sh e e'. (IxShapeRepr (EltRepr ix) ~ EltRepr sh, Ix ix, Elt ix, Shape sh)
    => Array sh DIM1
    -> IO (CArray ix Int)
a2cGDIM arr
  = let
        (lo,hi) = shapeToRange (arrayShape arr)
        bnds    = (fromIxShapeRepr lo, fromIxShapeRepr hi)
        n       = S.size (arrayShape arr)
    in
    C.createCArray       bnds $ \p_cs      ->
      withShapeArray arr  $ \p_ar ->
        let
            -- TLM: Should we execute this in parallel using the worker threads of
            -- the current target? (Native)
            go !i | i P.>= n = return ()
            go !i            = do
              x <- peekElemOff p_ar i
              pokeElemOff p_cs i x
              go (i+1)
        in
        go 0

-- | Convert a packed CArray of complex numbers into an unzipped (SoA)
-- multidimensional Accelerate array of complex numbers.
--
c2aG :: forall ix sh e e'. (IxShapeRepr (EltRepr ix) ~ EltRepr sh, Ix ix, Elt ix, Shape sh)
    => CArray ix Int
    -> IO (Array sh Int)
c2aG carr
  = let
        (lo,hi) = C.bounds carr
        n       = C.rangeSize (lo,hi)
        sh      = rangeToShape (toIxShapeRepr lo, toIxShapeRepr hi)
    in do
      arr <- allocateArray sh
      C.withCArray carr        $ \p_cs      ->
        withIntArray arr $ \p_ar -> do
          let
              -- TLM: Should we execute this in parallel using the worker threads
              -- of the current target? (Native)
              go !i | i P.>= n = return ()
              go !i            = do
                  x <- peekElemOff p_cs i
                  pokeElemOff p_ar i x
                  go (i+1)
          --
          go 0
          return arr

-- Converting between Accelerate multidimensional shapes/indices and those used
-- by the CArray package (Data.Ix)
--

type family IxShapeRepr e where
  IxShapeRepr ()    = ()
  IxShapeRepr Int   = ((),Int)
  IxShapeRepr (t,h) = (IxShapeRepr t, h)

fromIxShapeRepr
    :: forall ix sh. (IxShapeRepr (EltRepr ix) ~ EltRepr sh, Shape sh, Elt ix)
    => sh
    -> ix
fromIxShapeRepr = liftToElt (go (eltType (undefined::ix)))
  where
    go :: forall ix'. TupleType ix' -> IxShapeRepr ix' -> ix'
    go UnitTuple                                                 ()     = ()
    go (PairTuple tt _)                                          (t, h) = (go tt t, h)
    go (SingleTuple (NumScalarType (IntegralNumType TypeInt{}))) ((),h) = h
    go _ _
      = $internalError "fromIxShapeRepr" "expected Int dimensions"

toIxShapeRepr
    :: forall ix sh. (IxShapeRepr (EltRepr ix) ~ EltRepr sh, Shape sh, Elt ix)
    => ix
    -> sh
toIxShapeRepr = liftToElt (go (eltType (undefined::ix)))
  where
    go :: forall ix'. TupleType ix' -> ix' -> IxShapeRepr ix'
    go UnitTuple        ()                                             = ()
    go (SingleTuple     (NumScalarType (IntegralNumType TypeInt{}))) h = ((), h)
    go (PairTuple tt _) (t, h)                                         = (go tt t, h)
    go _ _
      = error "toIxShapeRepr: not a valid Data.Ix index"


-- Dig out the underlying pointers of the Accelerate SoA data structure
--

withComplexArrayPtrs
    :: forall sh e a. IsFloating e
    => Array sh (Complex e)
    -> (ArrayPtrs e -> ArrayPtrs e -> IO a)
    -> IO a
withComplexArrayPtrs (Array _ adata) k
  | AD_Pair (AD_Pair AD_Unit ad1) ad2 <- adata
  = case floatingType :: FloatingType e of
      TypeFloat{}   -> withArrayData arrayElt ad1 $ \p1 -> withArrayData arrayElt ad2 $ \p2 -> k p1 p2
      TypeDouble{}  -> withArrayData arrayElt ad1 $ \p1 -> withArrayData arrayElt ad2 $ \p2 -> k p1 p2
      TypeCFloat{}  -> withArrayData arrayElt ad1 $ \p1 -> withArrayData arrayElt ad2 $ \p2 -> k p1 p2
      TypeCDouble{} -> withArrayData arrayElt ad1 $ \p1 -> withArrayData arrayElt ad2 $ \p2 -> k p1 p2

-- withScalarArrayPtrs
--     :: forall sh e a. IsFloating e
--     => Array sh e
--     -> (ArrayPtrs e -> IO a)
--     -> IO a
-- withScalarArrayPtrs (Array _ adata) =
--   case floatingType :: FloatingType e of
--     TypeFloat{}   -> withArrayData arrayElt adata
--     TypeDouble{}  -> withArrayData arrayElt adata
--     TypeCFloat{}  -> withArrayData arrayElt adata
--     TypeCDouble{} -> withArrayData arrayElt adata

withArrayData
    :: (ArrayPtrs e ~ Ptr a)
    => ArrayEltR e
    -> ArrayData e
    -> (Ptr a -> IO b)
    -> IO b
withArrayData ArrayEltRfloat   (AD_Float   ua) = withUniqueArrayPtr ua
withArrayData ArrayEltRdouble  (AD_Double  ua) = withUniqueArrayPtr ua
withArrayData ArrayEltRcfloat  (AD_CFloat  ua) = withUniqueArrayPtr ua
withArrayData ArrayEltRcdouble (AD_CDouble ua) = withUniqueArrayPtr ua
withArrayData _ _ =
  $internalError "withArrayData" "expected array of [C]Float or [C]Double"

withIntArray :: EltRepr e ~ Int => Array sh e -> (Ptr Int -> IO a) -> IO a
withIntArray (Array _ (AD_Int ua)) = withUniqueArrayPtr ua

withShapeArray :: Array sh DIM1 -> (Ptr Int -> IO a) -> IO a
withShapeArray (Array _ (AD_Pair AD_Unit (AD_Int ua))) = withUniqueArrayPtr ua
-}