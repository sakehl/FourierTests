{-# LANGUAGE BangPatterns #-}
module ReadFile where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Array.Accelerate      as A hiding (fromInteger, fromRational, fromIntegral)
import qualified Data.Array.Accelerate.Data.Complex as A
import qualified Data.Array.Accelerate.IO   as A
import qualified Data.Vector.Storable       as U
import System.Environment

readFiles :: Int -> Int -> IO (A.Matrix Int)
readFiles m n = readArrayFile ("data/list_" ++ show m ++ "_" ++ show n ++ ".in") (A.Z A.:. n A.:. m)

readFilesFourier :: Int -> Int -> IO (A.Array A.DIM3 Double)
readFilesFourier m n = readArrayFileD ("data/list_" ++ show m ++ "_" ++ show m ++ "_" ++ show n ++ ".in") (A.Z A.:. n A.:. m A.:. m)

readArrayFile :: (A.Shape sh) => String -> sh -> IO (A.Array sh Int)
readArrayFile f sh = do
    s   <- L.readFile f
    return . A.fromVectors sh . parse $ s

readArrayFileD :: (A.Shape sh) => String -> sh -> IO (A.Array sh Double)
readArrayFileD f sh = do
    s   <- L.readFile f
    return . A.fromVectors sh . parse2 $ s

-- Fill a new vector from a file containing a list of numbers.
parse = U.unfoldr step
  where
    step !s = case L.readInt s of
        Nothing -> case (L.null s,L.head s) of
            (True, _) -> Nothing
            (_, '[')  -> step (L.tail s)
            (_, ']')  -> step (L.tail s)
            (_, ',')  -> step (L.tail s)
            (_, ' ')  -> step (L.tail s)
            _   -> Nothing
        Just (!k, !t) -> Just (k, L.tail t)

parse2 :: L.ByteString -> U.Vector Double
parse2 = U.unfoldr step
  where
    step !s = case L.readInt s of
        Nothing -> case (L.null s,L.head s) of
            (True, _) -> Nothing
            (_, '[')  -> step (L.tail s)
            (_, ']')  -> step (L.tail s)
            (_, ',')  -> step (L.tail s)
            (_, ' ')  -> step (L.tail s)
            _   -> Nothing
        Just (!k, !t) -> Just (fromIntegral k, L.tail t)


