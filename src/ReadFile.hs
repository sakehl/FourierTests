{-# LANGUAGE BangPatterns #-}
module ReadFile where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Array.Accelerate      as A hiding (fromInteger, fromRational, fromIntegral)
import qualified Data.Array.Accelerate.IO.Data.Vector.Storable as A
import qualified Data.Vector.Storable       as U
import System.Environment

readArrayFile :: (A.Shape sh) => String -> sh -> IO (A.Array sh Int)
readArrayFile f sh = do
    -- [f] <- getArgs
    s   <- L.readFile f
    -- let s = L.pack "[12, 3]"
    return . A.fromVectors sh . parse $ s

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



