{-# LANGUAGE MagicHash #-}

module Main
    ( main
    ) where

import Control.Monad (when)
import Data.Bool     (bool)

import MillerRabin
import LucasLehmer

import Data.Time.Clock
import qualified Data.Array.Repa       as R
import qualified Data.Array.Repa.Repr.HintInterleave as R (hintInterleave)
import qualified Data.Vector.Unboxed   as V

chunk :: Int
chunk = 2000

main :: IO ()
main = go 3

go :: Word -> IO ()
{-# INLINE go #-}
go !begin = do
    let !arr = R.fromListUnboxed (R.Z R.:. chunk) $ take chunk $ filter millerTestWord [begin,begin+2..]
        !end = arr R.! ( R.Z R.:. (chunk - 1) )

    putStrLn $ "searching [" <> show begin <> "," <> show end <> "] ...:"
    !startTime <- getCurrentTime
    !res <- R.computeUnboxedP $ R.hintInterleave $ R.map lucasLehmerTestFast arr
    !endTime <- getCurrentTime

    putStrLn "result:"
    V.mapM_ (\(!(!n, !b)) -> when b (putStr $ show n <> ", ")) $ V.zip (R.toUnboxed arr) (R.toUnboxed res)
    putChar '\n'

    putStrLn $ "found " <> show (R.sumAllS $ R.map (bool 0 1) res :: Int) <> " MPrimes"

    putChar '\n'
    putStrLn $ "elapsed: " <> show (diffUTCTime endTime startTime)
    putChar '\n'

    go (end + 2)
