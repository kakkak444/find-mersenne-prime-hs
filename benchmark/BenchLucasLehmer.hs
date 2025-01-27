{-# LANGUAGE MagicHash #-}

module Main
    ( main
    ) where

import GHC.Num.Integer
import GHC.Num.Natural
import GHC.Base
import Data.Time.Clock

printTime :: String -> NominalDiffTime -> IO ()
printTime str diff = putStrLn $ str ++ " " ++ show diff


main :: IO ()
main = do
    let testCase# :: Word#
        testCase  :: Word

        !testCase# = 44497##
        !testCase  = W# testCase#

    start <- getCurrentTime
    let !m = lucasLehmerTest testCase
    end <- getCurrentTime

    printTime "lucasLehmerTest" $ diffUTCTime end start

    start1 <- getCurrentTime
    let !m1 = lucasLehmerTestFast testCase
    end1 <- getCurrentTime

    printTime "lucasLehmerTestFast" $ diffUTCTime end1 start1

    start2 <- getCurrentTime
    let !m2 = lucasLehmerTestFast# testCase#
    end2 <- getCurrentTime

    printTime "lucasLehmerTestFast#" $ diffUTCTime end2 start2


    putStrLn "result:"
    putStrLn $ "lucasLehmerTest:      " ++ show m
    putStrLn $ "lucasLehmerTestFast:  " ++ show m1
    putStrLn $ "lucasLehmerTestFast#: " ++ show m2

    return ()


lucasLehmerTest :: Word -> Bool
{-# INLINE lucasLehmerTest #-}
lucasLehmerTest !p = go (p - 2) sInit
  where
    !sInit = 4
    !mp = (1 `integerShiftL` p) - 1
    go i !s
        | i == 0 = s == 0
        | otherwise = go (i - 1) ((s * s - 2) `mod` mp)

lucasLehmerTestFast :: Word -> Bool
{-# INLINE lucasLehmerTestFast #-}
lucasLehmerTestFast !p = go (p - 2) sInit
  where
    !sInit = 4
    !m = (1 `integerShiftL` p) - 1
    go i !s
        | i == 0 = s == 0
        | otherwise =
            let !s2 = s * s
                !s' = (s2 `integerAnd` m) + (s2 `integerShiftR` p)
            in
                if s' >= m then
                    go (i - 1) (s' - m - 2)
                else
                    go (i - 1) (s' - 2)

lucasLehmerTestFast# :: Word# -> Bool
{-# INLINE lucasLehmerTestFast# #-}
lucasLehmerTestFast# p# = go (p# `minusWord#` 2##) sInit
  where
    !sInit = 4
    !m = (1 `naturalShiftL#` p#) - 1
    go i# s
        | isTrue# (i# `eqWord#` 0##) = s == 0
        | otherwise =
            let !s2 = s * s
                !s' = (s2 `naturalAnd` m) + (s2 `naturalShiftR#` p#)
            in
                if s' >= m then
                    go (i# `minusWord#` 1##) (s' - m - 2)
                else
                    go (i# `minusWord#` 1##) (s' - 2)