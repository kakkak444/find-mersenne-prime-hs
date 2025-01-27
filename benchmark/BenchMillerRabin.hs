{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}

module Main
    ( main
    ) where

import GHC.Base
import GHC.Num
import GHC.Num.BigNat

import Data.List (iterate')
import Control.DeepSeq (force)
import Control.Parallel.Strategies

import Data.Time.Clock
import qualified Data.Vector.Fusion.Bundle as B

testCase :: Integral a => a
{-# SPECIALISE testCase :: Integer #-}
{-# SPECIALISE testCase :: Natural #-}
testCase = 2 ^ 521 - 1

main :: IO ()
main = do
    start0 <- getCurrentTime
    let !m0 = millerTest testCase
    end0 <- getCurrentTime
    putStrLn $ "millerTest " ++ show (diffUTCTime end0 start0)

    -- start1 <- getCurrentTime
    -- let !m1 = millerTest' testCase
    -- end1 <- getCurrentTime
    -- putStrLn $ "millerTest' " ++ show (diffUTCTime end1 start1)

    -- start2 <- getCurrentTime
    -- let !m2 = parMillerTest testCase
    -- end2 <- getCurrentTime
    -- putStrLn $ "parMillerTest " ++ show (diffUTCTime end2 start2)

    start3 <- getCurrentTime
    let !m3 = millerTestV testCase
    end3 <- getCurrentTime
    putStrLn $ "millerTestV " ++ show (diffUTCTime end3 start3)

    start4 <- getCurrentTime
    let !m4 = millerTestNat testCase
    end4 <- getCurrentTime
    putStrLn $ "millerTestNat " ++ show (diffUTCTime end4 start4)

    putStrLn "result:"
    putStrLn $ "millerTest   : " ++ show m0
    -- putStrLn $ "millerTest'  : " ++ show m1
    -- putStrLn $ "parMillerTest: " ++ show m2
    putStrLn $ "millerTestV  : " ++ show m3
    putStrLn $ "millerTestNat: " ++ show m4


millerTest :: Integer -> Bool
{-# INLINE millerTest #-}
millerTest !n
    | n < 4759123141 = millerRabinTest [2, 7, 61] n
    | n < 341550071728321 = millerRabinTest [2, 3, 5, 7, 11, 13, 17] n
    | otherwise =
        let double !x = x * x
            maxElem = min (n - 1) $! truncate $! (3 * double (log $! fromInteger n) :: Double)
        in
            millerRabinTest [2..maxElem] n

millerTest' :: Integer -> Bool
{-# INLINE millerTest' #-}
millerTest' !n
    | n < 4759123141 = millerRabinTest' [2, 7, 61] n
    | n < 341550071728321 = millerRabinTest' [2, 3, 5, 7, 11, 13, 17] n
    | otherwise =
        let double !x = x * x
            maxElem = min (n - 1) $! truncate $! (3 * double (log $! fromInteger n) :: Double)
        in
            millerRabinTest' [2..maxElem] n

millerTestNat :: Natural -> Bool
{-# INLINE millerTestNat #-}
millerTestNat !n
    | n < 4759123141 = millerRabinTestNat [2, 7, 61] n
    | n < 341550071728321 = millerRabinTestNat [2, 3, 5, 7, 11, 13, 17] n
    | otherwise =
        let double !x = x * x
            maxElem = min (n - 1) $! truncate $! (3 * double (log $! fromIntegral n) :: Double)
        in
            millerRabinTestNat [2..maxElem] n

millerTestV :: Integer -> Bool
{-# INLINE millerTestV #-}
millerTestV !n
    | n < 4759123141 = millerRabinTestV (B.unsafeFromList 3 [2, 7, 61]) n
    | n < 341550071728321 = millerRabinTestV (B.unsafeFromList 7 [2, 3, 5, 7, 11, 13, 17]) n
    | otherwise =
        let double !x = x * x
            maxElem = min (n - 1) $! truncate $! (3 * double (log $! fromInteger n) :: Double)
        in
            millerRabinTestV (B.fromList [2..maxElem]) n

parMillerTest :: Integer -> Bool
{-# INLINE parMillerTest #-}
parMillerTest !n
    | n < 4759123141 = millerRabinTest [2, 7, 61] n
    | n < 341550071728321 = millerRabinTest [2, 3, 5, 7, 11, 13, 17] n
    | otherwise =
        let double !x = x * x
            !maxElem = min (n - 1) $! truncate $! (3 * double (log $! fromInteger n) :: Double)
        in
            parMillerRabinTest [2..maxElem] n

millerRabinTest :: [Integer] -> Integer -> Bool
{-# INLINE millerRabinTest #-}
millerRabinTest xs !n = all (miller n) xs

millerRabinTest' :: [Integer] -> Integer -> Bool
{-# INLINE millerRabinTest' #-}
millerRabinTest' xs !n = all (miller' n) xs

millerRabinTestNat :: [Natural] -> Natural -> Bool
{-# INLINE millerRabinTestNat #-}
millerRabinTestNat xs !n = all (millerNat n) xs

millerRabinTestV :: B.Bundle v Integer -> Integer -> Bool
{-# INLINE millerRabinTestV #-}
millerRabinTestV xs !n = B.foldl' (&&) True $ B.map (millerV n) xs

parMillerRabinTest :: [Integer] -> Integer -> Bool
{-# INLINE parMillerRabinTest #-}
-- parMillerRabinTest xs !n = foldl' (&&) True $ parMap rdeepseq (miller' n) xs
-- parMillerRabinTest xs !n = runEval $ rMillerRabinTest xs n
parMillerRabinTest xs !n = foldl' (&&) True $ withStrategy strat go
  where
    bufferLen :: Int
    !bufferLen = 1000

    go = map (miller' n) xs

    strat = parBuffer bufferLen rdeepseq
    -- strat = parList rdeepseq

miller :: Integer -> Integer -> Bool
{-# INLINE miller #-}
miller !n !a =
    let (# !s, !d #) = decompose $! n - 1
        !aExpD = expMod a d n
        double !x = expMod x 2 n
    in
        aExpD == 1 || (n - 1) `elem` take s (iterate' double aExpD)

miller' :: Integer -> Integer -> Bool
{-# INLINE miller' #-}
miller' !n !a = decomposeCPS (n - 1) $ \(!s) (!d) ->
        let !aExpD = expMod a d n
            double !x = expMod x 2 n
        in
            aExpD == 1 || (n - 1) `elem` take s (iterate' double aExpD)

millerNat :: Natural -> Natural -> Bool
{-# INLINE millerNat #-}
millerNat !n !a =
    let (# s#, d #) = decompose# $! n - 1
        !aExpD = expMod' a d n
        double !x = expMod' x 2 n
    in
        aExpD == 1 || (n - 1) `elem` take (I# $ word2Int# s#) (iterate' double aExpD)

millerV :: Integer -> Integer -> Bool
{-# INLINE millerV #-}
millerV !n !a =
    let (# !s, !d #) = decompose $! n - 1
        !aExpD = expMod a d n
        double !x = expMod x 2 n
    in
        aExpD == 1 || (n - 1) `B.elem` B.iterateN s double aExpD

even' :: Integer -> Bool
{-# INLINE even' #-}
even' !n = not (isTrue# (integerTestBit# n 0##))

even'' :: Natural -> Bool
{-# INLINE even'' #-}
even'' !n = not (isTrue# (naturalTestBit# n 0##))

div2 :: Integer -> Integer
{-# INLINE div2 #-}
div2 = (`integerShiftR#` 1##)

div2' :: Natural -> Natural
{-# INLINE div2' #-}
div2' = (`naturalShiftR#` 1##)

naturalCountLeadingZeros# :: Natural -> (# (#  #) | Word# #)
{-# INLINE naturalCountLeadingZeros# #-}
naturalCountLeadingZeros# (NS w#)
    | isTrue# (w# `eqWord#` 0##) = (# (#  #) | #)
    | otherwise = (# | ctz# w# #)
naturalCountLeadingZeros# (NB bignat#) =
    let
        size# = bigNatSize# bignat#

        {-# INLINE go #-}
        go acc# idx#
            | isTrue# (idx# >=# size#) = acc#
            | otherwise =
                let w# = bigNatIndex# bignat# idx#
                in
                    if isTrue# (w# `neWord#` 0##) then
                        acc# `plusWord#` ctz# w#
                    else
                        go (acc# `plusWord#` 64##) (idx# +# 1#)
    in
        (# | go 0## 0# #)

expMod :: Integer -> Integer -> Integer -> Integer
{-# INLINE expMod #-}
expMod = expMod' 1
  where
    expMod' !ans !base !ex !m
        | ex == 0 = ans
        | even' ex = expMod' ans (square base) (div2 ex) m
        | otherwise = expMod' ((ans * base) `mod` m) (square base) (div2 ex) m
        where
          square !x = (x * x) `mod` m

expMod' :: Natural -> Natural -> Natural -> Natural
{-# INLINE expMod' #-}
expMod' = naturalPowMod

decompose :: Integer -> (# Int, Integer #)
{-# INLINE decompose #-}
decompose !n = decomp' (# 0, n #)
  where
    decomp' (# !s, !d #)
        | even' d = decomp' (# s + 1, div2 d #)
        | otherwise = (# s, d #)

decomposeCPS :: Integer -> (Int -> Integer -> r) -> r
decomposeCPS !n cont = decomp' 0 n
  where
    decomp' !s !d
        | even' d = decomp' (s + 1) (div2 d)
        | otherwise = cont s d

decompose# :: Natural -> (# Word#, Natural #)
{-# INLINE decompose# #-}
decompose# !n = case naturalCountLeadingZeros# n of
    (# (#  #) | #) -> (# 0##, n #)
    (# | s# #)     -> (# s#, naturalShiftR# n s# #)
