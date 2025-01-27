{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE MagicHash #-}

module MillerRabin
    ( millerTest
    , millerTestNat
    , millerTestWord
    , parMillerTest
    , parMillerTestNat
    ) where


import Data.List (iterate')

import Control.Parallel.Strategies
import qualified Data.Vector.Fusion.Bundle as B

import GHC.Num.Natural
import GHC.Num.BigNat
import GHC.Num.Integer
import GHC.Base


even' :: Integer -> Bool
{-# INLINE even' #-}
even' !n = not (isTrue# (integerTestBit# n 0##))

even'' :: Natural -> Bool
{-# INLINE even'' #-}
even'' !n = not (isTrue# (naturalTestBit# n 0##))

iterateN' :: Int -> (a -> a) -> a -> [a]
{-# INLINE iterateN' #-}
iterateN' n f init' = take n $ iterate' f init'

div2 :: Integer -> Integer
{-# INLINE div2 #-}
div2 = (`integerShiftR#` 1##)

div2' :: Natural -> Natural
{-# INLINE div2' #-}
div2' = (`naturalShiftR#` 1##)

double :: (Num a) => a -> a
{-# INLINE double #-}
{-# SPECIALISE double :: Integer -> Integer #-}
{-# SPECIALISE double :: Int -> Int #-}
{-# SPECIALISE double :: Word -> Word #-}
{-# SPECIALISE double :: Natural -> Natural #-}
double !x = x * x

naturalCountLeadingZeros :: Natural -> Maybe Word
{-# INLINE naturalCountLeadingZeros #-}
naturalCountLeadingZeros n =
    case naturalCountLeadingZeros# n of
        (# (#  #) | #) -> Nothing
        (# | w# #)     -> Just (W# w#)

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
expMod !base !ex !m = go 1 base ex
  where
    go :: Integer -> Integer -> Integer -> Integer
    {-# INLINE go #-}
    go !ans !base' !ex'
        | ex' == 0 = ans
        | even' ex' = go ans (square base') (div2 ex')
        | otherwise = go ((ans * base') `mod` m) (square base') (div2 ex')

    square :: Integer -> Integer
    {-# INLINE square #-}
    square = (`mod` m) . double

expModNat :: Natural -> Natural -> Natural -> Natural
{-# INLINE expModNat #-}
expModNat = naturalPowMod

expModWord :: Word -> Word -> Word -> Word
{-# INLINE expModWord #-}
expModWord (W# b#) (W# ex#) (W# m#) = W# (expModWord# b# ex# m#)

expModWord# :: Word# -> Word# -> Word# -> Word#
{-# INLINE expModWord# #-}
expModWord# = powModWord#

factor2 :: Integer -> (Int, Integer)
{-# INLINE factor2 #-}
factor2 !n = case go (# 0#, n #) of
        (# s#, d #) -> (I# s#, d)
    where
        go :: (# Int#, Integer #) -> (# Int#, Integer #)
        {-# INLINE go #-}
        go (# !s, !d #)
            | even' d = go (# s +# 1#, div2 d #)
            | otherwise = (# s, d #)

factor2Nat :: Natural -> (Word, Natural)
{-# INLINE factor2Nat #-}
factor2Nat !n = case factor2Nat# n of
    (# s#, d #) -> (W# s#, d)

factor2Nat# :: Natural -> (# Word#, Natural #)
{-# INLINE factor2Nat# #-}
factor2Nat# !n = case naturalCountLeadingZeros# n of
    (# (#  #) | #) -> (# 0##, n #)
    (# | s# #)     -> (# s# , naturalShiftR# n s# #)

factor2Word# :: Word# -> (# Word#, Word# #)
{-# INLINE factor2Word# #-}
factor2Word# w#
    | isTrue# (w# `eqWord#` 0##) = (# 0##, 0## #)
    | otherwise =
        let s# = ctz# w#
        in
            (# s#, w# `uncheckedShiftRL#` (word2Int# s#) #)

miller :: Integer -> Integer -> Bool
{-# INLINE miller #-}
miller !n !a =
    let !(!s, !d) = factor2 $! n - 1
        !aExpD = expMod a d n
    in
        aExpD == 1 || (n - 1) `elem` (iterateN' s ((`mod` n) . double) aExpD)

millerNat :: Natural -> Natural -> Bool
{-# INLINE millerNat #-}
millerNat !n !a = case factor2Nat# $! n - 1 of
    (# s#, d #) ->
        let !aExpD = expModNat a d n
        in
            aExpD == 1 || (n - 1) `elem` (iterateN' (I# $ word2Int# s#) (\x -> expModNat x 2 n) aExpD)

millerWord# :: Word# -> Word# -> Bool
{-# INLINE millerWord# #-}
millerWord# n# a# = case factor2Word# (n# `minusWord#` 1##) of
    (# s#, d# #) ->
        let aExpD# = expModWord# a# d# n#
            nM1#  = n# `minusWord#` 1##
            go i# acc#
                | isTrue# (i# `eqWord#` 0##) = False
                | isTrue# (acc# `eqWord#` nM1#) = True
                | otherwise = go (i# `minusWord#` 1##) (powModWord# acc# 2## n#)
        in
            isTrue# (aExpD# `eqWord#` 1##) || go s# aExpD#

millerWord :: Word -> Word -> Bool
{-# INLINE millerWord #-}
millerWord (W# n#) (W# a#) = millerWord# n# a#

millerV :: Integer -> Integer -> Bool
{-# INLINE millerV #-}
millerV !n !a =
    let !(!s, !d) = factor2 $! n - 1
        !aExpD = expMod a d n
    in
        aExpD == 1 || (n - 1) `B.elem` B.iterateN s ((`mod` n) . double) aExpD

check :: [Integer] -> Integer -> Bool
{-# INLINE check #-}
check list !n = all (miller n) list

checkNat :: [Natural] -> Natural -> Bool
{-# INLINE checkNat #-}
checkNat list !n = all (millerNat n) list

checkWord :: [Word] -> Word -> Bool
{-# INLINE checkWord #-}
checkWord list !n = all (millerWord n) list

checkV :: [Integer] -> Integer -> Bool
{-# INLINE checkV #-}
checkV list !n = all (millerV n) list

type BufLen = Int

parCheck :: BufLen -> [Integer] -> Integer -> Bool
{-# INLINE parCheck #-}
parCheck !bufLen xs !n = foldl' (&&) True $ withStrategy strat go
  where
    go = map (miller n) xs
    strat = parBuffer bufLen rseq

parCheckNat :: BufLen -> [Natural] -> Natural -> Bool
{-# INLINE parCheckNat #-}
parCheckNat !bufLen xs !n = foldl' (&&) True $ withStrategy strat go
  where
    go = map (millerNat n) xs
    strat = parBuffer bufLen rseq

millerTest :: Integer -> Bool
{-# INLINE millerTest #-}
millerTest n
    | n < 4759123141 = checkV [2, 7, 61] n
    | n < 341550071728321 = checkV [2, 3, 5, 7, 11, 13, 17] n
    | n < 18446744073709551615 = checkV [2, 325, 9375, 28178, 450775, 9780504, 1795265022] n
    | otherwise =
        let !maxElem = min (n - 1) $! truncate (3 * double (log $! fromInteger n) :: Double)
        in
            checkV [2..maxElem] n

millerTestNat :: Natural -> Bool
{-# INLINE millerTestNat #-}
millerTestNat (NS w#) = millerTestWord (W# w#)
millerTestNat n =
        let !maxElem = min (n - 1) $! truncate (3 * double (log $! fromIntegral n) :: Double)
        in
            checkNat [2..maxElem] n

millerTestWord :: Word -> Bool
{-# INLINE millerTestWord #-}
millerTestWord n
    | n < 4759123141 = checkWord [2, 7, 61] n
    | n < 341550071728321 = checkWord [2, 3, 5, 7, 11, 13, 17] n
    | otherwise = checkWord [2, 325, 9375, 28178, 450775, 9780504, 1795265022] n

parMillerTest :: Integer -> Bool
{-# INLINE parMillerTest #-}
parMillerTest n
    | n < 4759123141 = checkV [2, 7, 61] n
    | n < 341550071728321 = checkV [2, 3, 5, 7, 11, 13, 17] n
    | n < 18446744073709551615 = checkV [2, 325, 9375, 28178, 450775, 9780504, 1795265022] n
    | otherwise =
        let !maxElem = min (n - 1) $! truncate (3 * double (log $! fromInteger n) :: Double)
            !bufLen  = 1000
        in
            parCheck bufLen [2..maxElem] n

parMillerTestNat :: Natural -> Bool
{-# INLINE parMillerTestNat #-}
parMillerTestNat n
    | n < 4759123141 = checkNat [2, 7, 61] n
    | n < 341550071728321 = checkNat [2, 3, 5, 7, 11, 13, 17] n
    | n < 18446744073709551615 = checkNat [2, 325, 9375, 28178, 450775, 9780504, 1795265022] n
    | otherwise =
        let !maxElem = min (n - 1) $! truncate (3 * double (log $! fromIntegral n) :: Double)
            !bufLen  = 1000
        in
            parCheckNat bufLen [2..maxElem] n
