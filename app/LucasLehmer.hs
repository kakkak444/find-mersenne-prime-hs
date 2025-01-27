{-# LANGUAGE MagicHash #-}

module LucasLehmer
    ( lucasLehmerTest
    , lucasLehmerTestFast
    , lucasLehmerTestFast#
    ) where

import GHC.Num.Integer
import GHC.Num.Natural
import GHC.Base

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
