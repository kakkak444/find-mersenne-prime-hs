{-# LANGUAGE MagicHash #-}

module Main
    ( main
    ) where

import Numeric.Natural (Natural)

import MillerRabin
import LucasLehmer

import Data.Time.Clock

testCase :: Integral a => a
{-# SPECIALISE testCase :: Integer #-}
{-# SPECIALISE testCase :: Natural #-}
testCase = 2 ^ (521 :: Word) - 1

main :: IO ()
-- main = do
--     putStrLn "input number"
--     n <- read <$> getLine
--     print $ millerTest n
main = do
    putStrLn "start measure:"

    start <- getCurrentTime
    -- let !m = parMillerTest $ toInteger testCase
    let !m = millerTestNat testCase
    -- let !m = millerTest testCase
    end <- getCurrentTime

    s1 <- getCurrentTime
    let !m1 = lucasLehmerTestFast# 521##
    e1 <- getCurrentTime

    putStrLn $ "millerRabin"
    putStrLn $ "result: " <> show m
    putStrLn $ "elapsed: " <> show (diffUTCTime end start)

    putStrLn $ "lucasLehmer"
    putStrLn $ "result: " <> show m1
    putStrLn $ "elapsed: " <> show (diffUTCTime e1 s1)
