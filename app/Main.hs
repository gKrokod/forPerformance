{-# LANGUAGE BangPatterns #-}
module Main (main) where
-- import Test.Tasty
import Test.Tasty.Bench
import Lib
import Data.Char (digitToInt)

testA, testB :: Integer
testA = 5 
testB = 15
testC = 20

ver1 :: Integer -> Integer
ver1 0 = 0
ver1 n = sum $ map ( \ i -> ver1 (i-1) + 3 * i ) [1..n]

ver2 :: Integer -> Integer
ver2 = (map findmemo [0..] !!)  . fromIntegral
  where
    findmemo 0 = 0
    findmemo n = sum $ map ( \ i -> ver2 (i-1) + 3 * i ) [1..n]

ver3 :: Integer -> Integer
ver3 0 = 0
ver3 1 = 3
ver3 n = 2 * ver3 (n - 1) + 3 * n

ver4 :: Integer -> Integer
ver4 =( ver4' !! ) . fromIntegral

ver4' :: [Integer]
ver4' = 0 : zipWith (+) (map (*2) ver4') ([3,6..])

ver5 :: Integer -> Integer
ver5 k = ver5' k 0 k

ver5' :: Integer -> Integer -> Integer -> Integer
ver5' (-1) acc k = acc
ver5' n acc k = ver5' (pred n) (2 * acc + fromIntegral (3 * (k - n))) k 

ver6 :: Integer -> Integer 
ver6 = (\(a,b) -> 3 * (a + b)) . ((iterate (\(a,b) -> (a + b, 1 + 2 * b)) (0,1)) !!) . pred . fromIntegral

ver7 :: Integer -> Integer
ver7 n = go n (0,1) 
  where
    go :: Integer -> (Integer, Integer) -> Integer 
    go !n (!a, !b) | n==0      = 3 * (a + b) 
                   | otherwise = go (n-1) (a + b, 1 + 2 * b)

main :: IO ()
main =
	defaultMain
    [ bgroup
        ("x func for " ++ show testA)
        [ bench "test ver1" $ nf ver1 testA,
          bench "test ver2" $ nf ver2 testA,
          bench "test ver3" $ nf ver3 testA,
          bench "test ver4" $ nf ver4 testA,
          bench "test ver5" $ nf ver5 testA,
          bench "test ver6" $ nf ver6 testA,
          bench "test ver7" $ nf ver7 testA],
     bgroup
        ("x func for " ++ show testB)
        [ bench "test ver1" $ nf ver1 testB,
          bench "test ver2" $ nf ver2 testB,
          bench "test ver3" $ nf ver3 testB,
          bench "test ver4" $ nf ver4 testB,
          bench "test ver5" $ nf ver5 testB,
          bench "test ver6" $ nf ver6 testB,
          bench "test ver7" $ nf ver7 testB],
     bgroup
        ("x func for " ++ show testC)
        [ bench "test ver1" $ nf ver1 testC,
          bench "test ver2" $ nf ver2 testC,
          bench "test ver3" $ nf ver3 testC,
          bench "test ver4" $ nf ver4 testC,
          bench "test ver5" $ nf ver5 testC,
          bench "test ver6" $ nf ver6 testC,
          bench "test ver7" $ nf ver7 testC]
    ]


