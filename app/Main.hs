{-# LANGUAGE BangPatterns #-}
module Main (main) where
-- import Test.Tasty
import Test.Tasty.Bench
import Lib
import Data.Char (digitToInt)
import Numeric

testA, testB, testC, testD :: Integer
testA = 5 
testB = 15
testC = 20
testD = 1000000

ver1 :: Integer -> Integer
ver1 0 = 0
ver1 n = sum $ map ( \ i -> ver1 (i-1) + 3 * i ) [1..n]

ver2 :: Integer -> Integer
ver2 = (map findmemo [0..] !!)  . fromIntegral
  where
    findmemo 0 = 0
    findmemo n = mod (sum $ map ( \ i -> ver2 (i-1) + 3 * i ) [1..n]) (10^9 + 7)

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

ver8 :: Int -> Int
ver8 =( ver8' !! ) 

ver8' :: [Int]
ver8' = 0 : zipWith (\a b -> mod (a + b) (10^9 + 7)) (map (*2) ver8') ([3,6..])

ver9 :: Int -> Int
ver9 = (map findmemo [0..] !!) 
  where
    findmemo :: Int -> Int
    findmemo 0 = 0
    findmemo n = mod (sum $ map ( \ i -> ver9 (i-1) + 3 * i ) [1..n]) (10^9 + 7)

ver10 :: Int -> Int
ver10 =( ver10' !! ) 

ver10' :: [Int]
ver10' = 0 : zipWith (\a b -> mod (a + b) (1000000007)) (map (*2) ver10') ([3,6..])

ver11 :: Int -> Int
ver11 n = fromIntegral $ mod (helper n) (10^9 + 7)

helper :: Int -> Integer
helper n = 3 * (-2 + 2^(1 + k) - k)
  where k = toInteger n

ver12 :: Int -> Int
ver12 n = fromIntegral $ mod (helper2 n) (10^9 + 7)

helper2 :: Int -> Integer
helper2 n = 3 * (-2 + (fastPower 2 (1 + n)) - k)
  where k = toInteger n

fastPower :: Int -> Int -> Integer
fastPower a m = helper ms (toInteger a) 1
  where ms = map digitToInt $ showBin m ""
        helper :: [Int] -> Integer ->  Integer -> Integer
        helper [] a acc = acc
        helper (1 : ms) a acc = helper ms a (a * acc^2)
        helper (0 : ms) a acc = helper ms a (acc^2)
        

ver13 :: Int -> Int
ver13 n = fromIntegral $ mod (helper3 n) (10^9 + 7)

fp2 :: Integer -> Int -> Integer
fp2 a 0 = 1
fp2 a 1 = a
fp2 a m | even m = fp2 (a^2) (div m 2)
        | odd m = a * fp2 (a^2) (div (pred m) 2)

modo :: Int -> Int
modo = flip mod (10^9 + 7)
      
helper3 :: Int -> Integer
helper3 n = 3 * (-2 + (fp2 2 (1 + n)) - k)
  where k = toInteger n

ver14 :: Int -> Int
ver14 n =  mod (helper4 n) (10^9 + 7)

helper4 :: Int -> Int
helper4 n = 3 * (-2 + (fp3 2 (1 + n)) - n)
  where k = toInteger n

fp3 :: Int -> Int -> Int
fp3 a 0 = 1
fp3 a 1 = a
fp3 a m | even m = fp3 (modo $ a^2) (div m 2)
        | odd m = modo $ a * fp3 (modo $ a^2) (div (pred m) 2)
main :: IO ()
main =
	defaultMain
   [
    -- [ bgroup
    --     ("x func for " ++ show testA)
    --     [ bench "test ver1" $ nf ver1 testA,
    --       bench "test ver2" $ nf ver2 testA,
    --       bench "test ver3" $ nf ver3 testA,
    --       bench "test ver4" $ nf ver4 testA,
    --       bench "test ver5" $ nf ver5 testA,
    --       bench "test ver6" $ nf ver6 testA,
    --       bench "test ver7" $ nf ver7 testA],
    --  bgroup
    --     ("x func for " ++ show testB)
    --     [ bench "test ver1" $ nf ver1 testB,
    --       bench "test ver2" $ nf ver2 testB,
    --       bench "test ver3" $ nf ver3 testB,
    --       bench "test ver4" $ nf ver4 testB,
    --       bench "test ver5" $ nf ver5 testB,
    --       bench "test ver6" $ nf ver6 testB,
    --       bench "test ver7" $ nf ver7 testB],
    --  bgroup
    --     ("x func for " ++ show testC)
    --     [ bench "test ver1" $ nf ver1 testC,
    --       bench "test ver2" $ nf ver2 testC,
    --       bench "test ver3" $ nf ver3 testC,
    --       bench "test ver4" $ nf ver4 testC,
    --       bench "test ver5" $ nf ver5 testC,
    --       bench "test ver6" $ nf ver6 testC,
    --       bench "test ver7" $ nf ver7 testC],
     -- bgroup
     --    ("x func for " ++ show testD)
        -- [ bench "test ver2" $ nf ver2 testD,
        -- [ bench "test ver4" $ nf ver4 testD,
         -- [ bench "test ver6" $ nf ver6 testD],
          -- bench "test ver8" $ nf ver8 (fromIntegral testD)],
     bgroup
        ("x func for Int " ++ show testD)
        [ bench "test ver8" $ nf ver8 (fromIntegral testD),
         bench "test ver10" $ nf ver10 (fromIntegral testD),
          bench "test ver11" $ nf ver11 (fromIntegral testD),
          bench "test ver13" $ nf ver13 (fromIntegral testD),
          bench "test ver14" $ nf ver14 (fromIntegral testD)]
    ]


