module Main (main) where
-- import Test.Tasty
import Test.Tasty.Bench
import Lib
import Data.Char
import Data.List

a = concat
        [ "           \n"
        , "     a     \n"
        , "    e      \n"
        , "           \n"
        , "  d     b  \n"
        , "           \n"
        , "           \n"
        , "     c     \n"

        , "           \n"]

-- digSum :: (Int, Int) -> Int
-- digSum (a, b) = a' + b'
--   where a' = (sum . map digitToInt . show) a
--         b' = (sum . map digitToInt . show) b     
--
--
-- ble :: (Int, Int) -> Int
-- ble (a, c) = helper a 0 + helper c 0
--   where helper :: Int -> Int -> Int
--         helper n acc | n < 10 = n + acc
--                      | otherwise = let (x, y) = divMod n 10 in helper x (acc + y)
bar :: (Int, Int) -> Int
bar (a, b) = a' + b'
  where a' = (sum . map digitToInt . show) a
        b' = (sum . map digitToInt . show) b
        
foo :: (Int, Int) -> Int
foo (a, c) = helper a 0 + helper c 0
  where helper :: Int -> Int -> Int
        helper n acc | n < 10 = n + acc 
                     | otherwise = let (x, y) = divMod n 10 in helper x (acc + y) 

baz :: (Int, Int) -> Int
baz (a, b) = a'
  where a' = (sum . map digitToInt ) (show a ++ show b)

type Matrix = [[Integer]]

sMatrix :: Matrix
sMatrix = [[1,1,1],[1,1,0],[0,1,1]]

sE = [[1,0,0],[0,1,0],[0,0,1]]

multiMatrix :: Matrix -> Matrix -> Matrix
multiMatrix xs ys = transpose (a : b : [c])
  where a = map (multiRowAndColomn d) xs 
        b = map (multiRowAndColomn e) xs 
        c = map (multiRowAndColomn f) xs 
        (d : e : [f]) = transpose ys


multiRowAndColomn :: [Integer] -> [Integer] -> Integer
multiRowAndColomn  = (sum .) . zipWith (*) 

multiMatrixAndVector :: Matrix -> [Integer] -> Matrix
multiMatrixAndVector xs v = map (\x -> pure $ multiRowAndColomn x v) xs 

sqrMatrix :: Matrix -> Int -> Matrix -> Matrix
sqrMatrix xs 0 acc = [[1,0,0],[0,1,0],[0,0,1]]
sqrMatrix xs 1 acc = acc
sqrMatrix xs n acc = sqrMatrix xs (pred n) (multiMatrix acc xs)

get :: Int -> Integer
get x = result !! number
    where (stepen, number) = divMod x 3
          result = reverse $ concat $ multiMatrixAndVector matrixN [1, 1, 1]
          matrixN = sqrMatrix matrix stepen matrix
          matrix = sMatrix

get3 :: Int -> Integer
get3 x = result !! number
    where (stepen, number) = divMod x 3
          result = reverse $ concat $ multiMatrixAndVector matrixN [1, 1, 1]
          matrixN = sqrMatrix' stepen sE sMatrix

get2 :: Int -> Integer
get2 = memoized_pado

memoized_pado :: Int -> Integer
memoized_pado = (map fib [0 ..] !!)
   where fib 0 = 1
         fib 1 = 1
         fib 2 = 1
         fib n = memoized_pado (n-2)
                 + memoized_pado (n-3)

-- main :: IO ()
-- main =
-- 	defaultMain
--     [ bgroup
--         "sum"
--         [ bench "bar" $ nf bar (0, 100000000),
--           bench "foo" $ nf foo (0,100000000),
--           bench "baz" $ nf baz (0,100000000)]
--     ]
 
sqrMatrix' :: Int -> Matrix -> Matrix -> Matrix
sqrMatrix' 0 y z = y
sqrMatrix' n y z | mod n 2 == 0 = sqrMatrix' (div n 2) y (multiMatrix z z)
                 | otherwise = sqrMatrix' (n - 1) (multiMatrix y z) z


sumD :: Integer -> Int
sumD = sum . map digitToInt . show

sumD' :: Integer -> Int
sumD' = helper 0
  where helper :: Int -> Integer -> Int
        helper acc n | n == 0 = acc
                     | n < 10 = helper (acc + fromIntegral n) 0
                     | otherwise = let (x, y) = divMod n 10 in helper (acc + fromIntegral y) x


main :: IO ()
main =
	defaultMain
    [ bgroup
        "get"
        [ bench "sumD sum map" $ nf sumD 100000000,
          bench "sumD' helper" $ nf sumD' 100000000]
    ]

