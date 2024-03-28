module Main (main) where
-- import Test.Tasty
import Test.Tasty.Bench
import Lib
import Data.Char (digitToInt)

testA, testB :: Integer
testA = 129394949293294234923492394293492349239429431111323423423423413123123123123123123111133
testB = 129394949293294234923492

testDigitToInt :: Integer -> Int
testDigitToInt = sum . map digitToInt . show

testReadPure :: Integer -> Int
testReadPure = sum . map (read . pure) . show

main :: IO ()
main =
	defaultMain
    [ bgroup
        "sum digits of number"
        [ bench "test DigitToInt" $ nf testDigitToInt testA,
          bench "test ReadPure" $ nf testReadPure testA,
         bench "test DigitToInt testB" $ nf testDigitToInt testB,
          bench "test ReadPure testB" $ nf testReadPure testB]
    ]

