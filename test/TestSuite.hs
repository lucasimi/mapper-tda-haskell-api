module Main where
import BallTree.InternalTestSuite (internalTestSuite)
import Test.QuickCheck (quickCheck)

main :: IO ()
main = do
    internalTestSuite