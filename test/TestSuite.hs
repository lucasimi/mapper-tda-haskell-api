module Main where
import Test.QuickCheck
import BallTree.InternalTestSuite
import UtilsTestSuite

main :: IO ()
main = do
    internalTestSuite
    utilsTestSuite