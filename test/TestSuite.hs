module Main where
import BallTree.CircleTree.SingletonLeafTest
import UtilsTest

main :: IO ()
main = do
    internalTestSuite
    utilsTestSuite