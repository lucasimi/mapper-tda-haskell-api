module Main where
import BallTree.CircleTree.SingletonLeafTest
import BallTree.CommonTest

main :: IO ()
main = do
    internalTestSuite
    utilsTestSuite