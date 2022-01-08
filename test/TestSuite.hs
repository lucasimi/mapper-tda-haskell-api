module Main where
import Data.BallTree.CircleTree.SingletonLeafTest
import Data.QuickSelectTest

main :: IO ()
main = do
    internalTestSuite
    quickSelectTestSuite 