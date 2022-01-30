module Main where
import Data.BallTree.CircleTreeTest
import Data.QuickSelectTest

main :: IO ()
main = do
    circleTreeTestSuite
    quickSelectTestSuite 