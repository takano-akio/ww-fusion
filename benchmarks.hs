import Control.DeepSeq
import Criterion.Main

import qualified Data.List
import qualified WWFusion

testData :: [Int]
testData = [0..1000]

main :: IO ()
main = rnf testData `seq` defaultMain
  [ bench "Data.List.foldl'" $ whnf (Data.List.foldl' (+) 0) testData
  , bench "WWFusion.foldl'" $ whnf (WWFusion.foldl' (+) 0) testData
  ]
