import Control.DeepSeq
import Criterion.Main

import qualified Data.List
import qualified WWFusion

testData :: [Int]
testData = [0..1000]

nestedData :: [[Int]]
nestedData = [[len .. 2 * len] | len <- [0 .. 100]]

main :: IO ()
main = rnf (testData, nestedData) `seq` defaultMain
  [ bench "Data.List.foldl'" $ whnf (Data.List.foldl' (+) 0) testData
  , bench "WWFusion.foldl'" $ whnf (WWFusion.foldl' (+) 0) testData
  , bench "Data.List.filter/concat" $ nf (Data.List.filter even . Data.List.concat) nestedData
  , bench "WWFusion.filter/concat" $ nf (WWFusion.filter even . Data.List.concat) nestedData
  , bench "Data.List.concat/enumFromTo" $ nf
      ( Data.List.concat
      . Data.List.map (\x -> enumFromTo 0 x)
      . enumFromTo (0 :: Int)
      ) 100
  , bench "WWFusion.concat/enumFromTo" $ nf
      ( WWFusion.concat
      . WWFusion.map (\x -> WWFusion.eft 0 x)
      . WWFusion.eft 0
      ) 100
  , bench "Data.List.foldl'/enumFromTo" $ nf (Data.List.foldl' (+) 0 . enumFromTo (0 :: Int)) 100
  , bench "WWFusion.foldl'/enumFromTo" $ nf (WWFusion.foldl' (+) 0 . WWFusion.eft (0 :: Int)) 100
  ]
