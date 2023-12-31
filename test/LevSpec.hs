module LevSpec(spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import WebMVC.LevenShteinDistance

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "LevenShteinDistance tests" $ do
         modifyMaxSize (const 15) $
           prop "same as naive" $ \as (bs :: [Bool]) ->
             levenShteinDistance as bs === levenShteinDistanceNaive as bs

--------------------------------------------------------------------------------

-- | computes the levenShteinDistance in a navie direct way
levenShteinDistanceNaive :: Eq a => [a] -> [a] -> Int
levenShteinDistanceNaive = levD
  where
    levD []         bs = length bs
    levD as         [] = length as
    levD as@(a:as') bs@(b:bs')
      | a == b    = levD as' bs'
      | otherwise = 1 + minimum [ levD as bs'
                                , levD as' bs
                                , levD as' bs'
                                ]
