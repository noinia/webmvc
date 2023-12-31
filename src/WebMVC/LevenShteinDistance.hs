module WebMVC.LevenShteinDistance
  ( levenShteinDistance
  ) where

import qualified Data.Array as Array
import           Data.Array (Ix, Array, (!))

import Debug.Trace

--------------------------------------------------------------------------------

type Distance = Int

data Operation = Insert { elemIx :: Int -- ^ insert old ! elemIx
                        , at     :: Int -- ^ just before the element at newIndex
                        }
               | Replace { at :: Int
                         , by :: Int -- index
                         }
               | Delete Int      -- ^ delete the element i


-- | computes the levenShteinDistance
levenShteinDistance         :: Eq a => [a] -> [a] -> Int
levenShteinDistance []  new = length new
levenShteinDistance old []  = length old
levenShteinDistance old new = lev old' new' (length old', length new')
  where
    old' = Array.listArray (0, length old - 1) old
    new' = Array.listArray (0, length new - 1) new


lev         :: Eq a => Array.Array Int a -> Array.Array Int a -> (Int,Int) -> Distance
lev old new = lev'
  where
    -- the argument represents suffixes of lengths i and j of old and new respectively.
    lev' :: (Int,Int) -> Distance
    lev' = memo ((0,0),(n+1,m+1)) $ \case
      (0,j)                              -> j
      (i,0)                              -> i
      (i,j) | old ! (n-i) == new ! (m-j) -> lev' (i-1, j-1)
            | otherwise                  -> 1 + minimum [ lev' (i-1, j-1) -- replace old ! i by new  ! j
                                                        , lev' (i,   j-1) --
                                                        , lev' (i-1, j)
                                                        ]
    n = length old
    m = length new

-- add memoization, see
-- https://byorgey.wordpress.com/2023/06/06/dynamic-programming-in-haskell-automatic-memoization/

memo     :: Ix i=> (i,i) -> (i -> a) -> (i -> a)
memo rng = (!) . tabulate rng

tabulate       :: Ix i => (i,i) -> (i -> a) -> Array i a
tabulate rng f = Array.listArray rng . map f $ Array.range rng


--------------------------------------------------------------------------------
