module WebMVC.LevenShteinDistance
  ( levenShteinDistance
  , levenShteinDistanceWith

  , Operation(..)
  ) where

import           Control.DeepSeq
import           Data.Array (Ix, Array, (!))
import qualified Data.Array as Array
import           GHC.Generics (Generic)

--------------------------------------------------------------------------------


data Operation = Insert { elemIx :: {-# UNPACK #-}!Int -- ^ insert old ! elemIx
                        , at     :: {-# UNPACK #-}!Int -- ^ just before the element at newIndex
                        }
               | Replace { at :: {-# UNPACK #-}!Int
                         , by :: {-# UNPACK #-}!Int -- index
                         }
               | Delete {-# UNPACK #-}!Int      -- ^ delete the element i
               deriving (Show,Eq,Generic)

instance NFData Operation

-- | computes the levenShteinDistance
--
-- \(O(nm)\)
levenShteinDistance         :: Eq a => [a] -> [a] -> Int
levenShteinDistance old new = getDist $ levenShteinDistanceWith' old new

-- | computes the levenShteinDistance, as well as the edits. Note that the list of edits
-- is forced, so using 'levenShteinDistance' may be slightly more efficient.
--
--
-- \(O(nm)\)
levenShteinDistanceWith         :: Eq a => [a] -> [a] -> (Int, [Operation])
levenShteinDistanceWith old new = force . toTuple $ levenShteinDistanceWith' old new
  -- we force the result so that we don't leak the entire DP table.


-- | Implementation of the levenShteinDistance
levenShteinDistanceWith'         :: Eq a => [a] -> [a] -> Distance
levenShteinDistanceWith' old new = lev old' new' (length old', length new')
  where
    old' = Array.listArray (0, length old - 1) old
    new' = Array.listArray (0, length new - 1) new

data Distance = Dist { getDist    :: {-# UNPACK#-}!Int
                     , _getDeltas :: [Operation]
                     } deriving (Show)

toTuple              :: Distance -> (Int,[Operation])
toTuple (Dist d ops) = (d,ops)

instance Eq Distance where
  (Dist d _) == (Dist d' _) = d == d'
instance Ord Distance where
  (Dist d _) `compare` (Dist d' _) = d `compare` d'

inc :: Distance -> Operation -> Distance
inc (Dist d xs) x = Dist (d+1) (x:xs)

lev         :: Eq a => Array.Array Int a -> Array.Array Int a -> (Int,Int) -> Distance
lev old new = lev'
  where
    -- the argument represents suffixes of lengths i and j of old and new respectively.
    lev' :: (Int,Int) -> Distance
    lev' = memo ((0,0),(n+1,m+1)) $ \case
      (0,j)                              -> Dist j (Insert 0 <$> suffix m j)
      (i,0)                              -> Dist i (Delete   <$> suffix n i)
      (i,j) | old ! (n-i) == new ! (m-j) -> lev' (i-1, j-1)
            | otherwise                  -> minimum
                                            [ lev' (i-1, j-1) `inc` Replace (n-i) (m-j)
                                            , lev' (i,   j-1) `inc` Insert  (n-i) (m-j)
                                            , lev' (i-1, j)   `inc` Delete  (n-i)
                                            ]
    n = length old
    m = length new

suffix     :: Int -> Int -> [Int]
suffix n l = [(n-l)..n-1]

-- add memoization, see
-- https://byorgey.wordpress.com/2023/06/06/dynamic-programming-in-haskell-automatic-memoization/

memo     :: Ix i=> (i,i) -> (i -> a) -> (i -> a)
memo rng = (!) . tabulate rng

tabulate       :: Ix i => (i,i) -> (i -> a) -> Array i a
tabulate rng f = Array.listArray rng . map f $ Array.range rng


--------------------------------------------------------------------------------
