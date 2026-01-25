module Hopfield where

import System.Random (StdGen, randoms)
import System.Random.Internal (splitGen)
import Data.List (sortOn)

genSchedule :: Int -> StdGen -> ([Int], StdGen)
genSchedule n gen =
  let (g1, g2) = splitGen gen
      -- Generate n random integers using g1
      rands = take n (randoms g1 :: [Int])
      
      -- The indices to shuffle
      indices = [0 .. n - 1]
      
      -- Zip them: [(Rand1, 0), (Rand2, 1)...]
      paired = zip rands indices
      
      -- Sort by the random integer (fst) and keep the index (snd)
      shuffled = map snd (sortOn fst paired)
  in (shuffled, g2)
