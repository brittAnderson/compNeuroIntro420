module Hopfield where

import System.Random (StdGen, mkStdGen, randoms)
import System.Random.Internal (splitGen)
import Data.List (sortOn)
import MyMats

testWtMat :: SimpMat Integer
testWtMat = [[0,-2,2,-2],[-2,0,-2,2],[2,-2,0,-2],[-2,2,-2,0]]

testInput :: SimpMat Integer
testInput = [[1,-1,1,-1]]

genSchedule :: Int -> StdGen -> ([Int], StdGen)
genSchedule n gen =
  let (g1, g2) = splitGen gen
      rands = take n (randoms g1 :: [Int])
      indices = [0 .. n - 1]
      paired = zip rands indices
      shuffled = map snd (sortOn fst paired)
  in (shuffled, g2)

updateInput' :: (Ord a, Num a) => SimpMat a -> SimpMat a -> Int -> SimpMat a
updateInput' wmat input idx =
  [take idx innerinput ++ [thresholdNewVal newval] ++ drop (idx + 1) innerinput] 
  where innerinput = head input
        newval = multRow2Input (accessSimpMatRow idx wmat) input
        multRow2Input rowInMat inPatt = multSimpMats rowInMat $ rotateLList inPatt
        accessSimpMatRow row inmat = [inmat!!row]
        thresholdNewVal [[v]] = if v > 0 then 1 else -1

updateInput1Rnd :: (Ord a, Num a) => SimpMat a -> [Int] -> SimpMat a -> SimpMat a
updateInput1Rnd wtmat idxs input =
  foldl (updateInput' wtmat) input idxs

updateInputRand :: (Num a, Ord a) => Int -> SimpMat a -> SimpMat a -> SimpMat a
updateInputRand rndSeed wtMat input =
  let ridx = fst (genSchedule (length (head input)) (mkStdGen rndSeed)) in
    updateInput1Rnd wtMat ridx input
-- this is where it would be necessary to recurse with the newStdGen until convergence --
-- would also need to right the function to handle a list or matrix of multiple inputs. 
  
