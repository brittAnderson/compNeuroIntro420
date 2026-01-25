{-# LANGUAGE ScopedTypeVariables #-}
module HopfieldImproved where

{-
I wrote the Hopfield.hs. But then to learn I fed that file to Claude and
had it offer some perspective on ways to improve. This is what it returned.
The idea is that you spend time (maybe a lot of time writing yourself) and
then use the LLMs as a tutor.
-}

import System.Random (StdGen, mkStdGen, randoms)
import System.Random.Internal (splitGen)
import Data.List (sortOn)
import MyMats

-- | Example weight matrix for a Hopfield network
-- Note: diagonal is zero (no self-connections), matrix is symmetric
testWtMat :: SimpMat Integer
testWtMat = [[0,-2,2,-2],[-2,0,-2,2],[2,-2,0,-2],[-2,2,-2,0]]

-- | Example input pattern (represented as a 1-row matrix)
-- In Hopfield networks, we use bipolar encoding: 1 and -1
testInput :: SimpMat Integer
testInput = [[1,-1,1,-1]]

-- | Generate a random permutation of indices [0..n-1]
-- This gives us an asynchronous update schedule for the Hopfield network
-- 
-- How it works: We pair each index with a random number, sort by the random
-- number, then extract the indices. This effectively shuffles the indices.
genSchedule :: Int -> StdGen -> ([Int], StdGen)
genSchedule n gen =
  let (g1, g2) = splitGen gen
      rands = take n (randoms g1 :: [Int])
      indices = [0 .. n - 1]
      paired = zip rands indices  -- [(randomNum, index), ...]
      shuffled = map snd (sortOn fst paired)  -- sort by random, keep indices
  in (shuffled, g2)

-- | Update a single neuron in the network using the Hopfield update rule
-- 
-- The update rule is:
--   1. Compute activation: sum of (weight * input) for all connections
--   2. Apply threshold: if activation > 0 then 1 else -1
updateNeuronAt :: (Ord a, Num a) => SimpMat a -> SimpMat a -> Int -> SimpMat a
updateNeuronAt weightMatrix input neuronIdx =
  [take neuronIdx pattern ++ [newValue] ++ drop (neuronIdx + 1) pattern] 
  where 
    pattern = head input  -- Extract the pattern from 1-row matrix
    weightRow = weightMatrix !! neuronIdx  -- Weights for this neuron
    
    -- Compute activation (dot product of weights and input)
    activation = sum $ zipWith (*) weightRow pattern
    
    -- Apply threshold function (sign function, treating 0 as negative)
    newValue = if activation > 0 then 1 else -1

-- | Update all neurons once, in the order specified by the index list
-- This performs one "round" of asynchronous updates
updateOneRound :: (Ord a, Num a) => SimpMat a -> [Int] -> SimpMat a -> SimpMat a
updateOneRound weightMatrix updateOrder input =
  foldl (updateNeuronAt weightMatrix) input updateOrder

-- | Update the network once with a random update order
-- This is a single round of asynchronous Hopfield updates
updateWithRandomSchedule :: (Num a, Ord a) => Int -> SimpMat a -> SimpMat a -> SimpMat a
updateWithRandomSchedule randomSeed weightMatrix input =
  let numNeurons = length (head input)
      updateOrder = fst (genSchedule numNeurons (mkStdGen randomSeed))
  in updateOneRound weightMatrix updateOrder input

-- | Check if the network has reached a stable state (converged)
hasConverged :: Eq a => SimpMat a -> SimpMat a -> Bool
hasConverged oldState newState = oldState == newState

-- | Run the Hopfield network until convergence or max iterations
-- Returns the final state and the number of iterations taken
runUntilConvergence :: (Num a, Ord a, Eq a) => 
                       Int ->           -- Initial random seed
                       Int ->           -- Maximum iterations
                       SimpMat a ->     -- Weight matrix
                       SimpMat a ->     -- Initial input pattern
                       (SimpMat a, Int) -- (Final state, iterations)
runUntilConvergence seed maxIter weightMatrix initialInput =
  go initialInput (mkStdGen seed) 0
  where
    go currentState gen iterCount
      | iterCount >= maxIter = (currentState, iterCount)
      | otherwise =
          let numNeurons = length (head currentState)
              (updateOrder, newGen) = genSchedule numNeurons gen
              newState = updateOneRound weightMatrix updateOrder currentState
          in if hasConverged currentState newState
             then (newState, iterCount + 1)
             else go newState newGen (iterCount + 1)

-- | Example usage:
-- >>> runUntilConvergence 42 100 testWtMat testInput
-- ([[1,-1,1,-1]], 1)  -- Converged in 1 iteration (already at attractor)
