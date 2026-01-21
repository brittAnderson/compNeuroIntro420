{-# LANGUAGE InstanceSigs #-}
module Perceptron where

import MyMats as M

-- | A new custom type that can only be Positive (1) or Negative (-1).
data Bipolar = Pos | Neg
  deriving (Show, Eq)

-- | Making Bipolar an instance of Num allows us to use it with operators like (*).
-- We define multiplication to follow standard sign rules (e.g. Neg * Neg = Pos).
-- Note: We leave addition undefined (or error) if it doesn't make sense for this type.
instance Num Bipolar where
  Pos * Pos = Pos
  Neg * Neg = Pos
  _   * _   = Neg

  abs _      = Pos
  signum x   = x
  negate Pos = Neg
  negate Neg = Pos

  fromInteger :: Integer -> Bipolar
  fromInteger x
    | x >= 0    = Pos
    | otherwise = Neg

  (+) _ _ = error "Addition is undefined for Bipolar logic"

-- | Helper to convert Bipolar back to a standard Number (1 or -1)
-- so it can interact with the Weight and Input matrices.
toNum :: Num a => Bipolar -> a
toNum Pos =  1
toNum Neg = -1

type Weight a    = SimpMat a
type Input  a    = SimpMat a
type Threshold a = a

activity :: Num a => Weight a -> Input a -> a
activity w i  =
  let tempA = M.vectMult w i
  in head (head tempA)

appendBias :: Num a => Input a -> Input a
appendBias v = [ x ++ [1] | x <- v]

-- | Check if intensity is greater than threshold.
-- Returns our strict Bipolar type.
output :: (Ord a, Num a) => Threshold a -> a -> Bipolar
output  threshold intensity
  | intensity >= threshold = Pos
  | otherwise              = Neg

-- | Checks correctness. Returns Bipolar.
isCorrect :: (Eq a) => a -> a -> Bipolar
isCorrect des obs = if des == obs then Pos else Neg

-- | Updates weights.
-- We take in the Bipolar values, multiply them (using our Num instance),
-- but convert the result back to 'Num a' to scale the input vector.
updateWeight :: (Ord a, Num a) => Threshold a -> Input a -> a -> Weight a -> Weight a
updateWeight th inp des ow =
  let intensity = activity ow inp
      obs          = output th intensity
      amIRight       = isCorrect des $ toNum obs
      scalar = toNum (amIRight * obs)
  in add2SimpMats ow [ map (* scalar) (head inp)]


-- used for class pencil and paper exercise  
myin :: SimpMat Double
myin = [[0.3,0.7]] :: SimpMat Double
myw :: Weight Double
myw = [[-0.6, 0.8]] :: Weight Double
mydes :: Double
mydes = 1 :: Double

-- used for class exercise
testData :: [(SimpMat Double, Double)]
testData = [([[0.3  ,  0.7 ]] ,  1.0 )
  , ([[-0.5 ,  0.3 ]] ,  -1.0 )
  , ([[0.7  ,  0.3 ]] ,   1.0 )
  , ([[-0.2 ,  -0.8]] ,  -1.0)]

updWeight0 :: (Ord a, Num a) => (Input a, a) ->  Weight a ->  Weight a  
updWeight0 (inp, des) = updateWeight 0 inp des

testDataFs :: [Weight Double -> Weight Double]
testDataFs = map updWeight0 testData


oneRound :: Weight a -> [Weight a ->  Weight a] ->  [Weight a]
oneRound = scanl (flip ($)) 

checkWt :: Weight Double -> [(SimpMat Double, Double)] -> [(Double, Double)]
checkWt w = map (\(inp,des) -> ((toNum . (output 0) . (activity w)) inp,des)) 
