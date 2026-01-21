{-# LANGUAGE InstanceSigs #-}
module Spring where

data SpringState = SpringState {
  sDt :: !Double,
  sAccFxn :: !(Double -> Double),
  sVel :: !Double,
  sLoc :: !Double
}

instance Show SpringState where
  show :: SpringState -> String
  show s = "SpringState { t: " ++ show (sDt s) ++ "\
           \, v: " ++ show (sVel s) ++ "\
           \, loc: " ++ show (sLoc s) ++ " }"


{- Initial Values -}

initV :: Double
initV = 0 
initLoc :: Double
initLoc = 10 
sprConst :: Double
sprConst = 2.0 
timeStep :: Double 
timeStep = 0.05 
sprdt :: Double
sprdt = timeStep


{- Helper Functions -}

accel :: Double -> Double -> Double 
accel sc loc = (-sc) * loc

accWithSC :: Double -> Double
accWithSC = accel sprConst -- called currying

eulerUD :: Double -> Double -> Double -> Double
eulerUD ts roc oldval = oldval + roc * ts

springLoop :: SpringState -> SpringState
springLoop ss =
  let cA = sAccFxn ss (sLoc ss)
      cV = eulerUD (sDt ss) cA (sVel ss)
      cL =  eulerUD (sDt ss) cV (sLoc ss)
  in ss {sVel = cV, sLoc = cL}
  

releaseSpring :: Int -> [SpringState]
releaseSpring maxIter =
  take maxIter (iterate springLoop
                (SpringState timeStep
                 accWithSC initV
                 initLoc)) -- makes use of lazy evaluation
