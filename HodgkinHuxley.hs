{-# LANGUAGE RecordWildCards #-}

module HodgkinHuxley where

-- Core data types
data HHNeuron = HHNeuron
  { dt :: Double
  , maxT :: Double
  , startTime :: Double
  , stopTime :: Double
  , injectionCurrent :: Double
  , initV :: Double
  -- HH-specific parameters
  , ena :: Double  -- Sodium reversal potential
  , gna :: Double  -- Sodium conductance
  , ek :: Double   -- Potassium reversal potential
  , gk :: Double   -- Potassium conductance
  , el :: Double   -- Leak reversal potential
  , gl :: Double   -- Leak conductance
  } deriving (Show)

-- Simulation state
data HHState = HHState
  { voltage :: Double
  , mGate :: Double
  , nGate :: Double
  , hGate :: Double
  , time :: Double
  } deriving (Show)

-- Simulation results
data SimResult = SimResult
  { times :: [Double]
  , voltages :: [Double]
  , currents :: [Double]
  , mGates :: [Double]
  , nGates :: [Double]
  , hGates :: [Double]
  } deriving (Show)

-- Default HH neuron parameters
defaultHHNeuron :: HHNeuron
defaultHHNeuron = HHNeuron
  { dt = 0.02
  , maxT = 450.0
  , startTime = 50.0
  , stopTime = 300.0
  , injectionCurrent = 7.0
  , initV = 0.0
  , ena = 115.0
  , gna = 120.0
  , ek = -12.0
  , gk = 36.0
  , el = 10.6
  , gl = 0.30
  }

-- Gate kinetics functions
alphaN :: Double -> Double
alphaN volt = (0.1 - 0.01 * volt) / (exp (1.0 - 0.1 * volt) - 1.0)

alphaM :: Double -> Double
alphaM volt = (2.5 - 0.1 * volt) / (exp (2.5 - 0.1 * volt) - 1.0)

alphaH :: Double -> Double
alphaH volt = 0.07 * exp ((-volt) / 20.0)

betaN :: Double -> Double
betaN volt = 0.125 * exp ((-volt) / 80.0)

betaM :: Double -> Double
betaM volt = 4.0 * exp ((-volt) / 18.0)

betaH :: Double -> Double
betaH volt = 1.0 / (exp (3.0 - 0.1 * volt) + 1.0)

-- Gate dynamics
mDot :: Double -> Double -> Double
mDot volt m = alphaM volt * (1 - m) - betaM volt * m

nDot :: Double -> Double -> Double
nDot volt n = alphaN volt * (1 - n) - betaN volt * n

hDot :: Double -> Double -> Double
hDot volt h = alphaH volt * (1 - h) - betaH volt * h

-- Steady-state gate values
mInfinity :: Double -> Double
mInfinity volt = alphaN volt / (alphaN volt + betaN volt)

nInfinity :: Double -> Double
nInfinity volt = alphaN volt / (alphaN volt + betaN volt)

hInfinity :: Double -> Double
hInfinity volt = alphaH volt / (alphaH volt + betaH volt)

-- Utility functions
update :: Double -> Double -> Double -> Double
update oldValue rateOfChange timeStep = oldValue + rateOfChange * timeStep

between :: Double -> Double -> Double -> Double -> Double
between currentTime lower upper valueIfTrue
  | currentTime >= lower && currentTime <= upper = valueIfTrue
  | otherwise = 0.0

-- Voltage dynamics
dvdt :: Double -> Double -> Double -> Double -> Double -> HHNeuron -> Double
dvdt voltageNow currIn hhM hhN hhH HHNeuron{..} =
  currIn - (gna * (hhM ** 3.0) * hhH * (voltageNow - ena) +
            gk * (hhN ** 4.0) * (voltageNow - ek) +
            gl * (voltageNow - el))

-- Main simulation function
runHHSim :: HHNeuron -> SimResult
runHHSim neuron@HHNeuron{..} = 
  let initialState = HHState
        { voltage = initV
        , mGate = mInfinity initV
        , nGate = nInfinity initV
        , hGate = hInfinity initV
        , time = 0.0
        }
      
      simulate :: HHState -> [HHState]
      simulate state@HHState{..}
        | time > maxT = []
        | otherwise = 
            let injCur = between time startTime stopTime injectionCurrent
                newM = update mGate (mDot voltage mGate) dt
                newN = update nGate (nDot voltage nGate) dt
                newH = update hGate (hDot voltage hGate) dt
                newV = update voltage (dvdt voltage injCur mGate nGate hGate neuron) dt
                newState = HHState
                  { voltage = newV
                  , mGate = newM
                  , nGate = newN
                  , hGate = newH
                  , time = time + dt
                  }
            in state : simulate newState
      
      results = simulate initialState
      
  in SimResult
     { times = map time results
     , voltages = map voltage results
     , currents = map (\HHState{..} -> between time startTime stopTime injectionCurrent) results
     , mGates = map mGate results
     , nGates = map nGate results
     , hGates = map hGate results
     }

-- Example usage
main :: IO ()
main = do
  let results = runHHSim defaultHHNeuron
  putStrLn "Hodgkin-Huxley simulation completed"
  putStrLn $ "Simulated " ++ show (length (times results)) ++ " time points"
  putStrLn $ "Final voltage: " ++ show (last (voltages results))
