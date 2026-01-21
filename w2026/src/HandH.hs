{-# LANGUAGE GADTs #-}

module HandH where
import Data.Maybe (fromMaybe)

data SimParameters where
  SimParameters :: {
    hhdt :: Float,
    maxT :: Float,
    initT :: Float,
    startTime :: Float,
    stopTime :: Float,
    capacitance :: Float,
    resistance :: Float,
    initialVoltage :: Float,
    injectionCurrent :: Float,
    hhtau :: Float
    } -> SimParameters
  deriving Show

data NeuronState where
  NeuronState :: {
    vns :: Float
    , ic  :: Float
    , neuTime :: Float
    , mns :: Maybe Float
    , nns :: Maybe Float
    , hns :: Maybe Float
  } -> NeuronState
  deriving Show

data NeuronParams where
  NeuronParams :: {
    ena :: Float,
    gna :: Float,
    ek  :: Float,
    gk  :: Float,
    el  :: Float,
    gl  :: Float
    } -> NeuronParams
  deriving Show

data NeuronDynamics where
  NeuronDynamics :: {
    alphaN :: Float -> Float,
    alphaM :: Float -> Float,
    alphaH :: Float -> Float,
    betaN  :: Float -> Float,
    betaM  :: Float -> Float,
    betaH  :: Float -> Float,
    mdot   :: Float -> Float -> Float,
    ndot   :: Float -> Float -> Float,
    hdot   :: Float -> Float -> Float,
    minf   :: Float -> Float,
    ninf   :: Float -> Float,
    hinf   :: Float -> Float
    } -> NeuronDynamics

data Neuron where
  Neuron :: {parameters :: NeuronParams,
             equations  :: NeuronDynamics
            } -> Neuron

dotDynamics :: Float -> Float ->
               (Float -> Float) ->
               (Float -> Float) ->
               Float            
dotDynamics volt  chan alphaf betaf =
  alphaf volt * (1 - chan) - betaf volt * chan

dotInfinity :: Float ->
               (Float -> Float) ->
               (Float -> Float) ->
               Float
dotInfinity v alphaf betaf = alphaf v / (alphaf v + betaf v)
  
pSet1 :: SimParameters
pSet1 = SimParameters 0.05 300.0 0.0 100.0
          150.0 1.0 2.0 0.0 20.0
          (resistance pSet1 * capacitance pSet1)

pSet2 :: SimParameters
pSet2 = SimParameters 0.05 300.0 0.0 10.0
           50.0 1.0 2.0 0.0 0.0
           (resistance pSet2 * capacitance pSet2)

healthyParams :: NeuronParams
healthyParams = NeuronParams 115 120 (-12) 36 10.6 0.3

healthyDynamics :: NeuronDynamics
healthyDynamics = NeuronDynamics {
  alphaN = \v -> (0.1-0.01*v)/(exp(1-0.1*v) - 1),
  alphaM = \v -> (2.5-0.1*v)/(exp(2.5-0.1*v) - 1),
  alphaH = \v ->  0.07*exp((-v)/20),
  betaN  = \v ->  0.125 * exp((-v)/80),
  betaM  = \v ->  4*exp((-v)/18),
  betaH  = \v ->  1/(exp(3-0.1*v)+1),
  mdot   = \v c -> dotDynamics v c (alphaM healthyDynamics)
                       (betaM healthyDynamics),
  ndot   = \v c -> dotDynamics v c (alphaN healthyDynamics)
                       (betaN healthyDynamics),
  hdot   = \v c -> dotDynamics v c (alphaH healthyDynamics)
                       (betaH healthyDynamics),
  minf   = \v -> dotInfinity v (alphaM healthyDynamics)
                 (betaM healthyDynamics),
  ninf   = \v -> dotInfinity v (alphaN healthyDynamics)
                 (betaN healthyDynamics),
  hinf  =  \v -> dotInfinity v (alphaH healthyDynamics)
                 (betaH healthyDynamics)
  }

healthyNeuron :: Neuron
healthyNeuron = Neuron healthyParams healthyDynamics

hhInitialState :: NeuronState
hhInitialState = NeuronState (initialVoltage pSet1)
  (injectionCurrent pSet1)
  (initT pSet1) Nothing Nothing Nothing

updNeuron :: Neuron  ->  SimParameters  ->  NeuronState ->  NeuronState
updNeuron neuin spin nsin = 
  let eqneuin = equations neuin
      voltagein = vns nsin
      nparamsin = parameters neuin
      outi = ic nsin
      mnew = fromMaybe 0.0
        (case mns nsin of
            Nothing ->  Just $ minf eqneuin  voltagein
            Just x  ->  Just $ x +
              mdot eqneuin  voltagein x * hhdt spin)
      nnew = fromMaybe 0.0
        (case nns nsin of
            Nothing ->  Just $ ninf eqneuin  voltagein
            Just x  ->  Just $ x +
              ndot eqneuin voltagein x * hhdt spin )
      hnew = fromMaybe 0.0
        (case hns nsin of 
            Nothing ->  Just $ hinf eqneuin voltagein
            Just x  ->  Just $ x +
              hdot eqneuin voltagein x * hhdt spin)
      dvdt = outi -
        (gna nparamsin * mnew ^ 3 * hnew *
          (voltagein - ena nparamsin) +
          gk nparamsin * nnew ^ 4 *
          (voltagein - ek nparamsin) +
          gl nparamsin *
          (voltagein - el nparamsin))
  in
    nsin {vns = voltagein + dvdt * hhdt spin
         , ic = if (neuTime nsin  < stopTime spin) &&
                   (neuTime nsin  > startTime spin)
                then injectionCurrent spin
                else 0.0
         , neuTime = neuTime nsin + hhdt spin 
         , mns = Just mnew
         , nns = Just nnew
         , hns = Just hnew
         }
