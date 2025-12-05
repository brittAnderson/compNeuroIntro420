module BusyBeaver where

import Data.Set (Set,fromList)
import qualified Data.Map.Lazy as Map
import Data.Map.Lazy (Map)

type TMState = Char
type Position = Int 
data BinNum = Zero | One deriving (Show, Eq, Ord, Enum)
type TMTape = Map Int BinNum

data TuringMachine = TM
  { state :: TMState
  , tape  :: TMTape
  , headLoc :: Int} deriving Show

testStates :: Set TMState
testStates = fromList ['a','b','h']

initialTape :: Map Int BinNum
initialTape = Map.empty

defaultTapeCell :: BinNum
defaultTapeCell = Zero

initialState :: TMState
initialState = 'a'

initialTM :: TuringMachine
initialTM = TM initialState initialTape 0

readFromTape :: TMTape -> Int -> BinNum
readFromTape t i =
  Map.findWithDefault Zero i t

writeToTape :: BinNum -> Int ->  TMTape ->  TMTape
writeToTape bn hl =  Map.insert hl bn


updTM :: TuringMachine -> TuringMachine
updTM tm@(TM st t hl) =
  let bn = readFromTape t hl in
  case st of
    'a' -> case bn of
             Zero -> tm {state = 'b'
                        , tape = writeToTape One  hl t
                        , headLoc =  hl + 1}
             One  -> tm {state = 'b'
                        , tape = writeToTape One  hl t
                        , headLoc =  hl - 1}
    'b' -> case bn of
             Zero -> tm {state = 'a'
                        , tape = writeToTape One  hl t
                        , headLoc =  hl - 1}
             One  -> tm {state = 'h'
                        , tape = writeToTape One  hl t
                        , headLoc =  hl }
    'h' -> tm
    _ -> error "Entered incorrect state variable" 
    
 

