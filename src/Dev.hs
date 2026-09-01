module Dev
  (module Automata
  , module BusyBeaver
  , module HandH
  , module IandF
  , module MyMats
  , module Perceptron
  , module Spring 
  , module Sqrt
  , module Hopfield
  ) where

import Automata
import BusyBeaver
import HandH
import IandF
import MyMats
import Perceptron
import Spring
import Sqrt
import Hopfield
import qualified System.Random as R

mkStdGen :: Int -> R.StdGen
mkStdGen = R.mkStdGen

