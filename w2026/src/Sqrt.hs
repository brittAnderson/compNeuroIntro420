module Sqrt where

xcubed :: Double -> Double
xcubed  = (** 3)

diffXCubed :: Double -> Double
diffXCubed = ( 3 *) . ( ** 2)

getStep :: Double -> Double -> Double
getStep guess goal = ( goal - xcubed guess)  / diffXCubed guess

getCubeRoot :: Double -> Double -> Double -> Double 
getCubeRoot g i t =
  let  cg = i + getStep i g
       myerr = abs (xcubed cg - g) in
    if myerr > t then getCubeRoot g cg t else cg


