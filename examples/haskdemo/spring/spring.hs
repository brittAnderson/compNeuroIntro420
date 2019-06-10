-- Uses the haskel-chart package for plotting our results
-- For file output we need to build Chart-diagrams with stack
-- We run with `stack runghc nmmunce_spring.hs`

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)

-- Dynamics functions
aStep s v p k dt = -p * s - k * v * dt

vStep v a dt = v + a * dt

sStep s v dt = s + v * dt

-- Creates list of s values over time
simulateSpring :: Double -> Double -> Double -> Double -> Double -> Double -> [Double]
simulateSpring s v p k n dt =
  if n == 0
  then [s]
  else s:simulateSpring ns nv p k (pred n) dt
  where
    nv = v + (aStep s v p k dt) * dt
    ns = s + nv * dt

-- Hardcoded simulation constants
initS = 0
initV = 1
p = 5
k = 5
duration = 20
dt = 0.1
steps = fromIntegral $ ceiling (duration / dt)

main :: IO()
main = toFile def "spring_output.png" $ do
  setColors [opaque blue, opaque red]
  plot (line "s" [(zip ([0,dt..duration] :: [Double]) res)])
  where res = (simulateSpring initS initV p k steps dt)
