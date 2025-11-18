import Graphics.Gnuplot.Simple
main = plotFunc [PNG "./test.png", Terminal "png"] (linearScale 100 (1, 4)) ((\x -> exp (1.0/x)) :: Double -> Double)

  
