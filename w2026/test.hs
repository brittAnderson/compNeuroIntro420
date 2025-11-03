myexp :: Double -> Int -> Double
myexp x p =
  foldr (*) 1 (replicate p x)

  
