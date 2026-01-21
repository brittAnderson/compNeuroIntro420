{-# LANGUAGE GADTs #-}

module IandF where

dt,maxt,initt,starttime,stoptime,cap
  ,res,threshold,spikedisplay,initv
  ,voltage,injectioncurrent
  ,iandftau :: Double
injectiontime,runningTime :: [Double]
dt = 0.05
maxt = 10.0
initt = 0.0
starttime = 1.0
stoptime = 6.0
cap = 1.0
res  = 2.0
threshold = 3.0
spikedisplay = 8.0
initv = 0.0
voltage = initv
injectioncurrent = 4.3
injectiontime = [starttime, stoptime]
iandftau = res * cap
runningTime = [0.0]

data IandFStrut where
  IandFStrut :: {
    time :: [Double],
    spikestatus :: Bool,
    currents :: [Double],
    voltages :: [Double]
    } -> IandFStrut
    deriving Show

instance Semigroup IandFStrut where
  (IandFStrut t1 _ c1 v1) <> (IandFStrut t2 s2 c2 v2) = 
      IandFStrut (t1 ++ t2) s2 (c1 ++ c2) (v1 ++ v2)

initialStrut :: IandFStrut
initialStrut = makeStep 0 False 0.0 initv

update :: Double -> Double -> Double -> Double 
update ov roc ts = roc * ts + ov

dvdt :: Double -> Double -> Double -> Double
dvdt lres li lv =
 (1 / iandftau) * ( lres * li - lv )

between :: Double -> Double
between ct = case injectiontime of
  [myfirst,mylast] ->
    if ct >= myfirst && ct <= mylast
    then injectioncurrent
    else 0.0
  badList -> error "Error: Expected list of two doubles"

vchoice :: (Double, Bool, Double, Double) -> Double
vchoice (cv,ss,thr,sd)
  | ss = 0.0
  | cv > thr  && not ss = sd
  | otherwise = cv

makeStep :: Double -> Bool -> Double -> Double -> IandFStrut
makeStep t s c v = IandFStrut [t] s [c] [v]
  
oneRunIandF :: IandFStrut -> IandFStrut
oneRunIandF inStrut =
  let ct = last (time inStrut)
      cv = last (voltages inStrut)
      ci = between ct
      nv  = vchoice (update cv (dvdt res ci cv) dt
                    , spikestatus inStrut
                    , threshold, spikedisplay)
      nextstep = makeStep (ct + dt) 
        (abs (nv - spikedisplay) < threshold)
        ci
        nv
  in (inStrut <> nextstep)

