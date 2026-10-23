#Defect selected: F184C
#seems that potassium gate is opening slower but closing is unaffected.
#conductance of K channels is reduced to 15% of a typical K channel
#dV = 27, so K channels needs a more depolarized membrane of +27mV to activate the same amount as a typical K channel?
eNa <- 115
gNa <- 120
eK <- -12
gK <- 36
eL <- 10.6
gL <- 0.3

#yA = K gate activation rate; yB = K gate closing rate; yK = K conductance/current that K channels carry; dV = voltage sensitivity changes
#with this defect, K outward current is reduced and alters the typical timings 
#leads to impairments during repolarization and prolonged depolarization?
yA <- 0.5
yB <- 1
yK <- 0.15
dV <- 27
yT <- 1

alNVal <- c()
alMVal <- c()
alHVal <- c()
beNVal <- c()
beMVal <- c()
beHVal <- c()

nVal <- c()
mVal <- c()
hVal <- c()

naCurVal <- c()
kCurVal <- c()
lCurVal <- c()

volt <- c(0.0)
current <- c()
baseCurrent <- 0.0
curIn <- 0
curRandRange <- 0

time <- c(0.0)
timeStep <- 0.01
duration <- 0

startTime <- 0
endTime <- 0

#n = k activation; m = na activation ; h = na inactivation
alphN <- function(v) {
  newAlN <- 0.01*(10-v)/(exp((10-v)/10)-1)
  alNVal <<- c(alNVal, newAlN)
  return(newAlN)
}

alphM <- function(v) {
  newAlM <- 0.1*(25-v)/(exp((25-v)/10)-1)
  alMVal <<- c(alMVal, newAlM)
  return(newAlM)
}

alphH <- function(v) {
  newAlH <- 0.07 * exp(-v / 20)
  alHVal <<- c(alHVal, newAlH)
  return(newAlH)
}

betaN <- function(v) {
  newBeN <- 0.125 * exp(-v / 80)
  beNVal <<- c(beNVal, newBeN)
  return(newBeN)
}

betaM <- function(v) {
  newBeM <- 4*exp(-v/18)
  beMVal <<- c(beMVal, newBeM)
  return(newBeM)
}

betaH <- function(v) {
  newBeH <- 1 / (exp((30 - v) / 10) + 1)
  beHVal <<- c(beHVal, newBeH)
  return(newBeH)
}

update <- function(old, deriv) {
  return(old + deriv * timeStep)
}

dNMH <- function(v, nmh) {
  if (nmh == tail(nVal, 1)) {
    return(yT * (yA * alphN(v - dV) * (1 - nmh) - yB * betaN(v - dV) * nmh))   
  } else if (nmh == tail(mVal, 1)) {
    return(alphM(v) * (1 - nmh) - betaM(v) * nmh)
  } else if (nmh == tail(hVal, 1)) {
    return(alphH(v) * (1 - nmh) - betaH(v) * nmh)
  }
}

#explain why we need a special case for v == 0
#the if statement seems to set the values of n,m, and h for the initial step (where voltage is 0)
makeNMH <- function(v) {
  if (v == 0) {
    nVal <<- c(nVal, yA*alphN(v - dV) / (yA*alphN(v - dV) + yB*betaN(v - dV))); alNVal <<- alNVal[-1]   
    mVal <<- c(mVal, alphM(v) / (alphM(v) + betaM(v))); alMVal <<- alMVal[-1]
    hVal <<- c(hVal, alphH(v) / (alphH(v) + betaH(v))); alHVal <<- alHVal[-1]
  } else {
    nVal <<- c(nVal, update(tail(nVal, 1), dNMH(v, tail(nVal, 1))))
    mVal <<- c(mVal, update(tail(mVal, 1), dNMH(v, tail(mVal, 1))))
    hVal <<- c(hVal, update(tail(hVal, 1), dNMH(v, tail(hVal, 1))))
  }
}

naCur <- function(v, m, h) {
  newNaCur <- gNa * (m^3) * h * (v - eNa)
  naCurVal <- c(naCurVal, newNaCur)
  return(newNaCur)
}

kCur <- function(v, n) {
  newKCur <- yK * gK * (n^4) * (v - eK)
  kCurVal <- c(kCurVal, newKCur)
  return(newKCur)
}

lCur <- function(v) {
  newLCur <- gL * (v - eL)
  lCurVal <- c(lCurVal, newLCur)
  return(newLCur)
}

naKLBuild <- function(v, n, m, h) {
  return(naCur(v,m,h) + kCur(v,n) + lCur(v))
}

dVolt <- function(inCur, v, n, m, h) {
  return(inCur - (naKLBuild(v, n, m, h)))
}

currentBuild <- function(bC, cIn, cRand, sT, eT) {
  if (tail(time, 1) < sT || tail(time, 1) > eT) {
    current <<- c(current, bC)
  } else {
    current <<- c(current, cIn + runif(1, -cRand, cRand))
  }
}

hHmodel <- function(duration, baseCurrent, curIn, curRandRange, startTime, endTime) {
  currentBuild(baseCurrent, curIn, curRandRange, startTime, endTime)
  makeNMH(tail(time, 1))
  naKLBuild(0.0, tail(nVal, 1), tail(mVal, 1), tail(hVal, 1))
  while (tail(time, 1) < duration) {
    currentBuild(baseCurrent, curIn, curRandRange, startTime, endTime)
    makeNMH(tail(volt, 1))
    volt <<- c(volt, tail(volt, 1) +
                 (dVolt(tail(current, 1), tail(volt, 1),
                        tail(nVal, 1), tail(mVal, 1), tail(hVal, 1)) * timeStep))
    time <<- c(time, tail(time, 1) + timeStep)
  }
}
#(duration, baseCurrent, curIn, curRandRange, startTime, endTime)
hHmodel(120, 0, 3, 10, 5, 110)

df <- data.frame(time = time, volt = volt)

library(ggplot2)

ggplot(df, aes(x = time, y = volt)) +
  geom_line()+
  labs(x = "time", y = "volt")
