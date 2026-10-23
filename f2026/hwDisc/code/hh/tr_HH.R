# PSYCH 420 HH Starter Program (R version)
#I used the starter code provided and got Copilot to give me the same code with
#the same fill in values in R. I am not quite comfortable working in Python yet
#prompt: can you convert this code to a language i better understand (R) and 
#keep the fill in the blank sections empty?

#Constants
eNa <- 115
gNa <- 120
eK  <- -12
gK  <- 36
eL  <- 10.6
gL <- 0.3
timeStep <- 0.01

#Rate functions
alphN <- function(v) 0.01 * (10 - v) / (exp((10 - v) / 10) - 1)
alphM <- function(v) 0.1  * (25 - v) / (exp((25 - v) / 10) - 1)
alphH <- function(v) 0.07 * exp(-v / 20)
betaN <- function(v) 0.125 * exp(-v / 80)
betaM <- function(v) 4 * exp(-v / 18)
betaH <- function(v) 1 / (exp((30 - v) / 10) + 1)

#Currents
naCur <- function(v, m, h) gNa * m^3 * h * (v - eNa)
kCur  <- function(v, n)    gK  * n^4 * (v - eK)
lCur  <- function(v)       gL  * (v - eL)

naKLBuild <- function(v, n, m, h) {
  naCur(v, m, h) + kCur(v, n) + lCur(v)
}

dVolt <- function(inCur, v, n, m, h) {
  inCur - naKLBuild(v, n, m, h)
}

update <- function(old, deriv) old + deriv * timeStep
dGate  <- function(alpha, beta, x) alpha * (1 - x) - beta * x

currentBuild <- function(t, baseCurrent, curIn, curRandRange, startTime, endTime) {
  if (t < startTime || t > endTime) {
    baseCurrent
  } else {
    curIn + runif(1, -curRandRange, curRandRange)
  }
}

#Model
hhModel <- function(duration, baseCurrent, curIn, curRandRange, startTime, endTime) {
  steps <- round(duration / timeStep)
  time <- (0:steps) * timeStep
  volt <- numeric(steps + 1)
  current <- numeric(steps + 1)
  n <- numeric(steps + 1)
  m <- numeric(steps + 1)
  h <- numeric(steps + 1)
  
  n[1] <- alphN(0) / (alphN(0) + betaN(0))
  m[1] <- alphM(0) / (alphM(0) + betaM(0))
  h[1] <- alphH(0) / (alphH(0) + betaH(0))
  
  for (i in 1:steps) {
    v <- volt[i]
    current[i] <- currentBuild(time[i], baseCurrent, curIn, curRandRange, startTime, endTime)
    
    if (v == 0) {
      n[i + 1] <- alphN(v) / (alphN(v) + betaN(v))
      m[i + 1] <- alphM(v) / (alphM(v) + betaM(v))
      h[i + 1] <- alphH(v) / (alphH(v) + betaH(v))
    } else {
      n[i + 1] <- update(n[i], dGate(alphN(v), betaN(v), n[i]))
      m[i + 1] <- update(m[i], dGate(alphM(v), betaM(v), m[i]))
      h[i + 1] <- update(h[i], dGate(alphH(v), betaH(v), h[i]))
    }
    
    volt[i + 1] <- volt[i] + dVolt(current[i], v, n[i + 1], m[i + 1], h[i + 1]) * timeStep
  }
  current[steps + 1] <- current[steps]
  list(time = time, volt = volt, current = current)
}

#Plot
res <- hhModel(250, 0, 40, 0, 50, 150)

plot(res$time, res$volt, type = "l", col = "black",
     xlab = "Time", ylab = "Voltage",
     ylim = c(-30, 130), xlim = c(0, 200),
     cex.axis = 1.3, cex.lab = 1.3)
lines(res$time, res$current, col = "red", lwd = 2)


