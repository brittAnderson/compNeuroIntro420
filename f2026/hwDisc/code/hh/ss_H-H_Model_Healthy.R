"Healthy Hodgkin-Huxley Model"
eNa <- 115
gNa <- 120
eK <- -12
gK <- 36
eL <- 10.6
gL <- 0.3

dt <- 0.01
duration <- 120

time <- seq(0, duration, by = dt)
nSteps <- length(time)

V <- numeric(nSteps)
n <- numeric(nSteps)
m <- numeric(nSteps)
h <- numeric(nSteps)

V[1] <- 0

alpha_n <- function(v){
  if(abs(v - 10) < 1e-10) return(0.1)
  0.01 * (10 - v) / (exp((10 - v)/10) - 1)
}

beta_n <- function(v){
  0.125 * exp(-v/80)
}

alpha_m <- function(v){
  if(abs(v - 25) < 1e-10) return(1)
  0.1 * (25 - v) / (exp((25 - v)/10) - 1)
}

beta_m <- function(v){
  4 * exp(-v/18)
}

alpha_h <- function(v){
  0.07 * exp(-v/20)
}

beta_h <- function(v){
  1 / (exp((30 - v)/10) + 1)
}

n[1] <- alpha_n(0)/(alpha_n(0)+beta_n(0))
m[1] <- alpha_m(0)/(alpha_m(0)+beta_m(0))
h[1] <- alpha_h(0)/(alpha_h(0)+beta_h(0))

Iext <- rep(0, nSteps)
Iext[time >= 5 & time <= 110] <- 10

for(i in 1:(nSteps-1)){
  
  INa <- gNa * (m[i]^3) * h[i] * (V[i] - eNa)
  IK <- gK * (n[i]^4) * (V[i] - eK)
  IL <- gL * (V[i] - eL)
  
  dV <- Iext[i] - INa - IK - IL
  
  dn <- alpha_n(V[i]) * (1 - n[i]) - beta_n(V[i]) * n[i]
  dm <- alpha_m(V[i]) * (1 - m[i]) - beta_m(V[i]) * m[i]
  dh <- alpha_h(V[i]) * (1 - h[i]) - beta_h(V[i]) * h[i]
  
  V[i+1] <- V[i] + dt * dV
  n[i+1] <- n[i] + dt * dn
  m[i+1] <- m[i] + dt * dm
  h[i+1] <- h[i] + dt * dh
}

plot(time, V,
     type = "l",
     col = "blue",
     lwd = 2,
     xlab = "Time (ms)",
     ylab = "Voltage (mV)")
grid()

