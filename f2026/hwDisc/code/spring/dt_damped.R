# Damped Spring 

P <- 5
C <- 0.15

dt <- 0.01
tmax <- 30

t <- seq(0, tmax, by = dt)
n <- length(t)

position <- numeric(n)
velocity <- numeric(n)

position[1] <- 1
velocity[1] <- 0

for(i in 1:(n-1)){
  
  acceleration <- -P * position[i] - C * velocity[i]
  
  velocity[i+1] <- velocity[i] + acceleration * dt
  position[i+1] <- position[i] + velocity[i] * dt
}

plot(
  t,
  position,
  type = "l",
  lwd = 3,
  col = "pink",
  main = "Damped Spring",
  xlab = "Time",
  ylab = "Position",
  xaxs = "i"
)

grid()
abline(h = 0, lty = 2)



