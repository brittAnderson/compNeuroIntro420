#I first asked claude to turn my old code into one that uses Euler's method, so I used & edited this one for the integrate and fire model
#With deSolve, I would just have to do "method = "euler" under ode, so I don't think it demonstrates much
P <- 1      # spring constant term
k <- 0.2    # damping coefficient

dt <- 0.1
times <- seq(0, 60, by = dt)
n <- length(times)

position <- numeric(n)
velocity <- numeric(n)

position[1] <- 10
velocity[1] <- 0

# Euler's method
for (i in 1:(n - 1)) {
  dposition <- velocity[i]
  dvelocity <- -P * position[i] - k * velocity[i]
  position[i + 1] <- position[i] + dposition * dt
  velocity[i + 1] <- velocity[i] + dvelocity * dt
}

damped_df <- data.frame(times, position, velocity)

ggplot(damped_df, aes(x = times, y = position)) +
  geom_line() +
  labs(x = "Time", y = "Position", title = "Damped Oscillator")