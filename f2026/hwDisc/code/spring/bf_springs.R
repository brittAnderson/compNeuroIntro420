# Frictionless and damped springs using forward Euler's method.
# Position is displacement from equilibrium. No extra packages needed.

P <- 1          # Spring constant (mass is assumed to be 1)
dt <- 0.001     # Time step; small to limit Euler approximation error
time <- seq(0, 20, by = dt)

simulate_spring <- function(k) {
  position <- numeric(length(time))
  velocity <- numeric(length(time))
  position[1] <- 1  # Start stretched one unit from equilibrium
  velocity[1] <- 0  # Release from rest

  for (i in 1:(length(time) - 1)) {
    acceleration <- -P * position[i] - k * velocity[i]
    # Both updates use values from the current time step.
    position[i + 1] <- position[i] + velocity[i] * dt
    velocity[i + 1] <- velocity[i] + acceleration * dt
  }

  data.frame(time = time, position = position, velocity = velocity)
}

frictionless <- simulate_spring(k = 0)
damped <- simulate_spring(k = 0.4)

# Show both plots with the same axis limits.
par(mfrow = c(1, 2))
plot(frictionless$time, frictionless$position, type = "l",
     col = "blue", ylim = c(-1.1, 1.1),
     main = "Frictionless spring", xlab = "Time", ylab = "Position")
abline(h = 0, lty = 2, col = "gray")
plot(damped$time, damped$position, type = "l",
     col = "red", ylim = c(-1.1, 1.1),
     main = "Damped oscillator", xlab = "Time", ylab = "Position")
abline(h = 0, lty = 2, col = "gray")
par(mfrow = c(1, 1))

# Forward Euler causes slight artificial amplitude growth in the
# frictionless case. Reducing dt reduces this error over a fixed duration.
