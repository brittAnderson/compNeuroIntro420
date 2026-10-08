# Integrate-and-Fire Neuron
# Uses Euler's Method

# Parameters
dt <- 0.05
steps <- 1000

tau <- 10
Vrest <- 0
Vth <- 1

# Initial voltage
V <- Vrest

# Storage vectors
voltage <- numeric(steps)
current <- numeric(steps)
spike <- numeric(steps)

# Time vector
time <- seq(0, by = dt, length.out = steps)

# Simulation
for(i in 1:steps) {
  
  # Current pulse
  if(i > 200 && i < 700) {
    I <- 1.5
  } else {
    I <- 0
  }
  
  # Integrate-and-fire equation
  dV <- (-V + I) / tau
  
  # Euler update
  V <- V + dV * dt
  
  # Spike and reset
  if(V >= Vth) {
    spike[i] <- 1
    V <- Vrest
  }
  
  # Save values
  voltage[i] <- V
  current[i] <- I
}

# Print total number of spikes
cat("Total spikes =", sum(spike), "\n")

# Clear old graphics devices
graphics.off()

# Plot voltage
plot(time, voltage,
     type = "l",
     col = "red",
     lwd = 2,
     ylim = c(0, 1.6),
     main = "Integrate-and-Fire Neuron",
     xlab = "Time",
     ylab = "Voltage / Current")

# Add current trace
lines(time, current,
      col = "blue",
      lwd = 2)

# Add spike markers
points(time[spike == 1],
       rep(Vth, sum(spike)),
       pch = 16,
       col = "black")

# Add legend
legend("topright",
       legend = c("Voltage", "Current", "Spikes"),
       col = c("red", "blue", "black"),
       lty = c(1, 1, NA),
       pch = c(NA, NA, 16))

# Explanation:
# When the current pulse ends, the voltage gradually
# returns to the resting potential rather than dropping
# instantly. This happens because the model includes a
# leak term (-V/tau). Without input current, the voltage
# decays back toward the resting potential. Tau controls
# how quickly this decay occurs.
     
