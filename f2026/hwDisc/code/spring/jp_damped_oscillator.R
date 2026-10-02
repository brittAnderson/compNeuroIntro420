# Jahnavi Patel 20949024

# DAMPED OSCILLATOR

# Setting Variables

P <- 1.0           # Spring constant
k <- 0.2           # Damping/friction constant
dt <- 0.01         # Tiny change in time

s <- 1.0           # Initial position
v <- 0.0           # Initial velocity

t_max <- 10.0                 # Total time to simulate in seconds (10 seconds)

num_steps <- t_max / dt       # Number of steps (10.0 divided by 0.01 = 1000 steps)

position_history <- numeric(num_steps)  # Numeric is a function that creates a list of zeros at a length, so 1000)
velocity_history <- numeric(num_steps)


# Calculations

for (i in 1:num_steps) {
  
  a <- -P * s - k * v     # Calculating acceleration, now subtracted by the damped constant multiplied by the velocity at current time
  v <- v + (a * dt)   # Updating Velocity: current velocity + acceleration multiplied by dt
  s <- s + ( v * dt)     # Updating position: current position + velocity multiply by dt
  
  position_history[i] <- s  # Allocates each calculation in each step to the current iteration in the index.
  velocity_history[i] <- v
  
}

# Plotting Results

time_axis <- seq(from = 0, to = t_max - dt, by = dt)     # X axis coordinates.

# to 10.0 - 0.01 since first item is 0.00 and we need to keep it at 1000 items (same length) (otherwise would be 1001 items).
# by = dt, tells us how far to jump between numbers, so intervals of 0.01.

# Drawing position graph

plot(
  time_axis,
  position_history,
  type = "l",
  col = "purple",
  xlab = "Time (seconds)",
  ylab = "Position",
  main = "Damped Oscillator"
)

# Position history are the Y axis values

