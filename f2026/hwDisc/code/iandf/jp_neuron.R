# Jahnavi Patel 20949024

# FIRE NEURON

# Setting Variables


tau <- 0.2           # tau = membrane resistance * membrane capacitance
dt <- 0.01         # Tiny change in time

v <- 1.0           # membrane voltage
r <- 2.0           # resistance
cur <- 1.0           # current
rest <- 0         # resting potential
thres <- 1.0      # threshold voltage

# Total time to simulate in seconds (2 seconds)
t_max <- 2.0                 

# Number of steps (2.0 divided by 0.01 = 200 steps)
num_steps <- t_max / dt       

voltage_history <- numeric(num_steps) 
current_history <- numeric(num_steps)


# Calculations

for (i in 1:num_steps) {
  
  #Rearranged formula to get only dv/dt on the left side, denoted by just dV for simplicity
  dV <- (-v + (r * cur)) / tau  
  
  # Updating voltage: current voltage + rate of change multiplied by dt
  v <- v + (dV * dt)   
  
  # Saves current voltage for this step
  voltage_history[i] <- v
  
  # Saves input current for this step
  current_history[i] <- cur   

  # Threshold Check
  
  if ( v >= thres) {
    v <- rest        # reset back to resting potential
  }
  }
  

# Plotting Results

time_axis <- seq(from = 0, to = t_max - dt, by = dt) # X axis coordinates.


# Drawing position graph

plot(
  time_axis,
  voltage_history,
  type = "l",
  lwd = 2,
  col = "purple",
  xlab = "Time (seconds)",
  ylab = "Membrane Voltage",
  main = "Integrate and Fire Neuron"
)

# Voltage history are the Y axis values

#When current is shut off, (current input is what drives voltage up away from the baseline),
#all that's left is voltage leak. this leak drains the remaining voltage back
#down to its baseline via exponential decay. 
#i tried this by doing cur = 0, which creates a negative rate of change. (-5)
#if we do cur = 1, we can see it creates a positive rate (+5)
# of change which creates an exponential decay that look upside down.
# if we keep cur = 2, and multiply that by resistance, which is 2, we get 4, which is
#greater than the threshold (3.0) making the reset happen.


