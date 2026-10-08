threshold <- 2
resting <- 0
tau <- 1
dt <- 0.01
n <- 1000

#vectors
voltage <- numeric(n)
spike <- numeric(n)
inputcurrent <- numeric(n)

#initial condition
voltage[1] <- resting

#loop
for(i in 2:n){
  current <- 0
  if(i > 100 & i < 600){
    current <- 3
  }
  inputcurrent[i] <- current
  voltchange <- ((resting - voltage[i-1]) +
                   current) / tau
  voltage[i] <- voltage[i-1] +
    voltchange * dt
  spike[i] <- voltage[i]
  if(voltage[i] >= threshold){
    spike[i] <- 5
    voltage[i] <- resting
  }
}

#plot
plot(
  spike,
  type = "l",
  main = "Integrate and Fire Neuron",
  xlab = "Time",
  ylab = "Voltage"
)

lines(
  inputcurrent,
  col = "red",
  lwd = 1
)

#explanation
#after the current ends, the voltage gradually returns 
#to the resting position because there is no more input pushing 
#it upwards and the leakage of tau. 
