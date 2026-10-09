library(ggplot2)

tau <- 10
R <- 1      
V_rest <- 0      
threshold <- 2
spike_peak <- 10

dt <- 0.1
times <- seq(0, 100, by = dt)
n <- length(times)

voltage <- numeric(n)
spike <- numeric(n)
current <- numeric(n)

current <- ifelse(times >= 10 & times <= 60, 4, 0)

for (i in 1:(n-1)) {
  dVdt <- (-voltage[i] + R * current[i]) / tau
  voltage[i+1] <- voltage[i] + dVdt * dt
  spike[i+1] <- voltage[i+1]
  if (voltage[i+1] > threshold) {
    spike[i+1] <- spike_peak    
    voltage[i+1] <- V_rest     
  }
}

neuron_df <- data.frame(times, voltage, current, spike)

p <- ggplot(neuron_df, aes(x = times)) +
  geom_line(aes(y = spike), color = "red") +
  geom_line(aes(y = current), color = "black") +
  labs(x = "Time", y = "Voltage & Current", title = "Integrate and Fire Neuron")

print(p)


#The drop after current ends seems to decay exponentially.
#No current input = -voltage/tau is the only thing influencing the membrane
#voltage would then leak towards its resting potential, at a rate determined by tau


#comparing I&F model to real world neuronal function 
#Real neurons are much more noisy in responses when given a constant input, and display much more irregular peaking patterns.
#Additionally, cortical neurons adapt over constant input, causing its interspike intervals to lengthen.
#The model we use fires identically in all spikes with the exact same ISI's, so it doesn't replicate this as there is no irregularity or noise. 

#The paper highlights how the same neurons fire with ~1ms timing precision across repeated trials when the input fluctuates, whereas steady input gives imprecise timings.
#The I&F model would be a resonable model of neuronal firing when the input its receiving is fluctuating rather than constant, but would still require adaptation.
