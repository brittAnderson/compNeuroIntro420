"""
Simulates a leaky integrate and fire neuron with Euler's method and plots it
Note: The Haskell code was adapted to Python with Claude Opus 5.5 on the medium effort level
"""
import matplotlib.pyplot as plt

#Defines step size (DT) and number of iterations (STEPS)
DT, STEPS = 0.01, 1050

#Initial value of voltage (v)
v = 0.0

rest = 0 #resting potential
threshold = 3 #depolarisation threshold
tau = 1 #membrane time constant; sets how quickly v relaxes back toward rest
R = 1 #membrane resistance
reset_potential = rest #voltage the membrane returns to after a spike
spike_peak = 10 #height drawn for each spike on the plot; does not affect the dynamics
spike_half_steps = 5 #samples used to draw each side of the spike; integration pauses for 2 * spike_half_steps
I_on = 5 #input current while the pulse is on
input_off_time = 5.5 #time at which the input current switches off, partway through a ramp

def current(t):
    """Returns the input current at time t: I_on until input_off_time, then zero."""
    if t < input_off_time:
        return I_on
    return 0

vs = [v] #vs is a list storing the voltages as they are updated
pending = [] #spike samples still waiting to be drawn
for step in range(STEPS - 1):
    """
    Loop runs 1049 times because the initial value is given
    Updates the voltage, checks for a spike, and appends the result to the list vs
    """
    if pending:
        vs.append(pending.pop(0)) #draws the spike while the model is paused
        continue

    I = current(step * DT)
    v += (DT / tau) * (-(v - rest) + R * I) #leak toward rest plus input current

    if v >= threshold:
        rise = [v + (spike_peak - v) * k / spike_half_steps for k in range(1, spike_half_steps + 1)]
        fall = [spike_peak + (reset_potential - spike_peak) * k / spike_half_steps for k in range(1, spike_half_steps + 1)]
        pending = rise + fall
        vs.append(v)
        v = reset_potential #resets the voltage after a spike
    else:
        vs.append(v) #appends the new voltage value to the list of voltages vs

t = [i * DT for i in range(STEPS)] #creates a list of time values
plt.plot(t, vs, label="membrane potential") #plots voltage over time, and names what's being plotted
plt.axvline(input_off_time, linestyle="--", color="gray", label="input off") #marks when the current stops
plt.xlabel("time") #labels the x-axis
plt.ylabel("voltage") #labels the y-axis
plt.legend() #creates a legend which shows the name for the plotted function
plt.show() #displays the plot
