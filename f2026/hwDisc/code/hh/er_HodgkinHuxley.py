import numpy as np
import matplotlib.pyplot as plt

# Sim Parameters
simParameters = {
    "startT": 0.0,
    "stopT": 300.0,
    "hhdt": 0.05,
    "injectionStart": 100.0,
    "injectionStop": 150.0,
    "capacitance": 1.0, 
    "resistance": 2.0, 
    "startingVoltage": 0, 
    "startingCurrent": 0.0,
    "injectionCurrent": 20.0
}
# Needed because of how Python dictionaries work
simParameters.update({
    "hhtau": simParameters["capacitance"] * simParameters["resistance"],
    "time": np.linspace(simParameters["startT"], simParameters["stopT"], int((simParameters["stopT"] - simParameters["startT"]) / simParameters["hhdt"]))
})

# Neuron Parameters
neuronParameters = {
    "ena": 115,
    "gna": 120,
    "ek": -12,
    "gk": 36,
    "el": 10.6,
    "gl": 0.3
}

# Alpha m/n/h equation
def alpha(voltage, type):
    if type == "n":
        return (0.1-0.01*voltage)/(np.exp(1-0.1*voltage) - 1)
    elif type == "m":
        return (2.5-0.1*voltage)/(np.exp(2.5-0.1*voltage) - 1)
    elif type == "h":
        return 0.07*np.exp((-1*voltage)/20)

# Beta m/n/h equation
def beta(voltage, type):
    if type == "n":
        return (0.125 * np.exp((-1*voltage)/80))
    elif type == "m":
        return (4*np.exp((-1*voltage)/18))
    elif type == "h":
            return (1/(np.exp(3-0.1*voltage)+1))

# Dot equation for m/n/h
def dotDynamics(voltage, channel, type):
    return alpha(voltage, type) * (1 - channel) - beta(voltage, type) * channel

# Altered dot equation for modified n channel
def alteredDotDynamics(voltage, channel, type, gtau, galpha, gbeta, dV):
    return gtau*(galpha*alpha(voltage - dV, type)*(1 - channel) - gbeta*beta(voltage - dV, type)*channel)

# Steady state value for m/n/h
def dotInfinity(voltage, type):
    a = alpha(voltage, type)
    return a / (a + beta(voltage, type))

# Generates a voltage vs time graph for either modified or unmodified neurons
def generateCurve(isAltered, gtau, galpha, gbeta, gK, dV):
    # Creates the empty voltage and current charts
    voltage = [simParameters["startingVoltage"]]
    current = [simParameters["startingCurrent"]]
    
    # Sets our baseline m/n/h values
    m = dotInfinity(voltage[0], "m")
    n = dotInfinity(voltage[0], "n")
    h = dotInfinity(voltage[0], "h")
    
    # Needed to force the altered dvdt equation to function as nonaltered
    if isAltered == False:
        gK = 1

    # Neuron voltage update loop
    for i in range(1, len(simParameters["time"])):
        m += dotDynamics(voltage[-1], m, "m") * simParameters["hhdt"]
        h += dotDynamics(voltage[-1], h, "h") * simParameters["hhdt"]
        
        # This if statement is included to show that I technically got the program working with an unmodified channel
        if isAltered == True:
            n += alteredDotDynamics(voltage[-1], n, "n", gtau, galpha, gbeta, dV) * simParameters["hhdt"]
        else:
            n += dotDynamics(voltage[-1], n, "n") * simParameters["hhdt"]

        # Checks if current injection occurs
        if simParameters["injectionStart"] <= simParameters["time"][i] <= simParameters["injectionStop"]:
            current.append(simParameters["injectionCurrent"])
        else:
            current.append(0.0)

        # dvdt equation
        dvdt = (current[i] - (
                neuronParameters["gna"] * m**3 * h * (voltage[-1] - neuronParameters["ena"])
                + gK*neuronParameters["gk"] * n**4 * (voltage[-1] - neuronParameters["ek"])
                + neuronParameters["gl"] * (voltage[-1] - neuronParameters["el"]))) / simParameters["capacitance"]

        # voltage (update) equation
        voltage.append(voltage[-1] + dvdt * simParameters["hhdt"])

    # Returns final voltage and current arrays
    return voltage

# Voltage graph with unmodified Neuron Channels
unmodified = generateCurve(False, None, None, None, None, None)
# Voltage graph with modified n channel equation but otherwise looks unmodified
modifiedNormal = generateCurve(True, 1, 1, 1, 1, 0)
# Voltage graph with modified n channel using using Kv1.1 variant T226R variables
modifiedT226R = generateCurve(True, 1, 1/6, 1/2, 0.03, 25)

# Generates 3 graphs
plt.rcParams["figure.figsize"] = (12, 6)
figure, axis = plt.subplots(1, 3)

axis[0].plot(simParameters["time"], unmodified)
axis[0].set_title("Unmodified Hodgekin Huxley")
axis[1].plot(simParameters["time"], modifiedNormal)
axis[1].set_title("Modified Hodgekin Huxley")
axis[2].plot(simParameters["time"], modifiedT226R)
axis[2].set_title("T226R Hodgekin Huxley")
figure.supxlabel("Time (s)")
figure.supylabel("Voltage (mv)")
plt.show()