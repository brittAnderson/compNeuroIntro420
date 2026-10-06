import csv
from pathlib import Path


# After current stops, the leak term pulls voltage toward rest
# With I = 0: dV/dt = -(V - V_rest)/tau, so the decay is exponential !!
# So, it is steep at first and then flattens as voltage approaches 0 (resting)

def simulate_neuron():
    voltage = 0.0
    times = [0.0]
    volts = [voltage]
    currents = [0.0]
    spikes = [0] # set to 1 when spiking

    resting_potential = 0.0
    threshold = 1.0
    tau = 10.0
    resistance = 1.0
    dt = 0.05

    for step in range(1, 2500):
        current = currents[-1]
        change = (-(voltage - resting_potential) + resistance * current) / tau
        voltage = voltage + change * dt

        spike = 0
        if voltage >= threshold:
            spike = 1
            voltage = resting_potential

        time = step * dt
        current = 0.0
        if 10.0 <= time < 85.0:
            current = 1.5

        times.append(time)
        volts.append(voltage)
        currents.append(current)
        spikes.append(spike)

    return times, volts, currents, spikes

def main():
    times, volts, currents, spikes = simulate_neuron()

    # PART 2: we plot the data (this is ai)
    import matplotlib.pyplot as plt

    fig, axes = plt.subplots(2, 1)
    axes[0].plot(times, volts, color="pink")
    for step in range(len(times)):
        if spikes[step] == 1:
            axes[0].vlines(times[step], 1.0, 1.2, color="black")
    axes[0].set_ylabel("Voltage (mV)")

    axes[1].step(times, currents, where="post", color="black")
    axes[1].set_ylabel("Current (nA)")
    axes[1].set_xlabel("Time (ms)")

    plt.savefig(Path(__file__).with_name("evelyn_neuron_plot.png"))


if __name__ == "__main__":
    main()
