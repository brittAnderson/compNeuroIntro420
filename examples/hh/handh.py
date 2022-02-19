from math import exp
from matplotlib import pyplot as plt

# lists to store the simulation data

times = []
voltages = []


# class for iandf neuron

class neuron_sim:
    def __init__(self, dt, max_t, start_time, stop_time, injection_current):
        self.dt = dt
        self.max_t = max_t
        self.init_t = 0.0
        self.starting_time = start_time
        self.stop_time = stop_time
        self.cap = 1.0
        self.res = 2.0
        self.threshold = 3.0
        self.spike_display = 8.0
        self.init_v = 0.0
        self.injection_current = injection_current
        self.voltage = self.init_v
        self.injection_time = [self.starting_time, self.stop_time]
        self.tau = self.res * self.cap


# child class for handh neuron with additional properties

class neuron_hh(neuron_sim):
    def __init__(self, dt, max_t, start_time, stop_time, injection_current):
        super().__init__(dt, max_t, start_time, stop_time, injection_current)
        self.ena = 115.0
        self.gna = 120.0
        self.ek = -12.0
        self.gk = 36.0
        self.el = 10.6
        self.gl = 0.3


# alpha and beta helper functions

def alpha_n(volt):
    return (0.1 - (0.01 * volt)) / (exp(1 - (0.1 * volt)) - 1)

def alpha_m(volt):
    return (2.5 - (0.1 * volt)) / (exp(2.5 - (0.1 * volt)) - 1)

def alpha_h(volt):
    return 0.07 * exp((-1.0 * volt) / 20.0)

def beta_n(volt):
    return 0.125 * exp((-1.0 * volt) / 80.0)

def beta_m(volt):
    return 4.0 * exp((-1.0 * volt) / 18.0)

def beta_h(volt):
    return 1.0 / (exp(3.0 - (0.1 * volt)) + 1.0)


# derivative functions

def m_dot(volt, m):
    return (alpha_m(volt) * (1 - m)) - (beta_m(volt) * m) 

def n_dot(volt, n):
    return (alpha_n(volt) * (1 - n)) - (beta_n(volt) * n) 

def h_dot(volt, h):
    return (alpha_h(volt) * (1 - h)) - (beta_h(volt) * h) 

def m_infinity(volt):
    return alpha_m(volt) / (alpha_m(volt) + beta_m(volt))

def n_infinity(volt):
    return alpha_n(volt) / (alpha_n(volt) + beta_n(volt))

def h_infinity(volt):
    return alpha_h(volt) / (alpha_h(volt) + beta_h(volt))


# voltage updating function
def  dvdt(voltage_now, curr_in, hh_m, hh_n, hh_h, hh_neuron:neuron_hh):

    ina = hh_neuron.gna *  pow(hh_m, 3.0) * hh_h * (voltage_now - hh_neuron.ena)
    ik = hh_neuron.gk * pow(hh_n, 4.0) * (voltage_now - hh_neuron.ek)
    il = hh_neuron.gl * (voltage_now - hh_neuron.el)

    return  curr_in - (ina + ik + il)

# useful helper functions from iandf simulation
def update(old_value, rate_of_change, time_step):
  return ((rate_of_change * time_step) + old_value)

def between(x, hh_neuron:neuron_hh):
  if  (x >=  hh_neuron.injection_time[0]) and (x <= hh_neuron.injection_time[1]):
    return hh_neuron.injection_current
  else:
    return 0 

# simulation process
def run_hh_sim(neuron:neuron_hh):

    # local variables for simulation
    current_time = 0.0
    inj_cur = 0.0
    hh_m_sim = m_infinity(neuron.init_v)
    hh_n_sim = n_infinity(neuron.init_v)
    hh_h_sim = h_infinity(neuron.init_v)

    # injecting current and collecting data for graphing
    while (current_time < neuron.max_t):
        current_time = neuron.dt + current_time
        inj_cur = between(current_time, neuron)
        hh_m_sim = update(hh_m_sim, m_dot(neuron.voltage, hh_m_sim), neuron.dt)
        hh_n_sim = update(hh_n_sim, n_dot(neuron.voltage, hh_n_sim), neuron.dt)
        hh_h_sim = update(hh_h_sim, h_dot(neuron.voltage, hh_h_sim), neuron.dt)
        neuron.voltage = update(neuron.voltage, dvdt(neuron.voltage, inj_cur, hh_m_sim, hh_n_sim, hh_h_sim, neuron), neuron.dt)

        times.append(current_time)
        voltages.append(neuron.voltage)


# graphing function

def plot_graph():
  plt.plot(times, voltages)
  plt.xlabel('Time')
  plt.ylabel('Voltage')
  plt.title('Voltage-Time Graph')
  plt.show()

# collecting simulation data and graphing
run_hh_sim(neuron_hh(0.02, 450.0, 50.0, 300.0, 7.0))
plot_graph()
