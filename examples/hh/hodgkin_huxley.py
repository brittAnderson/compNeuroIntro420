import matplotlib.pyplot as plt
import numpy as np

START_T = 50.0
END_T = 250.0
T_MID_1 = (END_T - START_T)/2 # define bounds to stop current
T_MID_2 = T_MID_1 + START_T
MAX_T = 300.0
DELTA_T = 0.01

CAPACITANCE = 1.0
RESISTANCE = 2.0

V_THRESHOLD = 6.0
RESET_VOLTAGE = -1.0
VOLTAGE_TOLERANCE = 0.1
SPIKE_LEVEL = 50.0

INIT_V = 0.0
INIT_I = 0.0
INIT_T = 0.0

INJECTION_CURRENT = 7.0
TAU = RESISTANCE * CAPACITANCE

class HodgkinHuxleyModel:
    ena = 115.0
    gna = 120.0
    ek = -12.0
    gk = 36.0
    el = 10.6
    gl = 0.3

    def __init__(self):
        self.v = INIT_V
        self.t = INIT_T
        self.i = INIT_I

        self.m = self.m_inf()
        self.n = self.n_inf()
        self.h = self.h_inf()

        # List of entries to make graphs
        self.v_list = []
        self.t_list = []
        self.i_list = []

    def alpha_n(self):
        exp = (10 - self.v)/10
        return (0.01 * (10 - self.v)) / (np.exp(exp) - 1)

    def alpha_m(self):
        exp = (25 - self.v)/10
        return (0.1 * (25 - self.v)) / (np.exp(exp) - 1)

    def alpha_h(self):
        return 0.07 * np.exp(-self.v/20)
    
    def beta_n(self):
        return 0.125 * np.exp(-self.v/80)
    
    def beta_m(self):
        return 4 * np.exp(-self.v/18)

    def beta_h(self):
        exp = (30 - self.v) / 10
        return 1 / (np.exp(exp) + 1)

    def m_deriv(self):
        return ( self.alpha_m() * (1 - self.m) ) - (self.beta_m() * self.m)
    
    def n_deriv(self):
        return ( self.alpha_n() * (1 - self.n) ) - (self.beta_n() * self.n)

    def h_deriv(self):
        return ( self.alpha_h() * (1 - self.h) ) - (self.beta_h() * self.h)
    
    def m_inf(self):
        return self.alpha_m() / (self.alpha_m() + self.beta_m())

    def n_inf(self):
        return self.alpha_n() / (self.alpha_n() + self.beta_n())

    def h_inf(self):
        return self.alpha_h() / (self.alpha_h() + self.beta_h())

    def update(self, curr_val, derivative):
        return curr_val + (derivative * DELTA_T)

    def i_na(self):
        return self.gna * np.power(self.m, 3) * self.h * (self.v - self.ena)

    def i_k(self):
        return self.gk * np.power(self.n, 4) * (self.v - self.ek)
    
    def i_l(self):
        return self.gl * (self.v - self.el)

    def i_inj(self):
        """[Step function for external current]
        - step up to INJECTION_CURRENT at t > START_T
        - step down to 0 at t > T_MID_1
        - step up to INJECTION_CURRENT at t > T_MID_2
        - step down to 0 at t > END_T

        Returns:
            [float]: [current based on current time]
        """        
        return INJECTION_CURRENT * (self.t > START_T) - INJECTION_CURRENT * (self.t > T_MID_1) + INJECTION_CURRENT * (self.t > T_MID_2) - INJECTION_CURRENT * (self.t > END_T)

    def dv_dt(self):
        return self.i - self.i_na() - self.i_k() - self.i_l()

    def execute_hh_model(self):
        while self.t < MAX_T:
            # Retrieve current based on current time and update voltage accordingly
            self.i = self.i_inj()
            self.v = self.update(self.v, self.dv_dt())

            # Append to lists for plotting graph
            self.v_list.append(self.v)
            self.t_list.append(self.t)
            self.i_list.append(self.i)
            self.t += DELTA_T

            # update m, n, h
            self.m = self.update(self.m, self.m_deriv())
            self.n = self.update(self.n, self.n_deriv())
            self.h = self.update(self.h, self.h_deriv())

    def graph(self):
        """[Function to plot the I&F graph. Retrieves the desired voltage and time coordinates for plotting, defines the plot table, axis labels and line size]
        """  
        plt.title("Hodgkin-Huxley Spiking Simulation (I = " + str(INJECTION_CURRENT) + ")")
        plt.xlabel("time (sec)")
        plt.ylabel("Voltage V(t)")
        plt.scatter(self.t_list, self.v_list, s=1, color="black")
        plt.plot(self.t_list, self.i_list, color="red")
        plt.plot(self.t_list, self.v_list)
        plt.show()


if __name__ == "__main__":
    hh = HodgkinHuxleyModel()

    hh.execute_hh_model()

    hh.graph()
