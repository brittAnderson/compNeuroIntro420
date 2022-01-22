"""" Oscillator

Author: Parmandeep Chaddha
Date: Jan 20, 2022
"""
from pathlib import Path
import matplotlib.pyplot as plt

class Spring(object):
    def __init__(self,
        initial_position: float,
        initial_speed: float,
        spring_constant: float,
        delta_t: float
    ):
        self.spring_constant: float = spring_constant
        self.delta_t: float = delta_t
        self.positions: list = [initial_position]
        self.speeds: list = [initial_speed]
        self.accelerations: list = []
        self.times: list = [0]

        self._update_acceleration()

    def release_spring(self, time: float):
        if time < self.delta_t:
            raise ValueError(f"Time should be larger than {self.delta_t}!")
        current_time = 0
        while current_time < time + (self.delta_t):
            current_time += self.delta_t
            self.times.append(current_time)
            self._update_acceleration()
            self._update_velocity()
            self._update_position()

    def _update_acceleration(self):
        acc: float = -1 * self.positions[-1] * self.spring_constant
        self.accelerations.append(acc)

    def _update_velocity(self):
        vel: float = self.speeds[-1] + self.accelerations[-1]*self.delta_t
        self.speeds.append(vel)

    def _update_position(self):
        pos: float = self.positions[-1] + self.speeds[-1]*self.delta_t
        self.positions.append(pos)

    def plot_spring(self, save_path: Path = None):
        fig, axs = plt.subplots(3, 1, figsize=(12, 8), sharex=True)
        axs[0].plot(self.times, self.positions)
        axs[0].set_title("Position")

        axs[1].plot(self.times, self.speeds)
        axs[1].set_title("Velocity")

        axs[2].plot(self.times, self.accelerations)
        axs[2].set_title("Acceleration")

        fig.suptitle('Oscillator Without Dampening')
        if save_path is not None:
            plt.savefig(save_path)
        plt.show()

if __name__ == "__main__":
    sprint_constant: float = 2
    initial_position: float = 10
    initial_velocity: float = -2
    delta_t = 0.1

    total_time: float = 100.0
    save_path = Path("./juliapsych420/assignment3/oscillator_without_dampening.png")

    spring = Spring(initial_position, initial_velocity, sprint_constant, delta_t)
    spring.release_spring(total_time)
    spring.plot_spring(save_path)
    

