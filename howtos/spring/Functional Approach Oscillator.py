#Initial conditions: choose v0 = 0, p = -1, timestep = choose
#d2s/dt2 = -Ps(t)
#s(tn) ~~ s(t0) + ds/dt * DeltaT
#v(tn) ~~ v(t0) + dv/dt * DeltaT
import numpy as np
import matplotlib.pyplot as plt





#P, t0, delta_t, s0, a0, v0, delta_v, delta_s

def acceleration(p,s,damp=0, v=0):
    """an expression for the acceleration of a harmonic oscillator
    s is the position value, Q is a damping coefficient 
    p is the spring coefficient v is velocity"""
    return -p * s - damp*v
        

def updater(c0, u, delta_t):
    """provided an initial value and the corresponding
    value to update by, it will update using Euler's forward method"""
    c1 = c0 + u * delta_t
    return c1

def eulers_spring(dt=.05,p=1,s0=1,v0=0,t0=0,damp=0,plotit = True,trials = 1000):
    """dt is time step, p is the spring coefficient, s0 is the initial
    position value (extension), v0 is initial velocity, t0 is initial time
    damp provides a damp coefficient to the spring system set the damp value to 0.05 to
    counteract euler's algorithm's instability, plotit allows you to choose to plot both
    velocity and position in time, or simply one."""
    
    delta_t = dt
    

    s = np.zeros([trials+1])
    v = np.zeros([trials+1])
    t = np.zeros([trials+1])
    s[0] = s0
    v[0] = v0
    t[0] = t0
    
    for i in range(trials):
        s[i + 1] = updater(s[i],v[i], delta_t)
        v[i + 1] = updater(v[i],acceleration(p,s[i],v=v[i],damp=damp),delta_t)
        t[i + 1] = updater(t[i],1,delta_t)
    
    #s[i + 1] = s[i] + v[i] *delta_t #non functional implementation I saw online
    #v[i + 1] = v[i] + -p*s[i] * delta_t
    #t[i + 1] = t[i] + delta_t
    
    if plotit == True:
        plt.plot(t,s)
        plt.plot(t,v)
        plt.show()


print(eulers_spring(damp = 0.05))