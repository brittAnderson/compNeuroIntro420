from numpy import *
from pylab import *
from math  import *

T      = 50    #ms
dt     = 0.01  #ms
time   = arange(0,T+dt,dt)
V_0    = 0     #intial rest potential (mV)
I      = zeros(len(time))
V      = zeros(len(time))
V[0]   = V_0
n      = zeros(len(time))
m      = zeros(len(time))
h      = zeros(len(time))
E_Na   = 115   #mV
E_K    = -12   #mV
E_L    = 10.6  #mV
g_Na   = 120   #mS/cm2
g_K    = 36    #mS/cm2
g_L    = 0.3   #mS/cm2
C      = 1     #uF/cm2

# the K gates cotrolled by n
def alpha_n(v): 
   alpha_n = (0.1-0.01*v)/(exp(1-0.1*v) - 1) if v!=10 else 0.1
   return alpha_n
def beta_n(v):
   beta_n  = 0.125 * exp(-v/80)
   return beta_n
# the Na gates cotrolled by m
def alpha_m(v):
   alpha_m = (2.5-0.1*v)/(exp(2.5-0.1*v) - 1) if v!=25 else 1
   return alpha_m
def beta_m(v):
   beta_m  = 4*exp(-v/18)
   return beta_m
# the Na gates cotrolled by h
def alpha_h(v):
   alpha_h = 0.07*exp(-v/20)
   return alpha_h
def beta_h(v):
   beta_h  = 1/(exp(3-0.1*v)+1)
   return beta_h

for j, t in enumerate(time):
   if 20 >= t >= 5:
      I[j] = 10  #current injection (uA/cm2)
      
for j in range(1, len(time)):
   n[j] = n[j-1] + dt*(alpha_n(V[j-1])*(1-n[j-1])-beta_n(V[j-1])*n[j-1])
   m[j] = m[j-1] + dt*(alpha_m(V[j-1])*(1-m[j-1])-beta_m(V[j-1])*m[j-1])
   h[j] = h[j-1] + dt*(alpha_h(V[j-1])*(1-h[j-1])-beta_h(V[j-1])*h[j-1])
   
   V[j] = V[j-1] + (-(g_Na*(m[j-1]**3)*h[j-1]*(V[j-1]-E_Na)+g_K*(n[j-1]**4)*(V[j-1]-E_K)+g_L*(V[j-1]-E_L))+I[j-1])/C*dt  #membrane potential (mV)

plot(time, V, time, I-25)
legend(('V','I'))
ylabel('V (mV)')
xlabel('Time (msec)')
show()
