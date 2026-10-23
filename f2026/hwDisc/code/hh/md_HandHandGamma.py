import os

import math, random, numpy, matplotlib
import numpy as np
import matplotlib.pyplot as plt

ena = 115 
gna = 120 
ek = -12 
gk = 36 
el = 10.6 
gl = 0.3 

alNVal = []
alMVal = []
alHVal = []
beNVal = []
beMVal = []
beHVal = []

nVal = []
mVal = []
hVal = []

naCurVal = []
kCurVal = []
lCurVal = []
BuildVal = []

current = []
baseCurrent = 0.0
curIn = 0.0
curRandRange = 0

time = [0.0]
hhdt = 0.01
initT = 0.0
startTime = 0.0
endTime = 0.0

cap = 1.0

volt = [0.0]

gr = 1.0 #6.0
ga = 1.0 #6.0
gb = 1.0 #6.0
gamk = 1.0 #0.08
delV = 0.0 #64.0

n = np.zeros(len(time))
m = np.zeros(len(time))
h = np.zeros(len(time))

def alphN(v):
    newAlN = (0.01*(10-v))/(np.exp((10 - v)/10)-1)
    alNVal.append(newAlN)
    return(newAlN)

def alphM(v):
    newAlM = 0.1*((25-v)/(np.exp((25-v)/10)-1))
    alMVal.append(newAlM)
    return(newAlM)

def alphH(v):
    newAlH = 0.07*np.exp(-v/20)
    alHVal.append(newAlH)
    return(newAlH)

def betaN(v):
    newBeN = 0.125*np.exp(-v/80)
    beNVal.append(newBeN)
    return(newBeN)

def betaM(v):
    newBeM = 4*np.exp(-v/18)
    beMVal.append(newBeM)
    return(newBeM)

def betaH(v):
    newBeH = 1/(np.exp((30-v)/10) +1)
    beHVal.append(newBeH)
    return(newBeH)

def update (old, deriv):
    return(old+deriv*hhdt)

def dN (v,n):
        return(gr*(ga*alphN(v-delV)*(1-n)-(gb*betaN(v-delV)*n)))

def dM (v,m):
        return(alphM(v)*(1-m)-betaM(v)*m)

def dH (v,h):
        return(alphH(v)*(1-h)-betaH(v)*h)

#explain why we need a special case for v == 0; v(0) is the infinity/resting state   
def makeNMH (v):
    if v == 0:
        nVal.append(alphN(v)/(alphN(v)+betaN(v))), alNVal.pop(0)
        mVal.append(alphM(v)/(alphM(v)+betaM(v))), alMVal.pop(0)
        hVal.append(alphH(v)/(alphH(v)+betaH(v))), alHVal.pop(0)
    else:
        nVal.append(update(nVal[-1], dN(v,nVal[-1])))
        mVal.append(update(mVal[-1], dM(v,mVal[-1])))
        hVal.append(update(hVal[-1], dH(v,hVal[-1])))

def naCur (v,m,h):
    newNaCur = gna*(m**3)*h*(v-ena)
    naCurVal.append(newNaCur)
    return(newNaCur)

def kCur (v,n):
    newKCur = gk*(n**4)*(v-ek)
    kCurVal.append(newKCur)
    return(newKCur)

def lCur (v):
    newLCur = gl*(v-el)
    lCurVal.append(newLCur)
    return(newLCur)

def naKLBuild(v,n,m,h,):
    naKLBuild = (gna*m**3*h*(v-ena)+gamk*gk* n**4*(v-ek)+gl*(v-el))
    BuildVal.append(naKLBuild)
    return(naKLBuild)

#injecting current to get dVolt, need curIn to function; also cap just equals one but still included 
def dVolt (curIn,v,n,m,h):
    return (curIn - naKLBuild(v,n,m,h))/cap

def currentBuild (bC, cIn, cRand, sT, eT):
    if time[-1] < sT or time[-1] > eT:
        current.append(bC)
    else:
        current.append(cIn+(random.uniform(-cRand,cRand)))

#What does [-1] get you in a python list?
def hHmodel (duration, baseCurrent, curIn, curRandRange, startTime, endTime):
    currentBuild(baseCurrent, curIn, curRandRange, startTime, endTime)
    makeNMH(time[-1])
    naKLBuild(0.0, nVal[-1], mVal[-1], hVal[-1])
    while time[-1] < duration:
        currentBuild(baseCurrent, curIn, curRandRange, startTime, endTime)
        makeNMH(volt[-1])
        volt.append(volt[-1]+((dVolt(current[-1], volt[-1], nVal[-1], mVal[-1], hVal[-1]))*hhdt))
        time.append(time[-1]+hhdt)
      
c = hHmodel(120, 0, 3, 10, 5, 110)

matplotlib.rcParams.update({'font.size': 18})
plt.figure(1, figsize=(12, 7))
#plt.rcParams['figure.figsize'] = [12, 7]
plt.subplot (2,1,1)
plt.title('Hodgkin and Huxley Simulation')
plt.grid(True)
plt.plot(time, volt)
plt.legend('V', loc = 'upper right')
plt.ylabel('Voltage (mV)')
plt.xlabel ('Time (msec)')
plt. subplot (2,1,2)
plt.plot (time,nVal, color = 'red', label = 'n')
plt.plot (time, mVal, color = 'blue', label = 'm')
plt.plot (time, hVal, color='green', label = 'h')
plt.ylabel ('Gating Variables')
plt.xlabel ('time(ms)')
plt.legend (loc = 'upper right')
plt.show()