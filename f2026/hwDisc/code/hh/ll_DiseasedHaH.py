import os

os.environ["FONTCONFIG_FILE"] = "/etc/fonts/fonts.conf"

import matplotlib, numpy, math, random
import numpy as np
import matplotlib.pyplot as plt

#constants
eNa = 115
gNa = 120
eK = -12
gK = 36
eL = 10.6
gL = 0.3

#initial values
alNVal = [0]
alMVal = [0]
alHVal = [0]
beNVal = [0]
beMVal = [0]
beHVal = [0]

nVal = [0]
mVal = [0]
hVal = [0]

naCurVal = [0]
kCurVal = [0]
lCurVal = [0]

volt = [0]
current = [0]
C = 1

#diseased initial values, according to F184C
Gtau = 1
Ga = 0.5
Gb = 1
Gk = 0.15 #unused
dV = 27

#setup
baseCurrent = 0
curIn = 3
curRandRange = 10

#time setup
time = [0]
timeStep = 0.05
duration = 120
sT = 5
eT = 80


# In[13]:


def alphN (v):
    newAlN = (0.01 * (10 - v)) / (np.exp((10-v)/10) - 1)
    alNVal.append(newAlN)
    return(newAlN)

def alphM (v):
    newAlM = (0.1 * (25 - v)) / (np.exp((25 - v)/10) - 1)
    alMVal.append(newAlM)
    return(newAlM)

def alphH (v):
    newAlH = 0.07 * np.exp((-v) / (20))
    alHVal.append(newAlH)
    return(newAlH)

def betaN (v):
    newBeN = 0.125 * np.exp((-v) / (80))
    beNVal.append(newBeN)
    return(newBeN)

def betaM (v):
    newBeM = 4 * np.exp((-v) / (18))
    beMVal.append(newBeM)
    return(newBeM)

def betaH (v):
    newBeH = (1) / (np.exp((30-v) / (10)) + 1)
    beHVal.append(newBeH)
    return(newBeH)

def update (old, deriv):
    return(old+deriv*timeStep)

def dNMH (v,nmh):           
    if nmh == nVal[-1]:
        return(Gtau*((Ga*alphN(v-dV)*(1-nmh))-(Gb*betaN(v-dV)*nmh))) #affected by disease
    elif nmh == mVal[-1]:
        return(alphM(v)*(1-nmh)-betaM(v)*nmh)
    elif nmh == hVal[-1]:
        return(alphH(v)*(1-nmh)-betaH(v)*nmh)

#explain why we need a special case for v == 0
    #to set a initial resting potential at the start of the simulation
    
def makeNMH (v):
    if v == 0:
        nVal.append(alphN(v)/(alphN(v)+betaN(v))), alNVal.pop(0)
        mVal.append(alphM(v)/(alphM(v)+betaM(v))), alMVal.pop(0)
        hVal.append(alphH(v)/(alphH(v)+betaH(v))), alHVal.pop(0)
    else:
        nVal.append(update(nVal[-1], dNMH(v,nVal[-1]))) #affected by disease
        mVal.append(update(mVal[-1], dNMH(v,mVal[-1])))
        hVal.append(update(hVal[-1], dNMH(v,hVal[-1])))

def naCur (v,m,h):
    newNaCur = gNa * (m ** 3) * h * (v - eNa)
    naCurVal.append(newNaCur)
    return(newNaCur)

def kCur (v,n):
    newKCur = gK * (n ** 4) * (v - eK)
    kCurVal.append(newKCur)
    return(newKCur) 

def lCur (v):
    newLCur = gL * (v - eL)
    lCurVal.append(newLCur)
    return(newLCur)

def naKLBuild(v,n,m,h):
    return(naCur(v, m, h) + kCur(v, n) + lCur(v))

def dVolt (inCur,v,n,m,h):
    return((inCur / C) - (naKLBuild(v,n,m,h)))

def currentBuild (bC, cIn, cRand, sT, eT):
    if time[-1] < sT or time[-1] > eT:
        current.append(bC)
    else:
        current.append(cIn+(random.uniform(-cRand,cRand)))

#What does [-1] get you in a python list? 
        #Returns the last element on a list
        
def hHmodel (duration, baseCurrent, curIn, curRandRange, startTime, endTime):
    currentBuild(baseCurrent, curIn, curRandRange, startTime, endTime)
    makeNMH(time[-1])
    naKLBuild(0.0, nVal[-1], mVal[-1], hVal[-1])
    while time[-1] < duration:
        currentBuild(baseCurrent, curIn, curRandRange, startTime, endTime)
        makeNMH(volt[-1]) #affected by disease
        volt.append(volt[-1]+((dVolt(current[-1], volt[-1], nVal[-1], mVal[-1], hVal[-1]))*timeStep))
        time.append(time[-1]+timeStep) 

c = hHmodel(duration, baseCurrent, curIn, curRandRange, sT, eT) #affected by disease

plt.rcParams.update({'font.size': 18})
plt.rcParams['figure.figsize'] = [12, 7]
plt.grid(True)              
plt.plot(time, volt)
plt.show()
