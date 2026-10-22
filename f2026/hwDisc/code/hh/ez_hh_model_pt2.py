# this is the "altered" version of hh_model.py

import os
import matplotlib, numpy, math, random
import numpy as np
import matplotlib.pyplot as p

""" 
from the Hafez & Gottschalk article,
i picked row labelled "T226A"
    yA = scales the gate opening rate down 50%
    yB = scales the gate closing rate down 50%
    yK = scales conductance to 5%
> basically weaker potassium current => harder for the neuron repolarize
> voltage plateau means the currents are balanced
> ie inward + injected current balances outward potassium + leak current


AI DECLARATION:
i screenshotted the table from article, pasted in the explanatory notes
from https://link.springer.com/article/10.1007/s10827-020-00766-1/tables/1
asked Codex GPT 6.1 to explain how the changes in row 3 translate to a biological neuron, which i summarized above

then after i generated the plot, i asked why the voltage plateaus instead of going down slowly
and it says because the potassium and sodium currents somehow reach a balance state. 
from what i understand, the original HH model can also reach equilibrium, but we dont see it because it spikes
whereas here, it stays in equilibrium because we dont go low enough for repolarization to happen

if this understanding is wrong, please tell me!!!!!
"""

eNa = 115
gNa = 120
eK = -12
gK = 36
eL = 10.6
gL = 0.3

# Added: 
yA = 0.5  
yB = 0.5
yT = 1.0 # overall scaling factor is same
yK = 0.05 
deltaV = 15.0 

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

volt = [0.0]
current = []
baseCurrent = 0.0
curIn = 0
curRandRange = 0

time = [0.0]
timeStep = 0.01
duration = 0

startTime = 0
endTime = 0


def alphN (v):
    newAlN = 0.01*(10-v)/(math.exp((10-v)/10)-1)
    alNVal.append(newAlN)
    return(newAlN)

def alphM (v):
    newAlM = 0.1*(25-v)/(math.exp((25-v)/10)-1)
    alMVal.append(newAlM)
    return(newAlM)

def alphH (v):
    newAlH = 0.07*math.exp(-v/20)
    alHVal.append(newAlH)
    return(newAlH)

def betaN (v):
    newBeN = 0.125*math.exp(-v/80)
    beNVal.append(newBeN)
    return(newBeN)

def betaM (v):
    newBeM = 4*math.exp(-v/18)
    beMVal.append(newBeM)
    return(newBeM)

def betaH (v):
    newBeH = 1/(math.exp((30-v)/10)+1)
    beHVal.append(newBeH)
    return(newBeH)

def update (old, deriv):
    return(old+deriv*timeStep)

def dNMH (v,nmh):
    if nmh == nVal[-1]:
        return(yT*(yA*alphN(v-deltaV)*(1-nmh)-yB*betaN(v-deltaV)*nmh))  # NEW: scale K rates, voltage dependence
    elif nmh == mVal[-1]:
        return(alphM(v)*(1-nmh)-betaM(v)*nmh)
    elif nmh == hVal[-1]:
        return(alphH(v)*(1-nmh)-betaH(v)*nmh)

#explain why we need a special case for v == 0    
# hello !
# the "update" function takes in a previous value (the "old" value)
# but the global vars declare these lists empty, so when we first call makeNMH we should add something
# although i think we could also just initialize the lists with a starting value of 0.0
# but i dont want to change the starter code
def makeNMH (v):
    if v == 0:
        nVal.append(alphN(v)/(alphN(v)+betaN(v))), alNVal.pop(0)
        mVal.append(alphM(v)/(alphM(v)+betaM(v))), alMVal.pop(0)
        hVal.append(alphH(v)/(alphH(v)+betaH(v))), alHVal.pop(0)
    else:
        nVal.append(update(nVal[-1], dNMH(v,nVal[-1])))
        mVal.append(update(mVal[-1], dNMH(v,mVal[-1])))
        hVal.append(update(hVal[-1], dNMH(v,hVal[-1])))

def naCur (v,m,h):
    newNaCur = gNa*(m**3)*h*(v-eNa)
    naCurVal.append(newNaCur)
    return(newNaCur)

def kCur(v, n):
    newKCur = yK * gK * n**4 * (v - eK)  # NEW: multiply original max K conductance by yK.
    kCurVal.append(newKCur)
    return newKCur

def lCur(v):
    newLCur = gL * (v - eL)
    lCurVal.append(newLCur)
    return newLCur

def naKLBuild(v, n, m, h):
    return naCur(v, m, h) + kCur(v, n) + lCur(v)

def dVolt(inCur, v, n, m, h):
    return inCur - naKLBuild(v, n, m, h)

def currentBuild (bC, cIn, cRand, sT, eT):
    if time[-1] < sT or time[-1] > eT:
        current.append(bC)
    else:
        current.append(cIn+(random.uniform(-cRand,cRand)))

#What does [-1] get you in a python list?
# it gets the last item
def hHmodel (duration, baseCurrent, curIn, curRandRange, startTime, endTime):
    currentBuild(baseCurrent, curIn, curRandRange, startTime, endTime)
    makeNMH(time[-1])
    naKLBuild(0.0, nVal[-1], mVal[-1], hVal[-1])
    while time[-1] < duration:
        currentBuild(baseCurrent, curIn, curRandRange, startTime, endTime)
        makeNMH(volt[-1])
        dv = dVolt(current[-1], volt[-1], nVal[-1], mVal[-1], hVal[-1]) 
        volt.append(update(volt[-1], dv)) # separated into 2 lines for readability
        time.append(time[-1]+timeStep)

def main():
    hHmodel(120, 0, 10, 10, 5, 110)

    matplotlib.rcParams.update({'font.size': 18})
    p.rcParams['figure.figsize'] = [12, 7]
    p.grid(True)
    p.plot(time, volt)
    p.xlabel("Time (ms)")
    p.ylabel("Voltage (mV)")
    p.title("Hodgkin-Huxley (Altered, T226A)") 
    p.show()

if __name__ == "__main__":  
    main()