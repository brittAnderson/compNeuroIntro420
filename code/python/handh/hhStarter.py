#!/usr/bin/env python
# coding: utf-8
# PSYCH 420 HH Starter Program

import os

## May not be needed by most students
os.environ["FONTCONFIG_FILE"] = "/etc/fonts/fonts.conf"

import matplotlib, numpy, math, random
import numpy as np
import matplotlib.pyplot as p

eNa = 115
gNa = 120
eK = -12
gK = 36
eL = 10.6
gL = 0.3

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


# In[13]:


def alphN (v):
    newAlN = ??
    alNVal.append(newAlN)
    return(newAlN)

def alphM (v):
    newAlM = ??
    alMVal.append(newAlM)
    return(newAlM)

def alphH (v):
    newAlH = ??
    alHVal.append(newAlH)
    return(newAlH)

def betaN (v):
    newBeN = ??
    beNVal.append(newBeN)
    return(newBeN)

def betaM (v):
    newBeM = ??
    beMVal.append(newBeM)
    return(newBeM)

def betaH (v):
    newBeH = ??
    beHVal.append(newBeH)
    return(newBeH)

def update (old, deriv):
    return(old+deriv*timeStep)

def dNMH (v,nmh):
    if nmh == nVal[-1]:
        return(alphN(v)*(1-nmh)-betaN(v)*nmh)
    elif nmh == mVal[-1]:
        return(alphM(v)*(1-nmh)-betaM(v)*nmh)
    elif nmh == hVal[-1]:
        return(alphH(v)*(1-nmh)-betaH(v)*nmh)

#explain why we need a special case for v == 0    
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
    newNaCur = ??
    naCurVal.append(newNaCur)
    return(newNaCur)

def kCur (v,n):
    newKCur = gK*(n**4)*(v-eK)
    ??
    return(newKCur)

def lCur (v):
    newLCur = gL*(v-eL)
    ??
    return(newLCur)

def naKLBuild(v,n,m,h):
    return(??)

def dVolt (inCur,v,n,m,h):
    return(?? - (naKLBuild(v,n,m,h)))

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
        volt.append(volt[-1]+((dVolt(current[-1], volt[-1], nVal[-1], mVal[-1], hVal[-1]))*timeStep))
        time.append(time[-1]+timeStep)


c = hHmodel(120, 0, 3, 10, 5, 110)


matplotlib.rcParams.update({'font.size': 18})
p.rcParams['figure.figsize'] = [12, 7]
p.grid(True)
p.plot(time, volt)






