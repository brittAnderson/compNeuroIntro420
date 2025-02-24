import matplotlib
import matplotlib.pyplot as p
import random as r
import numpy as np
from copy import copy,deepcopy

def hopGenSzN ():
    pn = r.randint(4,8)
    psz = np.repeat(r.randint(6,10),2)
    return((pn,psz))

def hopMkPatts(numPs2Make,psize):
    ps = []
    for n in range(numPs2Make):
        tobepatt = np.array([[(np.round(r.random()) *2) - 1 for i in range(psize[0])] for j in range(psize[1])])
        ps.append(tobepatt)
    return(ps)

def hopMkWts(patterns):
    w = np.zeros(list(map((lambda x: x**2),patterns[0].shape)))
    for p in patterns:
        w = w + np.outer(p,p)
    w = 1.0/len(patterns) * w
    np.fill_diagonal(w,0)
    return(w)

def hopLoop(patt,wts):
    workingpatt = deepcopy(patt)
    testpatt    = deepcopy(patt)
    while True:
        rws = list(range(patt.shape[0]))
        cls = list(range(patt.shape[1]))
        r.shuffle(rws)
        r.shuffle(cls)
        linpatt = np.reshape(workingpatt,(1,workingpatt.size))
        for rw in rws:
            for cl in cls:
                workingpatt[rw][cl] = 1.0 if np.dot(linpatt,wts[rw*len(rws) + cl]) > 0 else -1.0
        if (np.all(workingpatt == testpatt)):
            break
        else:
            testpatt = deepcopy(workingpatt)
    return(workingpatt)

def hopPlot(ins,outs): 
    r = len(ins)
    c = 3
    pltcntr = 1
    for i in range(r):
        p.subplot(r,3,pltcntr)
        p.imshow(ins[i])
        p.subplot(r,3,(pltcntr+1))
        p.imshow(outs[i])
        p.subplot(r,3,(pltcntr+2))
        p.imshow(ins[i]-outs[i])
        pltcntr = pltcntr+3
    return(p)

myn,mysz = hopGenSzN()
myps = hopMkPatts(myn,mysz)
w = hopMkWts(myps)
outps = []
for inp in myps:
    op = hopLoop(inp,w)
    outps.append(op)

myp = hopPlot(myps,outps)


def testBitFlips (p,n):
    loc2flip = list(range(0,p.size))
    r.shuffle(loc2flip)
    loc2flp = loc2flip[0:n]
    cp = copy(p)
    lcp = np.reshape(cp,(1,cp.size))
    for i in loc2flp:
        lcp[0,i] = lcp[0,i]*-1
    rlcp = np.reshape(lcp,p.shape)
    return(rlcp)

# how you might test
# myps = hopMkPatts(4,(3,3))
# mywts = hopMkWts(myps)
# altp0 = testBitFlips(myps[0],mywts)
# myps[0] == altp0
# hopLoop(altp0,mywts) == hopLoop(myps[0],mywts)
