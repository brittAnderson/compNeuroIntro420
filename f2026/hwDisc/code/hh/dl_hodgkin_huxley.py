import matplotlib.pyplot as mpl
import math

# constants
c = {
    'cm': 1.0, # membrane capacitance
    'na_max': 120.0, # max sodium conductance
    'k_max': 36.0, # max potassium conductance
    'leak': 0.3, # leak conductance
    'r_na': 50.0, # sodium reversal potential
    'r_k': -77.0, # potassium reversal potential
    'r_leak': -54.387, # leak reversal potential
    'rest': -65.0, # resting potential
    'amp': 10.0, # amplitude
    'offset1': 10, # pulse offset
    'offset2': 90, # pulse end
    'n_x': 100, # x
    'intv': 0.01} # interval

normal = {'t': 1.0, 'a': 1.0, 'b': 1.0, 'dV': 0.0}
mutant = {'t': 1.0, 'a': 0.5, 'b': 0.5, 'dV': 15.0}

def x1(x,z):
    lstx = []
    y = 0
    while y < x:
        lstx.append(y)
        y += z
    return lstx
 
def rates(v, g):
    vs = v - g['dV']
    return [(0.1*(v+40)/(1-math.exp(-(v+40)/10)), 4*math.exp(-(v+65)/18)),
            (0.07*math.exp(-(v+65)/20), 1/(1+math.exp(-(v+35)/10))),
            (g['t']*g['a']*0.01*(vs+55)/(1-math.exp(-(vs+55)/10)),
             g['t']*g['b']*0.125*math.exp(-(vs+65)/80))]
 
def y1(y, intv, g):
    lsty = []
    volt = c['rest']
    gates = [a/(a+b) for a, b in rates(volt, g)]
    for t in y:
        m, h, n = gates
        curr = c['amp'] if c['offset1'] <= t < c['offset2'] else 0.0
        i_na = c['na_max'] * m**3 * h * (volt - c['r_na'])
        i_k = c['k_max'] * n**4 * (volt - c['r_k'])
        i_l = c['leak'] * (volt - c['r_leak'])
        gates = [x + intv * (a*(1-x) - b*x) for x, (a, b) in zip(gates, rates(volt, g))]
        volt += intv * (curr - i_na - i_k - i_l) / c['cm']
        lsty.append(volt)
    return lsty
 
 
x = x1(c['n_x'], c['intv'])

mpl.figure()
mpl.plot(x, y1(x, c['intv'], normal), color='C0', label='original')
mpl.plot(x, y1(x, c['intv'], mutant), color='C3', label='T226A')

 
mpl.show()