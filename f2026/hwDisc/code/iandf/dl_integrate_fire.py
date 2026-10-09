import matplotlib.pyplot as mpl

# constants
tau_m = 10.0
rest = -65.0 # resting potential
reset = -65.0 # reset potential
spike = -50.0 # spike threshold
res = 10.0 # membrane resistance
ref = 2.0 # refractory period
amp = 2.0 # amplitude
offset = 25.0 # pulse offest
offset2 = 100.0 # pulse end

n_x = 150 # number of x values
intv = 0.5 # width between each x

def x1(x,z): # generates list of x starting at 0 with interval z
    lstx = []
    y = 0
    while y < x:
        lstx.append(y)
        y += z
    return lstx

def y1(y, intv):
    lsty = []
    volt = rest
    ref_timer = 0.0
    for t in y:
        if offset <= t < offset2:
            curr = amp
        else:
            curr = 0.0
        peak = False

        if ref_timer > 0:
            volt = reset
            ref_timer -= intv
        else:
            volt = volt + (intv * (-(volt - rest) + res * curr) / tau_m)
            if volt >= spike:
                peak = True

        if peak:
            lsty.append(0.0)
            volt = reset
            ref_timer = ref
        else:
            lsty.append(volt)
            
    return lsty


mpl.plot(x1(n_x,intv),y1(x1(n_x,intv),intv), color='C0')
mpl.show()