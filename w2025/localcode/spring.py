import matplotlib.pyplot as plt

# Initial Values
init_v = 0
init_s = 10
p = 2.0
dt = 0.05


def s_of_t (dt, v, s) :
    return ((v * dt) + s)
def v_of_t (dt, a, v) :
    return ((a * dt) + v)
def a_of_t (p, s) :
    return (-1 * p * s)

def local_loop(ip,ins,iv,it,idt):
        la = a_of_t(ip,ins)
        lv = v_of_t(idt,la,iv)
        ls = s_of_t(idt,lv,ins)
        lt = it + idt
        return (ip,ls,la,lv,lt)

def release_spring (max_time = 15, max_iter = 3000, dt = dt, p = p, s = init_s, v = init_v):
    a = a_of_t(p,s)
    localtime = 0
    vs = [(p,s,a,v,localtime)]
    for i in range(0,max_iter) :
        if (vs[-1][4] > max_time):
            return(vs)
        vs.append(local_loop(vs[-1][0],vs[-1][1],vs[-1][3],vs[-1][4],dt))
    return(vs)

def plot_spring() :
    plt.plot(*zip(*[(x[4],x[1]) for x in release_spring()]))


    
