import numpy as np

m_min = -2.0
m_max =  2.0
x_min = -20
x_max = 20
num_pts = 20

def quick_random_float(mn,mx,n=None):
    return((mx - mn) * np.random.random_sample(n) + mn)

def mk_array_possibleX (n, xn = x_min, xx = x_max):
    return(np.linspace(xn,xx,3*n))

def mk_array_possibleY (xarray,slope,intercept):
    return(np.array(list(map (lambda x: slope*x + intercept, xarray))))

def mk_array_noisyY (yarray,noise_mag = 0.1):
    qrf = quick_random_float(-1.0 * noise_mag,noise_mag,yarray.size)
    return(yarray + qrf)

def mk_test_data(n,sl,incpt,nmg):
    xs = mk_array_possibleX(n)
    ys = mk_array_possibleY(xs,sl,incpt)
    nys = mk_array_noisyY(ys,nmg)
    ds = np.array([xs,ys,nys]).copy().T
    def class_test (vs): return(1 if vs[1] >= vs[2] else -1)
    return(np.array([np.append(v,class_test(v)) for v in ds]))

# An example on how to use it:
# mk_test_data(num_pts,quick_random_float(m_min,m_max),quick_random_float(m_min,m_max),abs(quick_random_float(m_min,m_max)))
