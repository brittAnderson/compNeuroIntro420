## Paste your prior work in here, both
## Spring0 and Spring 1.


def release_spring (max_time = 15, max_iter = 3000, dt = dt, p = p, s = init_s, v = init_v):
    a = a_of_t(p,s)
    localtime = 0
    vs = [(p,s,a,v,localtime)]
    for i in range(0,max_iter) :
        if (vs[-1][4] > max_time):
            return(vs)
        vs.append(local_loop(vs[-1][0],vs[-1][1],vs[-1][3],vs[-1][4],dt))
    return(vs)

## This is correct. You do not need to change anything. Just
## save and source. Use it in the python interpreter with these
## default values. Save the data it sends back, e.g.
## mydata = release_spring().

## Inspect mydata. What is mydata[1]? Is everything working?

## Then add a function to plot the data. You will have to
## figure out the the correct columns to plot. You can use
## plot  matplotlib.pyplot, and it will work well.

## The file spring.py is my entire version in case something
## horribly wrong and you need to try and figure out what
## broke, or just verify that my version works on your system.
## If not, you have something installed wrong or some other
## misunderstanding.

## For the home work figure what needs to change in this code
## (basically just a couple of lines) to get the damped
## oscillator. 
