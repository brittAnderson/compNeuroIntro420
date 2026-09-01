## Paste your prior work in here, both
## Spring1 and Spring 2.


release_spring <- function(max_time = 15, max_iter = 3000, dt = default_dt, p = init_p, s = init_s, v = init_v) {
    a <- a_of_t(p, s)
    localtime <- 0
    vs <- list(c(p, s, a, v, localtime))
    for (i in 1:max_iter) {
        if (vs[[length(vs)]][5] > max_time) {
            return(vs)
        }
        last_state <- vs[[length(vs)]] 
        new_state <-                   
            local_loop(last_state[1], last_state[2],
                       last_state[4], last_state[5], dt)       
        vs <- append(vs, list(new_state)) 
    }
    return(vs)
}


## This is correct. You do not need to change anything. Just
## save and source. Use it in the R interpreter with these
## default values. Save the data it sends back, e.g.
## mydata = release_spring().

## Inspect mydata. What is mydata[1]? Is everything working?

## Then add a function to plot the data. You will have to
## figure out the the correct columns to plot. You can use
## base plot, and it will work well. But if you want explore
## ggplot2 (you may have to learn how to install a library)
## that will be useful.

## The file spring.R is my entire version in case something
## horribly wrong and you need to try and figure out what
## broke, or just verify that my version works on your system.
## If not, you have something installed wrong or some other
## misunderstanding.

## For the home work figure what needs to change in this code
## (basically just a couple of lines) to get the damped
## oscillator. 
