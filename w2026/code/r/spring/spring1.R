# Set your initial values

# Initial Values
init_v <- ?
init_s <- ?
init_p <- ?
default_dt <- ?

s_of_t <- function(dt = default_dt, v, s) {
    return((v * dt) + s)
}

v_of_t <- function(?,?,?) {
 ##    what goes here
 ##    Look below at a_of_t for hints   
}


a_of_t <- function(p, s) {
    return(-1 * p * s)
}

## Test that all your functions work. First, after you have
## edited the code you will save it, and then you will open up
## your R intepreter and you will source this file ~source("spring1.R")~. No tildes. Those just mean "this is code."
## 
## Then in that interpreter you can test your functions with
## commands like ~v_of_t(something here)~ and see if you get a
## sensible number back.
## 
## If everything checks out move on to the next file. 
