# Initial Values
init_v <- 0
init_s <- 10
init_p <- 20.0
default_dt <- 0.05


s_of_t <- function(dt = default_dt, v, s) {
    return((v * dt) + s)
}

v_of_t <- function(dt = default_dt, a, v) {
    return((a * dt) + v)
}

a_of_t <- function(p, s) {
    return(-1 * p * s)
}

local_loop <- function(ip, ins, iv, it, idt = default_dt) {
    la <- a_of_t(ip, ins)
    lv <- v_of_t(idt, la, iv)
    ls <- s_of_t(idt, lv, ins)
    lt <- it + idt
    return(c(ip, ls, la, lv, lt))
}

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

plot_spring <- function() {
    data <- release_spring()
    times <- sapply(data, function(x) x[5])
    positions <- sapply(data, function(x) x[2])
    plot(times, positions, type = "l",
         xlab = "Time", ylab = "Position")
}
