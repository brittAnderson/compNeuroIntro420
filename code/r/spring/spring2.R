## Cut and paste your Spring1.R variables and
## Functions here at the top. You are building on
## the last file with this one.


local_loop <- function(ip, ins, iv, it, idt = default_dt) {
    la <- a_of_t(ip, ins)
    ls <- s_of_t(idt, lv, ins)
    lv <- v_of_t(idt, la, iv)
    lt <- it + idt
    return(c(ip, ls, la, lv, lt))
}

## This function is our "loop" that pipes everything together.
## See how it "pipes" values through our functions, and gives
## us back new values. This loop is correct EXCEPT that I have
## changed the order of the functions. You will need to think
## about what is the right order. Once you get it rearranged
## to your liking you can save the file. Source it in your
## R interpreter and test it by doing ~local_loop(number,number
## number, number)~ and seeing if it is a sensible output.

## Ask yourself why you do not have to supply a value for ~idt~?

## Satisfied this is working. Move on to Spring3.R
