def xcubed (x) : return (x * x * x)
def diff_x_cubed (x) : return (3 * x * x)
def get_step (guess, goal) :
    return ((goal - (xcubed (guess))) / (diff_x_cubed (guess)))
def get_cube_root (goal, initial_guess, tolerance) :
    cur_guess = initial_guess + get_step(initial_guess, goal)
    error = abs(xcubed(cur_guess) - goal)
    while (error > tolerance):
        cur_guess = cur_guess + get_step(cur_guess, goal)
        error = abs(xcubed(cur_guess) - goal)
    return(cur_guess)
