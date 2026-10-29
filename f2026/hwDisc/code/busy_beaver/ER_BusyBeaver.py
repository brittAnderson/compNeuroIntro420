"""
# Program run time counter for fun
import time
start_time = time.perf_counter()
"""
# Sets default values
machine_tape = {}
machine_state = 1
current_indice = 0

# Caching method saves time
tape_get = machine_tape.get

# Loop of machine states
while True:
    
    tape_state = tape_get(current_indice, 0)
    
    # Machine state list
    match machine_state:
        case 1:
            if tape_state == 0:
                machine_tape[current_indice] = 1
                current_indice += 1
                machine_state = 2
            elif tape_state == 1:
                current_indice -= 1
                machine_state = 3
        case 2:
            if tape_state == 0:
                machine_tape[current_indice] = 1
                current_indice += 1
                machine_state = 3
            elif tape_state == 1:
                current_indice += 1
                machine_state = 2
        case 3:
            if tape_state == 0:
                machine_tape[current_indice] = 1
                current_indice += 1
                machine_state = 4
            elif tape_state == 1:
                machine_tape[current_indice] = 0
                current_indice -= 1
                machine_state = 5
        case 4:
            if tape_state == 0:
                machine_tape[current_indice] = 1
                current_indice -= 1
                machine_state = 1
            elif tape_state == 1:
                current_indice -= 1
                machine_state = 4
        case 5:
            if tape_state == 0:
                machine_tape[current_indice] = 1
                current_indice += 1
                break
            elif tape_state == 1:
                machine_tape[current_indice] = 0
                current_indice -= 1
                machine_state = 1

# Lists number of 1s and 0s in the final array
print("# of 1s is", sum(machine_tape.values()))
print("# of 0s is", len(machine_tape) - sum(machine_tape.values()))

"""
# Program run time counter for fun pt2.
end_time = time.perf_counter()
print("Elapsed time:", end_time - start_time)
"""