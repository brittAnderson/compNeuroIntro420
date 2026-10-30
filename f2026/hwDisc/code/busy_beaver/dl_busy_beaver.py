# (state, symbol) -> (write, move, next_state); move: +1 right, -1 left

'''States Tables'''
# Optimal Solution
states_table = {('A', 0): (1, +1, 'B'), ('A', 1): (1, -1, 'C'),
                ('B', 0): (1, +1, 'C'), ('B', 1): (1, +1, 'B'),
                ('C', 0): (1, +1, 'D'), ('C', 1): (0, -1, 'E'),
                ('D', 0): (1, -1, 'A'), ('D', 1): (1, -1, 'D'),
                ('E', 0): (1, +1, 'H'), ('E', 1): (0, -1, 'A')}

# never halts
never_halts_table = {('A', 0): (0, +1, 'B'), ('A', 1): (0, +1, 'B'),
                ('B', 0): (0, -1, 'A'), ('B', 1): (0, -1, 'A'),
                ('C', 0): (0, +1, 'C'), ('C', 1): (0, +1, 'C'),
                ('D', 0): (0, +1, 'D'), ('D', 1): (0, +1, 'D'),
                ('E', 0): (0, +1, 'E'), ('E', 1): (0, +1, 'E')}

# halts quickly
halts_quickly_table = {('A', 0): (1, +1, 'H'), ('A', 1): (1, +1, 'H'),
                ('B', 0): (1, +1, 'H'), ('B', 1): (1, +1, 'H'),
                ('C', 0): (1, +1, 'H'), ('C', 1): (1, +1, 'H'),
                ('D', 0): (1, +1, 'H'), ('D', 1): (1, +1, 'H'),
                ('E', 0): (1, +1, 'H'), ('E', 1): (1, +1, 'H')}

def turing(table):
    tape = {}
    pos, state, steps = 0, 'A', 0
    while state != 'H':
        write, move, state = table[(state, tape.get(pos, 0))]
        tape[pos] = write
        pos += move
        steps += 1
    return tape, steps

def one_counter(tape):
    best = cur = 0
    for p in range(min(tape), max(tape) + 1):
        cur = cur + 1 if tape.get(p, 0) == 1 else 0
        best = max(best, cur)
    return best


# tape, steps = turing(states_table)
# print("steps:", steps)
# print("total ones:", sum(tape.values()))
# print("longest run of 1s:", one_counter(tape))
