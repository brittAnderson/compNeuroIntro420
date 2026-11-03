from collections import defaultdict # returns 0


# Transition table:
# (current mood, the current symbol we are on) -> (what symbol to replace our current one with, move to teh left (L or right (R)), what mood we switch into)

movement = {
    ('A', 0): (1, 'R', 'B'),
    ('A', 1): (1, 'L', 'C'),

    ('B', 0): (1, 'R', 'C'),
    ('B', 1): (1, 'R', 'B'),

    ('C', 0): (1, 'R', 'D'),
    ('C', 1): (0, 'L', 'E'),

    ('D', 0): (1, 'L', 'A'),
    ('D', 1): (1, 'L', 'D'),

    ('E', 0): (1, 'R', 'H'),
    ('E', 1): (0, 'L', 'A')
}

# this stores any symbol (0 or 1) that ive touched and consideres all the others that i didnt touch to be 0
tape = defaultdict(int)

#tells the code what state to start at and where to start and tells it that no steps from the begining have been mamde yet
state = 'A'
head = 0
steps = 0

while state != 'H': # tells it to keep foing until we reach Halt
    symbol = tape[head] # looks at what symbol its on (0 or 1)

    # tells it to find and match the rule corresponding to the state and symbol from the movement cheat sheet table
    write, move, nextstate = movement[(state, symbol)]

    tape[head] = write # replace the symbole (0 or 1) teh code is on

    # tellsit to move left or right
    if move == 'R':
        head += 1
    else:
        head -= 1

    # chnage mood/state
    state = nextstate
    steps += 1

# finds every position that contains a 1
onepositions = [i for i, value in tape.items() if value == 1]

if onepositions:  #if there is 1 anywhere
    left = min(onepositions)
    right = max(onepositions)

    # tape[i] looks at whats stored at each tape position between the range (left and right+1).
    # str writes out all teh numbers and join joins them in a line
    tapestring = ''.join(str(tape[i]) for i in range(left, right + 1))
else:
    tapestring = "" # if tehre is no 1s

# counts the total number of 1s on the tape
total_ones = tapestring.count('1')

# Find largest consecutive sequence of 1s
largest_run = 0
current_run = 0
largest_string = ""

for bit in tapestring:
    if bit == '1': # asking is this character a 1
        current_run += 1

        if current_run > largest_run:
            largest_run = current_run
            largest_string = '1' * current_run

    else:
        current_run = 0

print("Machine halted.")
print("Steps:", steps)
print("Final tape:", tapestring)
print("Total number of 1s:", total_ones)
print("Largest sequence length:", largest_run)
print("Largest sequence:", largest_string)