#busy beaver simulate n = 5 
tape = {}
headloc = 0 
state = 'a'
steps = 0

while state != 'HALT' and steps < 47176870:
  #Option to print out each individual step instead of just all of the ones for the output. I commented it out since it takes a long time to run 
    #print(
    #    f"state={state}, head={headloc}, "
    #    f"read={tape.get(headloc,0)}, steps={steps}"
    #)
    current = tape.get(headloc, 0)

    if state == 'a': 
      if tape.get(headloc, 0) == 0:
        tape[headloc] = 1 
        headloc -= 1
        state = 'b'
      else: 
        tape[headloc] = 1 
        headloc -= 1
        state = 'a'

    elif state == 'b':
      if tape.get(headloc, 0) == 0:
        tape[headloc] = 1 
        headloc += 1
        state = 'c'
      else: 
        tape[headloc] = 1 
        headloc += 1
        state = 'b'

    elif state == 'c':
      if tape.get(headloc, 0) == 0:
        tape[headloc] = 1 
        headloc -= 1 
        state = 'a'
      else: 
        tape[headloc] = 1 
        headloc += 1 
        state = 'd'

    elif state == 'd':
      if tape.get(headloc, 0) == 0:
        tape[headloc] = 1 
        headloc -= 1 
        state = 'a'
      else: 
        tape[headloc] = 1 
        headloc += 1 
        state = 'e'

    elif state == 'e':
      if tape.get(headloc, 0) == 0:
        tape[headloc] = 1 
        headloc += 1
        state = 'HALT'
      else: 
        tape[headloc] = 1 
        headloc += 1 
        state = 'c'

    steps += 1

print("Halted.")
print("Steps", steps)
print("Number of 1s:", sum(tape.values()))

for i in range(min(tape.keys()), max(tape.keys()) + 1):
    print(tape.get(i, 0), end="")
print()