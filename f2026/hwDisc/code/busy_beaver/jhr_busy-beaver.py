#!/usr/bin/env python
# coding: utf-8

# ## Busy Beaver Assignment

# Turing Machine Requirements: 
# 
# - A finite alphabet (0, 1)
# - A finite set of states
# - Capacity to read and write to a single location (dictionary)
# - A set of instructions (match/case)

# Busy Beaver Requirements: 
# 
# - Get as many consecutive '1' values on the tape as possible before halting, using 2 symbols (0,1) and 5 states.
# 
# Outputs (3): 
# 1. A symbol to write over the symbol in current cell
# 2. A direction to move (left or right relative to current cell)
# 3. A state to transition into (could be halt)

# ### The Process

# I quickly realized that I needed to carefully plan out my transition table to maximize the number of consecutive 1s my machine could produce.
# 
# I started by mapping out the logic as seen in the image below. My initial version produced only 7 consecutive 1s so I used that to start my machine
# 
# ![Busy Beaver Transition Diagram](visual-busy-beaver.png)

# ### Setting up my dictionary / variables

# In[1]:


tape = {} #tape is stored in a dictionary
position = 0 #tracks current position 
state = "A" #tracks current machine state


# In[2]:


def transition(position, state):
    symbol = tape.get(position, 0)
    match (state, symbol):
        case("A", 0):
            tape[position] = 1
            position += 1 #move right
            state = "B"
        case("A", 1):
            tape[position] = 1
            position += 1
            state = "A"
        case("B", 0): 
            tape[position] = 1
            position += 1
            state = "C"
        case("B", 1):
            tape[position] = 1
            position -= 1 #move left
            state = "B"
        case("C", 0): 
            tape[position] = 1
            position += 1
            state = "D"
        case("C", 1):
            tape[position] = 1
            position += 1
            state = "H" #halt
        case("D", 0):
            tape[position] = 1
            position -= 1
            state = "E"
        case("D", 1):
            tape[position] = 0
            position += 1
            state = "A"
        case("E", 0): 
            tape[position] = 0
            position -= 1
            state = "B"
        case("E", 1):
            tape[position] = 1
            position -= 1
            state = "E"
    return position, state


# In[3]:


steps = 0
longest = 0
while state != "H" and steps < 100:
    position, state = transition(position, state)
    steps += 1

    current = 0
    for n in sorted(tape):
        if tape[n] == 1: 
            if tape.get(n -1, 0) == 1: 
                current += 1 
            else:
                current = 1
        else:
            current = 0

        if current > longest:
            longest = current

print("Total steps:", steps)
print("Final number of consecutive 1s: ", longest)
print("Halted:", state == "H")

