"""
Turing Machine: the 5n Busy Beaver Problem
Sources consulted (for the programming portion): 
    1. Used Claude 5.5 Opus to figure out the tape reading. It pointed out that a dictionary is better for this task
    when I described my list method. I was struggling with the indexing using a list of tuples.
    2. Online resources: Stack Overflow, the official Python documentation, GeeksforGeeks, and W3schools

I may come back later and create modular functions for defining transition rules, as there is a lot of similar code there.
"""

#Initialising lists of allowed symbols, states, and actions
binary = ["Zero", "One"]
states = ["A", "B", "C", "D", "E", "Halt"]
actions = ["Right", "Left"]

#dictionary to store positon and symbol values
tape = {}
init_pos = 0

#Reading position and 
#the .setdefault method takes a key, position, and returns its value. If that key isn't found, then the default value is assigned to that key
def read_tape(pos):
    symbol = tape.setdefault(pos, "Zero") 
    return symbol

#Recording to tape
def record_tape(pos, symbol=binary[1]):
    tape[pos] = symbol

#Main function
def Turing_Machine(state=states[0], position=0):
    #locally defined values that are iteratively altered
    current_state = state
    old_position = position

    #Handles the halt state being passed as an initial state
    if current_state == states[-1]:
        print("Invalid initial state: Halt state")
    else:
        while current_state != states[-1]:
            read_tape(old_position)

            #Cases for state A
            if current_state == states[0] and read_tape(old_position) == binary[0]:
                record_tape(old_position, binary[1])
                old_position += 1
                current_state = states[1]

            elif current_state == states[0] and read_tape(old_position) == binary[1]:
                old_position += -1

            #Cases for state B
            elif current_state == states[1] and read_tape(old_position) == binary[0]:
                record_tape(old_position, binary[1])
                old_position += 1
                current_state = states[2]

            elif current_state == states[1] and read_tape(old_position) == binary[1]:
                old_position += -1

            #Cases for state C
            elif current_state == states[2] and read_tape(old_position) == binary[0]:
                record_tape(old_position, binary[1])
                old_position += 1
                current_state = states[3]

            elif current_state == states[2] and read_tape(old_position) == binary[1]:
                old_position += -1

            #Cases for state D
            elif current_state == states[3] and read_tape(old_position) == binary[0]:
                record_tape(old_position, binary[1])
                old_position += 1
                current_state = states[4]

            elif current_state == states[3] and read_tape(old_position) == binary[1]:
                old_position += -1

            #Cases for E
            elif current_state == states[4] and read_tape(old_position) == binary[0]:
                record_tape(old_position, binary[1])
                old_position += -1
                current_state = states[0]

            elif current_state == states[4] and read_tape(old_position) == binary[1]:
                old_position += 1
                current_state = states[5]

            #Halt state
            elif current_state == states[5]:
                break

        #creates an ordered version of the tape by sorting the keys (otherwise the negative keys printed at the end of the dictionary)  
        ordered_tape = dict(sorted(tape.items()))
        print(ordered_tape.items())

#The function does not need to have values passed in, because of the default rules.
Turing_Machine(states[5])
