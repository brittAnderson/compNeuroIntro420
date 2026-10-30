#tape/position/move setup
tape = {0: "0"}
pos = 0
step = 1

#state setup
states = ["A", "B", "C", "D", "E", "H"] #H is halt
state = "A"

#safety
steps = 0
maxSteps = 1000 * 2
"""
I did not want to populate an overly large number, so
instead I used maxSteps to limit how large it could get,
as five states was said to be incomputatable, and I did
not want to go that far. This is the best of both worlds,
demonstrating the code working while still not crashing my
terminal, if that becomes the case if the amount is too
large. I don't want to risk it, just in case.
"""

while state != "H" and step < maxSteps:
    step += 1
#initial
    read = tape.get(pos, "0")
    write = read
    move = "right"
    nextState = state
#if then
    match state:
        case "A": 
            if read == "0":
                write = "1"
                nextState = "B"
                move = "left"
            elif read == "1":
                write = "1"
                nextState = "A"
                move = "left"
            else:
                print("Error")
                break
        case "B":
            if read == "0":
                write = "1"
                nextState = "C"
                move = "right"
            elif read == "1":
                write = "1"
                nextState = "B"
                move = "right"
            else:
                print("Error")
                break
        case "C": 
            if read == "0":
                write = "1"
                nextState = "C"
                move = "left"
            elif read == "1":
                write = "1"
                nextState = "B"
                move = "right"
            else:
                print("Error")
                break
        case "D":
            if read == "0":
                write = "1"
                nextState = "A"
                move = "left"
            elif read == "1":
                write = "1"
                nextState = "E"
                move = "right"
            else:
                print("Error")
                break
        case "E":
            if read == "0":
                write = "1"
                nextState = "H"
                move = "right"
            elif read == "1":
                write = "0"
                nextState = "c"
                move = "right"
            else:
                print("Error")
                break
        case "H":
            break
#adjusting tape position
    tape[pos] = write
    if move == "right":
        pos += 1
    elif move == "left":
        pos -= 1
    state = nextState

#printing - unsure how to get rid of the tape spot indicator when printing
print(f"{tape}")
