# AI DECLARATION:
# stole the transition table from wikipedia (labelled "5-state, 2-symbol busy beaver")
#       (i did spend last entire night trying to come up with rules on my own)
#       (defined at line 15; replace line 42 with 43 to run it)
#       (but the max 1's i could get was 34)
#       (which is completely awful compared to 4098 but i admit defeat)
# and then i defined the values (states A-E, move left/right, write 0/1)
# had then had GPT-6.1 format it into the transition table at line 25

def main():
    A, B, C, D, E = range(5)
    HALT = -1
    LEFT, RIGHT = -1, 1

    evelyn_best_attempt = [
    # Read 0                 Read 1
    # (write, move, next)                      current state
    [(1, RIGHT, B),          (1, RIGHT, C)],   # A
    [(1, LEFT, D),           (0, RIGHT, A)],   # B
    [(1, RIGHT, E),          (1, RIGHT, D)],   # C
    [(1, LEFT, A),           (0, LEFT, B)],    # D
    [(1, RIGHT, HALT),       (0, RIGHT, D)],   # E
    ]

    transitions = [
        # Read 0          Read 1
        # write/move/nextState             # current state:
        [(1, RIGHT, B),   (1, LEFT, C)],   # A
        [(1, RIGHT, C),   (1, RIGHT, B)],  # B
        [(1, RIGHT, D),   (0, LEFT, E)],   # C
        [(1, LEFT, A),    (1, LEFT, D)],   # D
        [(1, RIGHT, HALT), (0, LEFT, A)],  # E
    ]

    ones = set() # set of tape positions holding 1s
    state = head = steps = 0

    print("Running.. (it takes a few seconds)")
    while state != HALT:
        currentSymbol = 1 if head in ones else 0

        #write, move, nextState = evelyn_best_attempt[state][currentSymbol]
        write, move, nextState = transitions[state][currentSymbol]

        if write:
            ones.add(head)
        else:
            ones.discard(head)
        head += move
        state = nextState
        steps += 1

    print("done!!")
    print("-- steps:", steps)
    print("-- ones:", len(ones))

if __name__ == "__main__":
    main()