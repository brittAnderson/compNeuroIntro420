def busy_beaver_n5():
    '''
    Initializes, creates, and prints out the result of a 
        2-symbol, 5-state Turing Machine annotated tape, 
        based on the busy beaver rules found by Heiner 
        Marzen and Jurgen Buntrock. 

    Args: 
        None 
    
    Returns:
        None 
    '''
    tape = [0]
    cursor = 0
    state = "A"
    steps = 0

    # (current state, read from tape) : (new state, written on tape, move L (-1) / R (1))
    state_symbol = {
        ("A", 0) : ("B", 1, 1),
        ("A", 1) : ("C", 1, -1),
        ("B", 0) : ("C", 1, 1),
        ("B", 1) : ("B", 1, 1),
        ("C", 0) : ("D", 1, 1),
        ("C", 1) : ("E", 0, -1),
        ("D", 0) : ("A", 1, -1),
        ("D", 1) : ("D", 1, -1),
        ("E", 0) : ("HALT!", 1, 1),
        ("E", 1) : ("A", 0, -1),
    }

    print("Please hold as we compute...")

    while state != "HALT!":
        while cursor < 0:
            tape.insert(0, 0)
            cursor += 1
        while cursor >= len(tape):
            tape.append(0)
        
        curr_symbol = tape[cursor]
        new_state, new_symbol, move = state_symbol[(state, curr_symbol)]

        tape[cursor] = new_symbol
        cursor += move
        state = new_state 
        steps += 1

    # max_run = 0
    # curr_run = 0 

    # for cell in tape:
    #     if cell == 1:
    #         curr_run += 1
    #         max_run = max(max_run, curr_run)
    #     else:
    #         curr_run = 0

    print(f"Number of steps taken: {steps}")
    print(f"Number of 1s on Tape: {sum(tape)}")
    # print(f"Longest Run of 1s: {max_run}")
    print(f"Final Tape: {tape}")

busy_beaver_n5()