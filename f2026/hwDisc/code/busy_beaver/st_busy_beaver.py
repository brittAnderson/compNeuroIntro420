from collections import defaultdict

def run_busy_beaver_5():
    tape = defaultdict(int)
    head = 0
    state = 'A'
    steps = 0

    transitions = {
        'A': {0: (1, 'R', 'B'), 1: (1, 'L', 'C')},
        'B': {0: (1, 'R', 'C'), 1: (1, 'R', 'B')},
        'C': {0: (1, 'R', 'D'), 1: (0, 'L', 'E')},
        'D': {0: (1, 'L', 'A'), 1: (1, 'L', 'D')},
        'E': {0: (1, 'R', 'HALT'), 1: (0, 'L', 'A')}
    }

    print("Simulating the BB(5) Turing machine")

    while state != 'HALT':
        symbol = tape[head]
        write_val, direction, next_state = transitions[state][symbol]
        tape[head] = write_val
        head += 1 if direction == 'R' else -1
        state = next_state
        steps += 1

    ones_count = sum(tape.values())
    print(f"\nMachine halted successfully!")
    print(f"Total steps: {steps:,}")
    print(f"Total 1s on Tape: {ones_count}")

if __name__ == "__main__":
    run_busy_beaver_5()