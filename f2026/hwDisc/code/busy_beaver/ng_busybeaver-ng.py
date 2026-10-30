
# Transition table: (write, move, next state)
# Move: -1 = left, +1 = right

beaver_table = {
    'A': {0: (1, 1, 'B'), 1: (1, -1, 'C')},
    'B': {0: (1, 1, 'C'), 1: (1, 1, 'B')},
    'C': {0: (1, 1, 'D'), 1: (0, -1, 'E')},
    'D': {0: (1, -1, 'A'), 1: (1, -1, 'D')},
    'E': {0: (1, 1, 'H'), 1: (0, -1, 'A')}
}

tape = set()  # Stores positions containing 1
head = 0
state = 'A'
steps = 0

while state != 'H':
    if head in tape:
        symbol = 1
    else:
        symbol = 0

    match state:
        case 'A' | 'B' | 'C' | 'D' | 'E':
            write, move, next_state = beaver_table[state][symbol]
        case _:
            raise ValueError("Invalid state")

    if write == 1:
        tape.add(head)
    else:
        tape.discard(head)

    head += move
    state = next_state
    steps += 1

    if steps % 1_000_000 == 0:
        print(f"Completed {steps:,} steps...", flush=True)

# Find longest uninterrupted sequence of ones
longest_run = 0
current_run = 0

for position in range(min(tape), max(tape) + 1):
    if position in tape:
        current_run += 1
        longest_run = max(longest_run, current_run)
    else:
        current_run = 0

print("Final state:", state)
print("Steps before halting:", steps)
print("Total number of ones:", len(tape))
print("Longest consecutive ones:", longest_run)
print("Longest string:", "1" * longest_run)
