from collections import defaultdict

tape = defaultdict(int)

pos = 0

current_state = "A"


# 2. The official 5-state Busy Beaver champion transition table
# Source: Marxen & Buntrock (1990) champion table
# Format: ("state", symbol_read): (symbol_write, direction, "next_state")
transitions = {
    ("A", 0): (1, "r", "B"),  # 1RB
    ("A", 1): (1, "l", "C"),  # 1LC
    ("B", 0): (1, "r", "C"),  # 1RC
    ("B", 1): (1, "r", "B"),  # 1RB
    ("C", 0): (1, "r", "D"),  # 1RD
    ("C", 1): (0, "l", "E"),  # 0LE
    ("D", 0): (1, "l", "A"),  # 1LA
    ("D", 1): (1, "l", "D"),  # 1LD
    ("E", 0): (1, "r", "Z"),  # 1RZ (Z = halt state)
    ("E", 1): (0, "l", "A"),  # 0LA
}

# 3. Run the Turing machine loop until it hits the halt state 'Z'
# Step count: how many total transitions the machine performs before it halts

step_count = 0

while current_state != "Z":
    symbol = int(tape[pos])
    write_val, direction, next_state = transitions[(current_state, symbol)]

    tape[pos] = write_val

    if direction == "r":
        pos += 1
    elif direction == "l":
        pos -= 1

    current_state = next_state
    step_count += 1


# 4. Count total 1s on the tape
total_ones = sum(1 for val in tape.values() if val == 1)

print(f"Machine successfully halted after {step_count} steps.")
print(f"Total number of 1s on the tape: {total_ones}")