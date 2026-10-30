#!/usr/bin/env python3
"""
5-state, 2-symbol Busy Beaver (Marxen & Buntrock champion, proven optimal in 2024).

Runs the machine until it halts, then reports:
  - total steps executed
  - total number of 1s on the tape
  - the longest run of consecutive 1s on the final tape
"""

# (state, symbol read) -> (symbol to write, move, next state)
# Moves: +1 = right, -1 = left. 'H' = halt.
RULES = {
    ("A", 0): (1, +1, "B"), ("A", 1): (1, -1, "C"),
    ("B", 0): (1, +1, "C"), ("B", 1): (1, +1, "B"),
    ("C", 0): (1, +1, "D"), ("C", 1): (0, -1, "E"),
    ("D", 0): (1, -1, "A"), ("D", 1): (1, -1, "D"),
    ("E", 0): (1, +1, "H"), ("E", 1): (0, -1, "A"),
}


def run():
    # Tape stored as a growable list with a movable origin offset.
    size = 1 << 16
    tape = bytearray(size)
    head = size // 2
    state = "A"
    steps = 0
    rules = RULES

    while state != "H":
        write, move, nxt = rules[(state, tape[head])]
        tape[head] = write
        head += move
        state = nxt
        steps += 1

        # Grow the tape if the head nears either edge.
        if head < 8:
            tape = bytearray(size) + tape
            head += size
            size *= 2
        elif head > size - 9:
            tape = tape + bytearray(size)
            size *= 2

    return tape, steps


def longest_run_of_ones(tape):
    best = cur = 0
    for cell in tape:
        if cell:
            cur += 1
            if cur > best:
                best = cur
        else:
            cur = 0
    return best


if __name__ == "__main__":
    tape, steps = run()
    print(f"Halted after {steps:,} steps")
    print(f"Total 1s on tape: {sum(tape):,}")
    print(f"Longest run of consecutive 1s: {longest_run_of_ones(tape):,}")
