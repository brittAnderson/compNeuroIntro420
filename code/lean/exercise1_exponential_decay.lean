-- Exercise 1: Exponential Decay
-- Equation:  y = e^(1/x)
--
-- Steps:
--   1. Compute y for 6 x-values (x = 1 through 6).
--   2. Write the (x, y) pairs to a CSV file so they can be plotted
--      (e.g. by opening the CSV in Excel, or loading it into another
--      tool like Python/matplotlib). Lean4 has no built-in charting
--      library, so a CSV is the "plot data" deliverable here.

-- ----- Step 1: Compute the numbers needed for the plot -----

-- The 6 x-values requested: 1, 2, 3, 4, 5, 6
-- (Float literals like `1.0` are used since `Float.exp` needs Float, not Nat)
def xValues : List Float := [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]

-- Apply the equation y = e^(1/x) to a single x value
def expDecay (x : Float) : Float :=
  Float.exp (1.0 / x)

-- Pair up each x with its computed y = e^(1/x)
def computePairs (xs : List Float) : List (Float × Float) :=
  xs.map (fun x => (x, expDecay x))

-- ----- Step 2: Format the results as CSV text -----

-- Turn one (x, y) pair into a "x,y" CSV row
def rowToCsv (pair : Float × Float) : String :=
  s!"{pair.fst},{pair.snd}"

-- Build the full CSV file contents: a header line, then one row per pair,
-- each joined by a newline
def toCsv (pairs : List (Float × Float)) : String :=
  let header := "x,y"
  let rows := pairs.map rowToCsv
  String.intercalate "\n" (header :: rows)

-- ----- Step 3: Run everything and write the output -----

def main : IO Unit := do
  -- Compute the (x, y) pairs from our 6 x-values
  let pairs := computePairs xValues

  -- Print each pair to the terminal so we can see the numbers directly
  for (x, y) in pairs do
    IO.println s!"x = {x}, y = e^(1/x) = {y}"

  -- Build the CSV text and write it next to this script
  let csvContents := toCsv pairs
  IO.FS.writeFile "exponential_decay.csv" csvContents

  IO.println "Wrote exponential_decay.csv (plot this file's x,y columns to see the decay curve)."
