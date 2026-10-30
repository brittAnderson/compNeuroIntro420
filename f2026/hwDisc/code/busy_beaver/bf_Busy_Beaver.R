#Turing machine that runs the busy beaver problem with 5 states and halts.
#Outputs largest sequential string of ones before halting.

#Starting variables
tape <- integer(50001)  #Tape length
head <- 25001
state <- "A"            #Starting state
steps <- 0
longestRun <- 0

#First column: what to do when reading 0
#Second column: what to do when reading 1
writeRules <- matrix(
  c(1, 1,
    1, 1,
    1, 0,
    1, 1,
    1, 0),
  nrow = 5, byrow = TRUE
)
rownames(writeRules) <- c("A", "B", "C", "D", "E")

moveRules <- matrix(
  c(1, -1,   #A
    1, 1,    #B
    1, -1,   #C
    -1, -1,  #D
    1, -1),  #E
  nrow = 5, byrow = TRUE
)
rownames(moveRules) <- c("A", "B", "C", "D", "E")

nextRules <- matrix(
  c("B", "C",
    "C", "B",
    "D", "E",
    "A", "D",
    "H", "A"),
  nrow = 5, byrow = TRUE
    )
rownames(nextRules) <- c("A", "B", "C", "D", "E")

#Meat and potatoes
while (state != "H") {
  #Read the current cell
  symbol <- tape[head]
  column <- symbol + 1
  #Look up the rule
  newSymbol <- writeRules[state, column]
  movement <- moveRules[state, column]
  nextState <- nextRules[state, column]
  #Write the new symbol
  tape[head] <- newSymbol
  #Check for a longer run
  if (symbol == 0 && newSymbol == 1) {
    left <- head
    right <- head
    #Find left end of ones
    while (left > 1 && tape[left - 1] == 1) {
      left <- left - 1
    }
    #Find right end of ones
    while (right < length(tape) && tape [right + 1] == 1) {
      right <- right + 1
    }
    #Calc run length
    runLength <- right - left + 1
    longestRun <- max(longestRun, runLength)
  }
  #Move head and change state
  head <- head + movement
  state <- nextState
  steps <- steps + 1 
}

cat("Steps before halting:", steps, "\n")
cat("Total ones at halting:", sum(tape), "\n")
cat("Longest consecutive run:", longestRun, "\n")
