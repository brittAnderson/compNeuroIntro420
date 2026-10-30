#rules were  taken from Wikipedia's page for the busy beaver program
#my code initially led to an error as the tape reader essentially moves out of bounds from the tape size (initial tape values: 0-100)
#I then asked Claude Sonnet 5.5 how long the tape needs to be to run these rules
#it said around 12,289 would be the minimum amount of cells required if the starting position was at the right end. A safe range it suggested is 30000 with a starting pos at 15000
#this code works, but it takes a long  time, as each step taken does a lot of work. 
library(dplyr)

tape <- rep(0, 30000)
position <- 15000
state <- "A"

while (state != "HALT") {
  symbol <- tape[position]
  transition <- case_when(
    state == "A" & symbol == 0 ~ "1,R,B",
    state == "A" & symbol == 1 ~ "1,L,C",
    state == "B" & symbol == 0 ~ "1,R,C",
    state == "B" & symbol == 1 ~ "1,R,B",
    state == "C" & symbol == 0 ~ "1,R,D",
    state == "C" & symbol == 1 ~ "0,L,E",
    state == "D" & symbol == 0 ~ "1,L,A",
    state == "D" & symbol == 1 ~ "1,L,D",
    state == "E" & symbol == 0 ~ "1,R,HALT",
    state == "E" & symbol == 1 ~ "0,L,A"
  )
  parts <- strsplit(transition, ",")[[1]]
  tape[position] <- as.numeric(parts[1])
  if (parts[2] == "R") {
    position <- position + 1
  } else if (parts[2] == "L") 
    position <- position - 1
  state <- parts[3]
}

runs <- rle(tape)
largest_ones <- max(runs$lengths[runs$values == 1])
cat("Largest string of sequential ones:", largest_ones, "\n")



