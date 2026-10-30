#AI disclosure: I had no clue how to make my code work quicker, so I asked Claude Sonnet 5.5 to convert my code so its more efficient and fast
#this is the generated code that used my busybeaver_slow code 
write_sym <- matrix(c(1,1,  1,1,  1,0,  1,1,  1,0), nrow = 5, byrow = TRUE)
move      <- matrix(c(1,-1, 1,1,  1,-1, -1,-1, 1,-1), nrow = 5, byrow = TRUE)  
next_st   <- matrix(c(2,3,  3,2,  4,5,  1,4,  0,1), nrow = 5, byrow = TRUE)   

tape <- integer(30000)
position <- 15000
state <- 1   
steps <- 0

while (state != 0) {
  symbol <- tape[position]
  tape[position] <- write_sym[state, symbol + 1]
  position <- position + move[state, symbol + 1]
  state <- next_st[state, symbol + 1]
  steps <- steps + 1
}

runs <- rle(tape)
largest_ones <- max(runs$lengths[runs$values == 1])
cat("Largest string of sequential ones:", largest_ones, "\n")