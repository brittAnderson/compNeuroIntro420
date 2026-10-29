
tape <- rep(0, 100)
position <- 50
state <- "A"
max_run <- 0

find_longest_run <- function(tape) {
  
  longest <- 0
  current <- 0
  
  for(value in tape) {
    
    if(value == 1) {
      
      current <- current + 1
      
      if(current > longest) {
        longest <- current
      }
      
    } else {
      
      current <- 0
      
    }
  }
  
  return(longest)
}


while(state != "H") {
  
  current <- tape[position]
  
  if(state == "A" && current == 0) {
    
    tape[position] <- 1
    position <- position + 1
    state <- "B"
    
  } else if(state == "B" && current == 0) {
    
    tape[position] <- 1
    position <- position + 1
    state <- "C"
    
  } else if(state == "C" && current == 0) {
    
    tape[position] <- 1
    position <- position + 1
    state <- "D"
    
  } else if(state == "D" && current == 0) {
    
    tape[position] <- 1
    position <- position + 1
    state <- "E"
    
  } else if(state == "E" && current == 0) {
    
    tape[position] <- 1
    position <- position - 1
    state <- "A"
    
  } else if(state == "A" && current == 1) {
    
    tape[position] <- 1
    position <- position - 1
    state <- "B"
    
  } else if(state == "B" && current == 1) {
    
    tape[position] <- 1
    position <- position - 1
    state <- "C"
    
  } else if(state == "C" && current == 1) {
    
    tape[position] <- 1
    position <- position - 1
    state <- "D"
    
  } else if(state == "D" && current == 1) {
    
    tape[position] <- 1
    position <- position - 1
    state <- "E"
    
  } else if(state == "E" && current == 1) {
    
    tape[position] <- 1
    position <- position + 1
    state <- "H"
    
  }
  
  current_run <- find_longest_run(tape)
  
  if(current_run > max_run) {
    max_run <- current_run
  }
}

print(tape)
cat("Longest run of 1s:", max_run, "\n")

