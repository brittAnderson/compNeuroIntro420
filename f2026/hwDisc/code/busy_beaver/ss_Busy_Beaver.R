#Busy Beaver
tape <- c(rep(0, 50))
pos <- 0
state <- "A"

while(state != "H"){
  
  key <- as.character(pos)
  
  if(key %in% names(tape)){
    symbol <- as.numeric(tape[key])
  } else {
    symbol <- 0
  }
  
  if(state == "A"){
    tape[key] <- 1
    pos <- pos + 1
    state <- "B"
    
  } else if(state == "B"){
    tape[key] <- 1
    pos <- pos + 1
    state <- "C"
    
  } else if(state == "C"){
    tape[key] <- 1
    pos <- pos + 1
    state <- "D"
    
  } else if(state == "D"){
    tape[key] <- 1
    pos <- pos + 1
    state <- "E"
    
  } else if(state == "E"){
    tape[key] <- 1
    state <- "H"
  }
}

ones <- sort(as.numeric(names(tape)))

longest <- length(ones)

cat("Largest run of 1s =", longest, "\n")
cat(paste(tape, collapse = " "), "\n")

