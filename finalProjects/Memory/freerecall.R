# freerecall()
# -> Number of Items
# -> Retention Interval
# -> Inter-Item Presentation Interval
# -> Rehearsal (Boolean): Introduce Rehearsal to Model

# Parameter Values
c <- 10
threshold <- 0.6
slope <- 8

freerecall <- function(n, retint, ipi, rehearse,o){
  d <- c()
  # Temporal Differences at Time of Interval
  for (i in c(((n - 1) * (1 / ipi)):0)){
    d <- c(d, i * ipi)
  }
  
  # Adding Retention Interval to Temporal Differences
  d <- d + retint
  
  # Initialize Probability of Discrimination Matrix
  prob <- matrix(vector(mode="numeric", length=(n*n)),nrow=n, ncol=n)
  
  # Performs Rehearsal Calculations (If Specified)
  if (rehearse == T){
    # Initialize Rehearsal Difference Vector
    dRehearse <- vector(mode="numeric", length=n)
    
    # Initializes Rehearsal Parameters
    rehearsalLag <- 0.1
    rehearsalTime <- 0.1
    
    #Rehearsal Equation
    for (i in c(2:n)){
      dRehearse[i-1] <- ((d[i-1] - d[i]) - (rehearsalLag + rehearsalTime * (i-1)))
      if (dRehearse[i-1] < 0){
        dRehearse[i-1] <- 0
      }
    }
    dRehearse[n] = 0
  }
  
  # Apply Log function to Temporal Distances
  for(index in c(1:n)){
    d[index] <- log(d[index])
  }
  
  # Add Rehearsal Value to Temporal Distances (If Specified)
  if (rehearse == T){
    d <- d + dRehearse
  }
  
  # 
  for(index in c(1:n)){
    ss <- 0 # Initialize Summed Similarity 
    for (index2 in c(1:n)){
      ss <- ss + exp((-c) * abs(d[index] - d[index2])) # Calculates Similarity and Sums with Total
    }
    
    for(index3 in c(1:n)){
      prob[index3, index] <- (exp((-c) * abs(d[index] - d[index3]))) / ss # Calculates D Value For One Time Given Another Item As A Cue
    }
  }
  
  # Applying Threshold
  for (i in c(1:n)){
    for (j in c(1:n)){
      prob[i,j] <- 1 / (1 + exp((-slope) * (prob[i,j] - threshold)))
    }
  }
  
  # Sum matrix which reflects that an item may be recalled in response to a cue for a different item
  # Model of Serial Position Curve 
  serialpos <- colSums(t(prob))
  
  # Limits Recall Probability To 100%
  for (index in c(1:n)){
    serialpos[index] <- min(1, serialpos[index])
  }
  
  s <- min(serialpos)
  m <- match(min(serialpos),serialpos)
  k <- 0.5
  
  
  if (o == 1){
  # Amnesiacs
    for (index in c(1:m)){
      serialpos[index] <- s
    }
  }
  
  if (o == 2){
  # Compromised LTM
   for (index in c(1:m)){
      serialpos[index] <- ((serialpos[index])-s)*(k) + s
    }
  }
  
  if (o == 3){
  # Alzheimer's
    for (index in c(m:n)){
      serialpos[index] <- s
    }
  }
  
  if (o == 4){
  # Compromised STM
    for (index in c(m:n)){
    serialpos[index] <- ((serialpos[index])-s)*(k) + s
    }
  }
  
  if (o == 5){
  #Alz/Amn in Reality
  for (index in c(1:m)){
    serialpos[index] <- (serialpos[index])*(k)
  }
  for (index in c((m+1):n)){
    serialpos[index] <- s*k + (max(serialpos[(n/2):n])-s*k)/(n-m)*(index-m)
  }
  }
  
  # Plots Serial Position Curve
  plot(serialpos, type='l')
}
freerecall(20,1,1,1,0)
