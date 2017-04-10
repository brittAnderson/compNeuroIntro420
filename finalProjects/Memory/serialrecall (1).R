# serialrecall()
# -> Number of Items
# -> Inter-Item Presentation Interval

# Parameter Values
c <- 10
threshold <- 0.6
slope <- 8

serialrecall <- function(n, ipi){
  
  #Temporal Distances Of Itmes At Start Of Recall
  startdtime <- c()
  for (i in c((n - 1):0)){
    startdtime <- c(startdtime, i * ipi)
  }
  
  # Initialize Probability of Discrimination Matrix
  prob <- matrix(vector(mode="numeric", length=(n*n)),nrow=n, ncol=n)
  
  
  for (index in c(1: n)){
    ss <- 0 # Initialize Summed Similarity
    rinc <- 1 # Amount Of Time Taken To Recall Each Item
    d <- startdtime + (rinc * index) # Calculates Temporal Distance Of All Itmes At The Time Each Item Is Recalled
    
    # Apply Log function to Temporal Distances
    for (i in c(1:n)){
      d[i] <- log(d[i]) 
    }
    
    
    for (index2 in c(1:n)){
      ss <- ss + exp((-c) * abs(d[index] - d[index2]))  # Calculates Similarity and Sums with Total
    }
    
    for (index3 in c(1:n)){
      prob[index3, index] <- (exp((-c) * abs(d[index] - d[index3]))) / ss # Calculates D Value For One Time Given Another Item As A Cue
    }
  }
  
  # Applying Threshold
  for (i in c(1:n)){
    for (j in c(1:n)){
      prob[i,j] <- 1 / (1 + exp((-slope) * (prob[i,j] - threshold)))
    }
  }
  
  # Normalize Columns If Column Sums To Greater Than 1
  sumprob <- colSums(prob)
  for (index in c(1:n)){
    if (sumprob[index] > 1){
      prob[, index] <- prob[, index] / sumprob[index]
    }
  }
  sumprob <- colSums(prob)
  s <- min(sumprob)
  m <- match(min(sumprob),sumprob)
  k <- 0.5
  o <- 0
  
  if (o == 1){
   #Amnesiacs
    for (index in c(1:m)){
      sumprob[index] <- s
    }
  }
  
  if (o == 2){
  # Compromised LTM
   for (index in c(1:m)){
      sumprob[index] <- ((sumprob[index])-s)*(k) + s
    }
  }
  
  if (o == 3){
  # Alzheimer's
    for (index in c(m:n)){
      sumprob[index] <- s
    }
  }
  
  if (o == 4){
  # Compromised STM
    for (index in c(m:n)){
       sumprob[index] <- ((sumprob[index])-s)*(k) + s
    }
  }
  
  if (o == 5){
  #Alz/Amn in Reality
    for (index in c(1:m)){
       sumprob[index] <- (sumprob[index])*(k)
    }
    for (index in c((m+1):n)){
       sumprob[index] <- s/2 + (max(sumprob[(n/2):n])-s*k)/(n-m)*(index-m)
    }
  }
  
  # Plots Probability of Discrimination Matrix
  plot(sumprob, type='l')
}
serialrecall(20,0.5)

