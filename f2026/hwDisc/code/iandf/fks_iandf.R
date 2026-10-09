#Values 
v<-0
I<-0
R<-3
dt<-0.02
tmax<-30

time<-c()
position<-c()
current<-c() 

#Intergrate Fire
for (t in seq(0, tmax, by = dt)) {
  dvdt <- -v + R*I
  v <- v +dvdt*dt
  
if(v>=10)v<-20
time <- c(time, t)
  position <- c(position, v)
  
if (t > 10 & t < 20) 
    I <- 4.5
  else 
    I <- 0
if(v==20) v<-0
current <- c(current, I)
}
#Graph Frictionless
plot(time, position,
     type = "l",
     xlab = "Time (s)",
     ylab = "Voltage (v)",
     main = "Intergrate and Fire")  
lines(time,current, col="blue")




