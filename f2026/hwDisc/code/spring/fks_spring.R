#Values 
m<-2
k<-5
c<-1.5
x<-1
v<-0
dt<-0.01 # edit britt
tmax<-10

time<-c()
position<-c()

#Frictionless
for (t in seq(0, tmax, by = dt)) {
F <- -k * x
a <- F / m
v <- v + a * dt
x <- x + v * dt
time <- c(time, t)
position <- c(position, x)
}

#Graph Frictionless
plot(time, position,
type = "l",
xlab = "Time (s)",
ylab = "Position (m)",
main = "Frictionless Oscillator")




#Damped
m<-2
k<-5
c<-1
x<-1
v<-0
dt<-0.01 #edit Britt
tmax<-10

time2<-c()
position2<-c()
for (t in seq(0, tmax, by = dt)) {
F <- -k * x-c*v
a <- F / m
v <- v + a * dt
x <- x + v * dt
time2 <- c(time2, t)
position2 <- c(position2, x)
}


#Graph Damped
plot(time2, position2,
type = "l",
xlab = "Time (s)",
ylab = "Position (m)",
main = "Damped Oscillator")
