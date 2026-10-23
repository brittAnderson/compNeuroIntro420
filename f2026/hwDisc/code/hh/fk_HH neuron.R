#Normal Neuron

#Values
v<-0
an=0.01*(10-v)/(exp((10-v)/10)-1)
am=0.1*(25-v)/(exp((25-v)/10)-1)
ah=0.07*exp(-v/20)
bn=0.125*exp(-v/80)
bm=4*exp(-v/18)
bh=1/(exp((30-v)/10)+1)

m=am/(am+bm) 
n=an/(an+bn)
h=ah/(ah+bh)

ena<-115
gna<-120
ek<--12
gk<-36
el<-10.6
gl<-0.3
I<-20
dt<-0.01
tmax<-200



time<-c()
position<-c()
current<-c() 
#loop
for (t in seq(0, tmax, by = dt)) {
  an=0.01*(10-v)/(exp((10-v)/10)-1)
  am=0.1*(25-v)/(exp((25-v)/10)-1)
  ah=0.07*exp(-v/20)
  bn=0.125*exp(-v/80)
  bm=4*exp(-v/18)
  bh=1/(exp((30-v)/10)+1)
  dvdt <- I-(gna*m^3*h*(v-ena)+gk*n^4*(v-ek)+gl*(v-el))
  dmdt<-am*(1-m)-bm*m
  dndt<-an*(1-n)-bn*n
  dhdt<-ah*(1-h)-bh*h
  v <- v +dvdt*dt
  m<-m+dmdt*dt
  n<-n+dndt*dt
  h<-h+dhdt*dt
  time <- c(time, t)
  position <- c(position, v)
  if (t > 70 & t < 120) 
    I <- 20
  else 
    I <- 0
  current <- c(current, I)
  
}

#Graph 
plot(time, position,
     type = "l",
     xlab = "Time (s)",
     ylab = "Voltage (v)",
     main = "H&H Normal") 

lines(time,current, col="blue")


#Defected Neuron

#Values
v<-0
an=0.01*(10-v)/(exp((10-v)/10)-1)
am=0.1*(25-v)/(exp((25-v)/10)-1)
ah=0.07*exp(-v/20)
bn=0.125*exp(-v/80)
bm=4*exp(-v/18)
bh=1/(exp((30-v)/10)+1)

m=am/(am+bm) 
n=an/(an+bn)
h=ah/(ah+bh)

ena<-115
gna<-120
ek<--12
gk<-36
el<-10.6
gl<-0.3
I<-20
ya<-2
yb<-20
yt<-0.2
dt<-0.01
tmax<-200



time<-c()
position<-c()
current<-c()

#loop
for (t in seq(0, tmax, by = dt)) {
  an=0.01*(10-v)/(exp((10-v)/10)-1)
  am=0.1*(25-v)/(exp((25-v)/10)-1)
  ah=0.07*exp(-v/20)
  bn=0.125*exp(-v/80)
  bm=4*exp(-v/18)
  bh=1/(exp((30-v)/10)+1)
  dvdt <- I-(gna*m^3*h*(v-ena)+gk*n^4*(v-ek)+gl*(v-el))
  dndt<-yt*((ya*an)*(1-n)-(yb*bn)*n) 
  dmdt<-am*(1-m)-bm*m
  dhdt<-ah*(1-h)-bh*h
  v <- v +dvdt*dt
  
  m<-m+dmdt*dt
  n<-n+dndt*dt
  h<-h+dhdt*dt
  time <- c(time, t)
  position <- c(position, v)
  if (t > 70 & t < 120) 
    I <- 20
  else 
    I <- 0
  current <- c(current, I)
  
}

#Graph 
plot(time, position,
     type = "l",
     xlab = "Time (s)",
     ylab = "Voltage (v)",
     main = "H&H Defected") 

lines(time,current, col="blue")

