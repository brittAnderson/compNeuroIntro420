#Spring oscillator
#Let t be time, s be position wrt equilibrium, v be velocity, a be acceleration and p be a constant where a = -ps(t)
t <- 0
t_column <- c()
s <- 10
s_column <- c()
v <- 0
v_column <- c()
p <- 1.5
c <- 0.5
a <- -1*p*s
a_column <- c()
#loop
while (t < 20){

  t_column<-c(t_column,t)
  t <- t+0.2
  
  s_column<-c(s_column,s)
  s <- s+(v*0.2)

  a_column<-c(a_column,a)
  a <- -1*p*s-c*v

  v_column<-c(v_column,v)
  v <- v+(a*0.2)
  
}
print(t_column)
print(s_column)
print(a_column)
print(v_column)
plot(t_column,s_column,type = "o")
plot(t_column,a_column,type = "o")
plot(t_column,v_column,type = "o")

