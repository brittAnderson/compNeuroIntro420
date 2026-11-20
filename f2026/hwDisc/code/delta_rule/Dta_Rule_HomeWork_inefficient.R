library(pracma)

#Constants
Thet<-0.7
n<-0.1

#Class 1 inputs and vectors
C.1_inputs<-matrix(c(1.7,-0.6,1.9,-1.7,1.6,1.4,-0.4,1.6,1,1,1,1), nrow = 4, ncol = 3, 
          dimnames = list(c("1","2","3","4"), c("xi", "xii","bias")))
C.1_inputs
v1<-C.1_inputs[1,]
v2<-C.1_inputs[2,]
v3<-C.1_inputs[3,]
v4<-C.1_inputs[4,]


#weights 
w<-matrix(c(0.5,0.4,0.2), nrow = 1, ncol = 3,
          dimnames = list(c("1"), c("wi", "wii", "wiii")))
w
wi<-w[,1]
wii<-w[,2]
wiii<-w[,3]

#Process for input 1
xi<-v1["xi"]
xi
xii<-v1["xii"]
xii
bias<-v1["bias"]
bias

repeat {Out<-dot(v1,w) 
if (Out>=Thet) y<-1 else y<--1
deltwi<-xi*n*(1-y)
deltwii<-xii*n*(1-y)
delt_wiii<-bias*n*(1-y)
if (deltwi==0 & deltwii==0 & delt_wiii==0)print(w) & break 
else
{wi<-wi+deltwi
wii<-wii+deltwii
wiii<-wiii+delt_wiii
w<-matrix(c(wi,wii,wiii), nrow = 1, ncol = 3)
wi<-w[,1]
wii<-w[,2]
wiii<-w[,3]}

}

#Process for input 2
xi<-v2["xi"]
xi
xii<-v2["xii"]
xii
bias<-v2["bias"]
bias

repeat {Out<-dot(v2,w) 
if (Out>=Thet) y<-1 else y<--1
deltwi<-xi*n*(1-y)
deltwii<-xii*n*(1-y)
delt_wiii<-bias*n*(1-y)
if (deltwi==0 & deltwii==0 & delt_wiii==0)print(w) & break 
else
{wi<-wi+deltwi
wii<-wii+deltwii
wiii<-wiii+delt_wiii
w<-matrix(c(wi,wii,wiii), nrow = 1, ncol = 3)
wi<-w[,1]
wii<-w[,2]
wiii<-w[,3]}
}

#Process for input 3
xi<-v3["xi"]
xi
xii<-v3["xii"]
xii
bias<-v3["bias"]
bias

repeat {Out<-dot(v3,w) 
if (Out>=Thet) y<-1 else y<--1
deltwi<-xi*n*(1-y)
deltwii<-xii*n*(1-y)
delt_wiii<-bias*n*(1-y)
if (deltwi==0 & deltwii==0 & delt_wiii==0)print(w) & break 
else
{wi<-wi+deltwi
wii<-wii+deltwii
wiii<-wiii+delt_wiii
w<-matrix(c(wi,wii,wiii), nrow = 1, ncol = 3)
wi<-w[,1]
wii<-w[,2]
wiii<-w[,3]}

}

#Process for input 4
xi<-v4["xi"]
xi
xii<-v4["xii"]
xii
bias<-v4["bias"]
bias

repeat {Out<-dot(v4,w) 
if (Out>=Thet) y<-1 else y<--1
deltwi<-xi*n*(1-y)
deltwii<-xii*n*(1-y)
delt_wiii<-bias*n*(1-y)
if (deltwi==0 & deltwii==0 & delt_wiii==0)print(w) & break 
else
{wi<-wi+deltwi
wii<-wii+deltwii
wiii<-wiii+delt_wiii
w<-matrix(c(wi,wii,wiii), nrow = 1, ncol = 3)
wi<-w[,1]
wii<-w[,2]
wiii<-w[,3]}

}


#Class -1 inputs and vectors
C.ng1_inputs<-matrix(c(0.5,-0.8,-0.1,2,-0.2,1.4,-0.5,-1.3,1,1,1,1), nrow = 4, ncol = 3,
                     dimnames = list(c("1","2","3","4"), c("xi","xii", "bias")))
C.ng1_inputs
v_1<-C.ng1_inputs[1,]
v_2<-C.ng1_inputs[2,]
v_3<-C.ng1_inputs[3,]
v_4<-C.ng1_inputs[4,]

#Weights
w<-matrix(c(0.5,0.4,0.2), nrow = 1, ncol = 3,
          dimnames = list(c("1"), c("wi", "wii", "wiii")))
w
wi<-w[,1]
wii<-w[,2]
wiii<-w[,3]

#Process for input 1
xi<-v_1["xi"]
xi
xii<-v_1["xii"]
xii
bias<-v_1["bias"]
bias

repeat {Out<-dot(v_1,w) 
if (Out>=Thet) y<--1 else y<-1
deltwi<-xi*n*(-1-y)
deltwii<-xii*n*(-1-y)
delt_wiii<-bias*n*(-1-y)
if (deltwi==0 & deltwii==0 & delt_wiii==0)print(w) & break 
else
{wi<-wi+deltwi
wii<-wii+deltwii
wiii<-wiii+delt_wiii
w<-matrix(c(wi,wii,wiii), nrow = 1, ncol = 3)
wi<-w[,1]
wii<-w[,2]
wiii<-w[,3]}

}

#Process for input 2
xi<-v_2["xi"]
xi
xii<-v_2["xii"]
xii
bias<-v_2["bias"]
bias

repeat {Out<-dot(v_2,w) 
if (Out>=Thet) y<--1 else y<-1
deltwi<-xi*n*(-1-y)
deltwii<-xii*n*(-1-y)
delt_wiii<-bias*n*(-1-y)
if (deltwi==0 & deltwii==0 & delt_wiii==0)print(w) & break 
else
{wi<-wi+deltwi
wii<-wii+deltwii
wiii<-wiii+delt_wiii
w<-matrix(c(wi,wii,wiii), nrow = 1, ncol = 3)
wi<-w[,1]
wii<-w[,2]
wiii<-w[,3]}

}


#Process for input 3
xi<-v_3["xi"]
xi
xii<-v_3["xii"]
xii
bias<-v_3["bias"]
bias

repeat {Out<-dot(v_3,w) 
if (Out>=Thet) y<--1 else y<-1
deltwi<-xi*n*(-1-y)
deltwii<-xii*n*(-1-y)
delt_wiii<-bias*n*(-1-y)
if (deltwi==0 & deltwii==0 & delt_wiii==0)print(w) & break 
else
{wi<-wi+deltwi
wii<-wii+deltwii
wiii<-wiii+delt_wiii
w<-matrix(c(wi,wii,wiii), nrow = 1, ncol = 3)
wi<-w[,1]
wii<-w[,2]
wiii<-w[,3]}

}


#Process for input 4
xi<-v_4["xi"]
xi
xii<-v_4["xii"]
xii
bias<-v_4["bias"]
bias

repeat {Out<-dot(v_4,w) 
if (Out>=Thet) y<--1 else y<-1
deltwi<-xi*n*(-1-y)
deltwii<-xii*n*(-1-y)
delt_wiii<-bias*n*(-1-y)
if (deltwi==0 & deltwii==0 & delt_wiii==0)print(w) & break 
else
{wi<-wi+deltwi
wii<-wii+deltwii
wiii<-wiii+delt_wiii
w<-matrix(c(wi,wii,wiii), nrow = 1, ncol = 3)
wi<-w[,1]
wii<-w[,2]
wiii<-w[,3]}

}


 

