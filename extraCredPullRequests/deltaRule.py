#PSYCH420 - Delta Rule

#Imports:
import random 
from operator import add 
import numpy as np
import matplotlib.pyplot as p

#Constants and storage:
m = random.randint(-10,10)
b = random.randint(-10,10)
xVals = random.sample(range(-20,20),20)
yVals=[]
newyVals=[]
updtY=[]
neurClass=[] #The +1/-1 activations.
trainResults=[] #Vector for storing testing results.
testClass=[]
weights=[0.1,0.4,0.2] 
t=0 #Threshold set at 0. 
bias=1
eta=0.1

#Delta rule for adjusting weights:
def dR(iVal,des,obs):
	return iVal*eta*(des-obs)

#Slope-intercept equation for finding y-values:
def slopeIntercept(slope,yInt,x,ylist):
	for n in x:
		y=slope*n+yInt
		ylist.append(y)
	return ylist

#Function for shifting y-values to create training/validation 
#	samples. Half are shifted above the line, and the other half
#	below the line:
def shift(alist):
	for n in range(len(alist)):
		if n%2==0 or n==0:
			newVal=alist[n]+random.randint(0,50)
			neurClass.append(1) 
		else:
			newVal=alist[n]+random.randint(-50,0)
			neurClass.append(-1)
		updtY.append(newVal)
	return updtY

#Calculating y-values for a random line, shifting points,
#	creating training/validation sets:
yVals=slopeIntercept(m,b,xVals,yVals)
updtY=shift(yVals)
trainX=xVals[:10]
trainY=updtY[:10]
checkAct=neurClass[10:]
validateX=xVals[10:]
validateY=updtY[10:]

#Computes dot-product of x,y,bias set against the weight vector, checks
#	for activation (above or below threshold), and classifies as +1 or -1
#	to create training set. Calculates new weight vector based on activation: 
def activation(x,y,w,resultList):
	for i in range(len(x)):
		dataSet=[x[i],y[i],bias]
		act=np.dot(dataSet,w)
		if act>=t:
			val=1
		elif act<t:
			val=-1
		resultList.append(val)
		w=np.add(w,[dR(x[i],checkAct[i],val),dR(y[i],checkAct[i],val),dR(bias,checkAct[i],val)])
	return resultList,w

#First run through for training and new weight vector:
epoch=activation(trainX,trainY,weights,trainResults)

#Iterations of training/adjusting weight vector to fit training data:
numRuns=10
for n in range(numRuns):
	trainResults=[]
	epoch=activation(trainX,trainY,epoch[1],trainResults)
trainWeight=epoch[1]
 
#Checking to see if the weight vector can correctly classify the 
#	validation set:
checkValidSet=activation(validateX,validateY,trainWeight,testClass)
testClass=checkValidSet[0]

#Counting the number of correctly classified validation points with respect to the 
#	new weight vector:
count=[]
for n in range(len(testClass)):
	if testClass[n]==checkAct[n]:
		val=1
	else:
		val=0 
	count.append(val)
countSum=sum(count)

###########Output Specifications###########

listTrainWeight=np.array(trainWeight).tolist() 
print "The trained weight vector is %r." %listTrainWeight
print "Number of correctly classified validation points: %d" %countSum

#Plotting specification for plotting the weight vector:
newM=-1*(trainWeight[0]/trainWeight[1]) 
newX=random.sample(range(-20,20),10)
newY=slopeIntercept(newM,b,newX,newyVals)

#Visual test:
p.plot(xVals,yVals,label="Decision Plane")
p.plot(newX,newY,label="Weight Vector",color="black")
p.scatter(xVals,updtY,label="Validation Points",color="blue")
p.scatter(trainX,trainY,label="Training Points",color="red") 
p.legend(loc="upper right",fontsize="small")
p.title("Delta Rule")
p.show()