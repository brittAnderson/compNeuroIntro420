#PSYCH420 - Iterating a solution with a DE

#Equations for y=x^2+x^3 and derivative:
#These functions take in your guess, and the value (val) that you 
#	want to find the solution for:
def error(guess,val):
	return (guess**2+guess**3)-val

def derivEqu(guess):
	return 2*guess+3*(guess**2)

#Newton's method equation to approximate a new guess:
def approx(guess,val):
	return guess - (error(guess,val)/float(derivEqu(guess)))

#Finding the approximate solution for the polynomial y=x^2+x^3. 

def solvePoly(guess,val):
	threshold=0.0001
	if abs(error(guess,val))<threshold:
		return round(approx(guess,val),3)
	else:
		updtGuess=approx(guess,val)
		return solvePoly(updtGuess,val)
