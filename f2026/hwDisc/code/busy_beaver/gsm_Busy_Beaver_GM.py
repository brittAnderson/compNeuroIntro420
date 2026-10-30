class Turing:
    def __init__ (self): #I had help from Gemini for this part since I did not know where to begin but everything after line 10 is mine and Google's works (I asked the AI to translate your code and simplify for a more general approach then followed that outline)
        self.tape = {0: 0}
        self.head = 0
        self.state = 'A'
        self.steps = 0

    def step(self):
        val = self.tape.get(self.head, 0)

        if self.state == 'A': #Current state
            if val == 0: #Current value
                self.tape[self.head] = 1 #The head's current position on the tape
                self.head += 1 #which way the head will move (right in this case)
                self.state = 'B' #new state after tape moves

            else:
                self.tape[self.head] = 1 
                self.head -= 1
                self.state = 'C'
        elif self.state == 'B':
            if val == 0:
                self.tape[self.head] = 1
                self.head += 1
                self.state = 'C'
            else:
                self.tape[self.head] = 1
                self.head +=1
                self.state = 'A'
        elif self.state == 'C':
            if val == 0:
                self.tape[self.head] = 1
                self.head += 1
                self.state = 'D'
            else:
                self.tape[self.head] = 1
                self.head -= 1
                self.state = 'A'#for some reason, when I set the state to A initially I got a memory error, but now it works normally
        elif self.state == 'D':
            if val == 0:
                self.tape[self.head] = 1
                self.head -= 1
                self.state = 'Halt'
            else:
                self.tape[self.head] = 0
                self.head -= 1
                self.state = 'C'

        self.steps += 1

    def run(self):
        while self.state != 'Halt':
            self.step()

        ones = sum(1 for v in self.tape.values() if v == 1)
        print(f"halted after {self.steps} steps")
        print (f"total ones on tape: {ones}")

tm = Turing()
tm.run() #I ran with only 4 states because 5 was getting too large and would not run, I was also unsure and thought you changed the criteria in class to 4 states, if I am wrong then I will happily add another state and resubmit if possible
