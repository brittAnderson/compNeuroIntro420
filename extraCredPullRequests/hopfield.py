import random

def dotProduct (a, b):
	out = 0
	if len(a) != len(b):
		out = float('NaN')
	else:
		for i in range(len(a)):
			out += a[i]*b[i]

	return out

w = [[0, -3, 3, -3],[-3,0,-3,3],[3-3,0,-3],[-3,3,-3,0]]
new = [1,0,0,1]
old = [0,0,0,0]
######### incremental ##########
i = -1

print new
while new != old:
	old = list(new)
	i = (i + 1) % 3
	print i
	new[i] = int(dotProduct(new, w[i]) >= 0)
	print new

######### random ##########
# print new
# streak = 0
# while streak < 3:
# 	old = list(new)
# 	i = random.randint(0,3)
# 	print i
# 	new[i] = int(dotProduct(new, w[i]) >= 0)
# 	print new
# 	if new != old :
# 		streak = 0
# 	else:
# 		streak += 1