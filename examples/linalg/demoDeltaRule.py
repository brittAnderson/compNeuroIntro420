"""
=============================
Wasam Ashraf Syed
Assignment 7
PSYCH 420
Perceptron and Delta rule

Collaborators: 
=============================
"""

import matplotlib.pyplot as plt
import numpy as np
import math 
from random import seed
import random as r

# Used random number generator to create classes. 
# Seed allows me to get the same set.

def class_gen(num_of_classes = 40):
    ans_lst = []
    seed(190)
    while(True):
        if (len(ans_lst) == num_of_classes): break
        val_x1, val_x2 = round(r.uniform(-1,1), 1), round(r.uniform(-1,1), 1)
        ans_lst.append((val_x1, val_x2))
        ans_lst = list(set(ans_lst))
    return ans_lst
        
    
main = class_gen()

# Edited points in main to create separate classes.

old_main = [(-0.1, -0.3), (0.4, 0.0), (0.8, -0.9), (-0.5, 0.0), (0.3, -0.2),
            (-0.7, -0.0), (-0.1, 0.1), (0.3, -0.7), (0.1, -0.1), (-0.9, 0.7), 
            (-0.5, -0.6), (-0.0, 0.7), (-0.9, 1.0), (0.3, 0.2), (0.0, -0.3),
            (0.4, -0.2), (0.4, 0.3), (0.0, 0.2), (-0.6, 0.6), (0.4, 0.8), 
            (0.6, 0.5), (0.9, -0.4), (0.8, -0.6), (-0.9, 0.1), (0.4, 0.7), 
            (-0.1, 0.9), (-0.7, 0.8), (-0.3, -0.5), (0.1, -0.3), (0.5, -0.6), 
            (-0.9, 0.5), (0.8, -0.7), (0.2, -0.2), (-0.8, 0.2), (0.4, -0.4),
            (-0.4, 0.3), (0.1, 0.3), (0.7, 1.0), (-0.5, -0.3), (-0.9, 0.9)]

minus_ones = [(-0.9, 0.7), (-0.9, 1.0), (-0.9, 0.1), (-0.9, 0.5), (-0.9, 0.9),
              (-0.8, 0.2), (-0.7, 0.0), (-0.7, 0.8), (-0.6, 0.6), (-0.5, 0.0),
              (-0.5, -0.3), (-0.4, 0.3), (-0.1, 0.1), (-0.1, 0.9), (-0.0, 0.7),
              (0.0, 0.2), (0.1, 0.3), (0.4, 0.8), (0.4, 0.7), (0.7, 1.0)]

plus_ones = [elem for elem in old_main if elem not in minus_ones]

# Created classes with outputs

minus_ones_w_ans = [[elem, 0] for elem in minus_ones]
plus_ones_w_ans = [[elem, 1] for elem in plus_ones]

# Declared globals for functions

final_inps = minus_ones_w_ans + plus_ones_w_ans
weights, threshold, eta, iterations = [-0.4,0.3], 0, 0.1, 1000

# Helper functions

def dot_func(arr_1, arr_2):
    ans = np.dot(arr_1,arr_2)
    return ans

def output_func(i, thresh):
    ans = 0
    if (i >= thresh): ans = 1
    else: ans = 0
    return ans

# Function that updates weights

def delta_rule_perc(wts = weights, inps = final_inps, 
                    thr = threshold, e = eta, it = iterations):
    I, y_hat, y, delta_wi, ind_1, ind_2 = 0, 0, 0, 0, 0, 0
    while (True):
        if (ind_1 == iterations): break
        elif (ind_2 == len(inps)): ind_2 = 0
        else:
            I = dot_func(wts,inps[ind_2][0])
            y = inps[ind_2][1]
            y_hat = output_func(I, thr)
            delta_wi = [x * (e * (y-y_hat)) for x in inps[ind_2][0]]
            wts = np.add(wts, delta_wi)
            ind_2 += 1
            ind_1 += 1
    return wts
        
delta_rule_perc_results = delta_rule_perc() 

# Delta rule classifier that returns the number of correct results 

def delta_rule_classifier(wts_1 = delta_rule_perc_results, 
                          inps_1 = final_inps, thr_1 = threshold,
                          e_1 = eta):
    ans, I_1, y_1, y_hat_1, delta_wi_1, cc, ic = [], 0, 0, 0, 0, 0, 0
    for elem in inps_1:
        inner_list = []
        I_1 = dot_func(wts_1, elem[0])
        y_1 = elem[1]
        y_hat_1 = output_func(I_1, thr_1)
        inner_list.append(elem[0])
        if (y_1-y_hat_1 == 0):
            inner_list.append("Correct")
            cc+=1
        else:
            inner_list.append("Incorrect")
            ic+=1
        ans.append(inner_list)
    return (ans, cc, ic)
            

classifier_output = delta_rule_classifier()

# Print statements to capture the number of inputs that 
# were classified correctly.

#print("Correct: {}".format(classifier_output[1]))
#print("Incorrect: {}".format(classifier_output[2]))

# Commented out to allow plotting

"""
x = [val / 10.0 for val in range(-10,10,1)]   
y = [(1.1*v) + 0.11 for v in x]
plt.scatter(*zip(*minus_ones))
plt.scatter(*zip(*plus_ones))
plt.plot(x,y, color = "green")
plt.show()
"""
