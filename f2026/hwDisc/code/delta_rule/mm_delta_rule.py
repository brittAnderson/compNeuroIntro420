def delta_rule(data, w1, w2, b, eta):
    mistakes = 1
    epoch = 0 
    max_epochs = 1000 # safety guard to avoid an infinite loop... 

    while mistakes > 0 and epoch < max_epochs: 

        mistakes = 0
        correct = 0
        epoch += 1

        for point, target in data:
            
            output = w1 * point[0] + w2 * point[1] + b
            error = target - output

            delta_w1 = eta * error * point[0]
            delta_w2 = eta * error * point[1]

            prediction = 1 if output >= 0 else -1

            if prediction == target:
                correct += 1
            else:
                mistakes += 1

            ## Updates
            w1 += delta_w1
            w2 += delta_w2
            b = b + eta * error 

            print(f"Updated weights: {w1}, {w2}")

        accuracy = correct / len(data) * 100

        print(f"Epoch {epoch}: {mistakes} mistakes, {accuracy:.2f}% accuracy") 

# ## Training Variables 
# ## Population 1 points: x + y >= 10
# ## Population -1 points: x + y < 10
# training_data = [((7, 5), 1),
#                  ((8, 6), 1),
#                  ((9, 7), 1),
#                  ((1, 1), -1),
#                  ((1, 2), -1),
#                  ((1, 3), -1)]

# training_w1 = 0.5
# training_w2 = -0.1

# training_b = 0.5 
# training_eta = 0.001 

# delta_rule(training_data, training_w1, training_w2, training_b, training_eta)


## Test Variables 
## Population 1 points: x + y >= 10
## Population -1 points: x + y < 10
test_data = [((6, 6), 1),
             ((6, 7), 1),
             ((4, 7), 1),
             ((8, 3), 1),
             ((9, 6), 1),
             ((2, 2), -1),
             ((2, 6), -1),
             ((3, 3), -1),
             ((4, 5), -1), 
             ((4, 4), -1)]

test_w1 = 0.5
test_w2 = -0.1

test_b = 0.5 
test_eta = 0.001 

delta_rule(test_data, test_w1, test_w2, test_b, test_eta)