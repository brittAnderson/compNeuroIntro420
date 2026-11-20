#Script that uses the delta rule to train a neuron to classify points above or
#below y = x. Neuron starts with random weights and adjusts based on errors 
#made across 100 epochs. Graph learning at different epoch stages. 

#Training points 
x <- c(2, 2, 4, 4)
y <- c(3, 1, 5, 3)
label <- ifelse(y > x, 1, -1)
#Learning rate and random weight selector
eta <- 0.01
w_x <- runif(1, -0.5, 0.5)
w_y <- runif(1, -0.5, 0.5)

#Visualization of how the decision plane moves as the unit learns
plot(x, y, xlim = c(0, 6), ylim = c(0, 6), asp = 1,
     pch = 19, col = ifelse(label == 1, "blue", "firebrick4"),
     main = "Decision boundary during training")
abline(a = 0, b = 1, lty = 2)
snapshot_epochs <- c(1, 10, 25, 50)
line_colors <- c("red", "orange", "gold2", "green2")

#Repeat x100 epochs 
for(epoch in 1:50){
  for(i in seq_along(x)){
    observed <- (w_x * x[i]) + (w_y * y[i])
    #Prediction error
    desired <- label[i]
    error <- desired - observed
    #Delta rule
    delta_w_x <- x[i] * eta * error
    delta_w_y <- y[i] * eta * error
    #Add each change to its current weight
    w_y <- w_y + delta_w_y
    w_x <- w_x + delta_w_x
  }
  #Updates graph
  if (epoch %in% snapshot_epochs) {
    line_color <- line_colors[match(epoch, snapshot_epochs)]
    if (w_y == 0) {
      abline(v = 0, col = line_color, lwd = 2)
    } else {
      abline(a = 0, b = -w_x / w_y, col = line_color, lwd = 2)
    }
  }
}

legend("topleft",
       legend = c("Target: y = x", paste("Epoch", snapshot_epochs)),
       col = c("black", line_colors),
       lty = c(2, 1, 1, 1, 1),
       lwd = c(1, 2, 2, 2, 2),
       bty = "n")

#Use final weights to score all 4 points
observed <- w_x * x + w_y * y
#Positive scores are 1, all else -1
predicted <- ifelse(observed > 0, 1, -1)

#Show predictions and check whether each matches its correct label
predicted
predicted == label

#4 new points that test trained neuron
x_test <- c(1, 3, 3, 5)
y_test <- c(4, 1, 5, 2)
label_test <- ifelse(y_test > x_test, 1, -1)
observed_test <- w_x * x_test + w_y * y_test
predicted_test <- ifelse(observed_test > 0, 1, -1)

#Check which test predictions match correct labels
correct_test <- predicted_test == label_test
correct_test
#Test accuracy %
test_accuracy <- mean(correct_test) * 100
test_accuracy

