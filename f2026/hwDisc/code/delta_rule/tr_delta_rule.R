set.seed(1)
x <- runif(80, 0, 10)
y <- runif(80, 0, 10)
label <- ifelse(x + y > 10, 1, -1)

#training and test data
train_x <- x[1:60]
train_y <- y[1:60]
train_label <- label[1:60]

test_x <- x[61:80]
test_y <- y[61:80]
test_label <- label[61:80]

#random weights

weight_x <- runif(1, -1, 1)
weight_y <- runif(1, -1, 1)

bias <- runif(1, -1, 1)
#eta
learning_rate <- 0.01

#delta rule

for(epoch in 1:100) {
  
  for(i in 1:length(train_x)) {
      activity <- weight_x * train_x[i] +
      weight_y * train_y[i] +
      bias
    if(activity > 0) {
      observed <- 1
    } else {
      observed <- -1
    }
    error <- train_label[i] - observed
    weight_x <- weight_x +
      learning_rate * error * train_x[i]
    weight_y <- weight_y +
      learning_rate * error * train_y[i]
    bias <- bias +
      learning_rate * error
  }
}

#test new points

correct <- 0
for(i in 1:length(test_x)) {
  activity <- weight_x * test_x[i] +
    weight_y * test_y[i] +
    bias
  if(activity > 0) {
    observed <- 1
  } else {
    observed <- -1
  }
  if(observed == test_label[i]) {
    correct <- correct + 1
  }
}

#accuracy
accuracy <- correct / length(test_x)
cat("Accuracy =", accuracy * 100, "%\n")
plot(
  x,
  y,
  col = ifelse(label == 1, "red", "green"),
  pch = 19
)


