#Delta Rule
set.seed(123)

generate_data <- function(n, margin = 2) {
  data <- data.frame()
  
  while (nrow(data) < n) {
    x1 <- runif(1, -10, 10)
    x2 <- runif(1, -10, 10)
    
    if (x2 > x1 + margin) {
      data <- rbind(data, data.frame(x1 = x1, x2 = x2, label = 1))
    } else if (x2 < x1 - margin) {
      data <- rbind(data, data.frame(x1 = x1, x2 = x2, label = -1))
    }
  }
  
  data
}

predict_neuron <- function(x1, x2, w, b) {
  ifelse(w[1] * x1 + w[2] * x2 + b >= 0, 1, -1)
}

train_delta <- function(data, eta = 0.05, epochs = 50) {
  w <- runif(2, -1, 1)
  b <- runif(1, -1, 1)
  
  for (epoch in 1:epochs) {
    for (i in 1:nrow(data)) {
      
      x <- c(data$x1[i], data$x2[i])
      desired <- data$label[i]
      
      observed <- predict_neuron(x[1], x[2], w, b)
      
      error <- desired - observed
      
      w <- w + eta * x * error
      b <- b + eta * error
    }
  }
  
  list(weights = w, bias = b)
}

evaluate <- function(data, w, b) {
  predictions <- mapply(
    predict_neuron,
    data$x1,
    data$x2,
    MoreArgs = list(w = w, b = b)
  )
  
  mean(predictions == data$label) * 100
}

training_data <- generate_data(n = 100, margin = 2)

model <- train_delta(
  data = training_data,
  eta = 0.05,
  epochs = 50
)

test_data <- generate_data(n = 50, margin = 2)

accuracy <- evaluate(
  data = test_data,
  w = model$weights,
  b = model$bias
)

cat("Weights:", model$weights, "\n")
cat("Bias:", model$bias, "\n")
cat("Test Accuracy:", round(accuracy, 2), "%\n")
