library(pracma)

#Constants
Thet<-0.7
n<-0.01

#Class 1 inputs and vectors
C.1_inputs<-matrix(c(1.7,0.7,1.9,-0.1,1.6,1.4,-0.4,1.6,1,1,1,1), nrow = 4, ncol = 3, 
                   dimnames = list(c("1","2","3","4"), c("xi", "xii","bias")))
C.1_inputs

#Class -1 inputs and vectors
C.ng1_inputs<-matrix(c(0.5,-0.8,-0.1,2,-0.2,1.4,-0.5,-1.3,1,1,1,1), nrow = 4, ncol = 3,
                     dimnames = list(c("1","2","3","4"), c("xi","xii", "bias")))
C.ng1_inputs

#weights 
set.seed(123321)
w<-matrix(runif(3,-1,1), nrow=1, ncol=3,
          dimnames=list(c("1"),
                        c("wi","wii","wiii")))
wi <- w[,1]
wii <- w[,2]
wiii <- w[,3]

weight_history <- matrix(
  c(wi, wii, wiii),
  nrow = 1
)

#Process and Loop

All_inputs <- rbind(C.1_inputs, C.ng1_inputs)

Targets <- c(1,1,1,1,-1,-1,-1,-1)

for(i in 1:8){
  
  v <- All_inputs[i,]
  
  xi <- v["xi"]
  xii <- v["xii"]
  bias <- v["bias"]
  
  target <- Targets[i]
  
  repeat {
    Out <- dot(v,w)
    
    if(Out >= Thet)
      y <- 1
    else
      y <- -1
    
    deltwi <- xi*n*(target-y)
    deltwii <- xii*n*(target-y)
    delt_wiii <- bias*n*(target-y)
    
    if(target == y){
      
      print(w)
      break
    }
    
    wi <- wi + deltwi
    wii <- wii + deltwii
    wiii <- wiii + delt_wiii
    
    w <- matrix(c(wi,wii,wiii),
                nrow=1,
                ncol=3)
    weight_history <- rbind(
      weight_history,
      c(wi,wii,wiii))
    
    wi <- w[,1]
    wii <- w[,2]
    wiii <- w[,3]
  }
}

cat("Final trained weights:\n")

print(w)

#Illustration
library(shiny)

ui <- fluidPage(
  
  titlePanel("Perceptron Learning Visualization"),
  
  sliderInput(
    "step",
    "Learning Step",
    min = 1,
    max = nrow(weight_history),
    value = 1,
    step = 1
  ),
  
  plotOutput("boundaryPlot")
)

server <- function(input, output) {
  
  output$boundaryPlot <- renderPlot({
    
    plot(
      C.1_inputs[,1],
      C.1_inputs[,2],
      col = "blue",
      pch = 19,
      xlim = c(-2.5,2.5),
      ylim = c(-2,2),
      xlab = "x1",
      ylab = "x2",
      main = paste("Learning Step", input$step)
    )
    
    points(
      C.ng1_inputs[,1],
      C.ng1_inputs[,2],
      col = "red",
      pch = 17
    )
    
    w1 <- weight_history[input$step,1]
    w2 <- weight_history[input$step,2]
    b  <- weight_history[input$step,3]
    
    x <- seq(-2.5,2.5,length.out=200)
    
    y <- (Thet - w1*x - b)/w2
    
    lines(x,y,col="darkgreen",lwd=3)
    
    legend(
      "topright",
      legend=c("Class +1","Class -1","Decision Boundary"),
      col=c("blue","red","darkgreen"),
      pch=c(19,17,NA),
      lty=c(NA,NA,1)
    )
  })
}

options(browser = "firefox")
app <-shinyApp(ui, server)
runApp(app)

#New Test
Test_inputs <- matrix(c(1.5,0.7,1,1.8,-1.2,1,-0.3,1.8,1,0.2,-1.0,1),nrow=4,ncol=3,byrow=TRUE)
Test_inputs

Correct <- c(1,1,-1,-1)
Predicted <- numeric(4)

for(i in 1:4){
  
  Out <- dot(Test_inputs[i,],w)
  
  if(Out >= Thet)
    Predicted[i] <- 1
  else
    Predicted[i] <- -1
}
Predicted

#Accuracy
Correct <- c(1,1,-1,-1)

accuracy <- sum(Predicted == Correct)/length(Correct)

cat("Accuracy = ",accuracy*100,"%\n")

#Results
Results <- data.frame(
  Actual = Correct,
  Predicted = Predicted
)

Results







