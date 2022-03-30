library(quantmod)
library(gramEvol)

stock <- 'NFLX'
fromDate <- '2017-01-01'
toDate <- '2017-03-01'
windowSize <- 20
testTrainSplit <- .20

# Read Data
getSymbols(stock, src="yahoo", from=fromDate, to=toDate, freq="daily")
dataAdjusted <- NFLX$NFLX.Adjusted
dataOpen <- NFLX$NFLX.Open
dataClose <- NFLX$NFLX.Close
dataLow <- NFLX$NFLX.Low
dataHigh <- NFLX$NFLX.High
dataVolume <- NFLX$NFLX.Volume

dataPrediction <- function(data){
  # Converting to DataFrame by lagging the dataset for prediction. 
  # Also, removing NA values
  
  # data <- dataAdjusted
  stocksData <- data.frame(Lag(data,seq(from=windowSize,to=1)), 
                           data)[(windowSize+1):nrow(data),]
  
  colNames <- c()
  for (i in 1:windowSize){
    colNames[i] <- paste('x', i, sep='') 
  }
  colNames <- colNames[windowSize:1]
  colNames[i+1] <- 'x'
  names(stocksData) <- colNames
  
  # split test and train data
  train <- head(stocksData, floor(nrow(stocksData)*(1-testTrainSplit)))
  test <- tail(stocksData, ceiling(nrow(stocksData)*testTrainSplit))
  
  # Generate Rule for expression
  rules <- list(expr = grule(op(expr, expr), func(expr), var),
                func = grule(sin, cos, exp, log),
                op = grule('+', '-', '*', '/', '^'),
                # TODO populate variables dynamically [cant figure out this one, 
                # will have to change along with change in window size]
                var = grule(mydata$x5, mydata$x4, mydata$x3, 
                            mydata$x2, mydata$x1),
                rn = gvrule(runif(100,-10,10))
  )
  
  ruleGrammar <- CreateGrammar(rules)
  
  # fitness function for calculating best expression
  fitnessFunction <- function(expr) {
    result <- eval(expr)
    if (any(is.nan(result)) || any(is.na(result)))
      return(Inf)
    return (sqrt(mean((mydata$x - result)^2)))
  }
  
  # Training using GE
  mydata <- train
  ge <- GrammaticalEvolution(ruleGrammar, fitnessFunction,
                             terminationCost = 0.05, max.depth = 5)
  predictions <- eval(ge$best$expressions)
  
  # Testing using GE
  mydata <- test
  predictions <- eval(ge$best$expressions)
  mydata <- cbind(mydata, predictions)
  # Next n days predictions
  mydata <- tail(mydata,1)[,c(1:windowSize)]
  variety_predictions <- c()
  for(i in 1:20){
    variety_predictions[i] <- eval(ge$best$expressions)
    for (j in (windowSize:2)){
      mydata[j]<-mydata[j-1]
    }
    mydata[1] <- variety_predictions[i]
  }
  return (variety_predictions)
}
adjustedPredictions <- dataPrediction(dataAdjusted)
openPredictions <- dataPrediction(dataOpen)
closePredictions <- dataPrediction(dataClose)

predictions <- data.frame(cbind(openPredictions, closePredictions, adjustedPredictions))

# Devise a hypothetical simple trading rule:
# if prediction for day n + 1 is at least 3 greater than CP then buy
# if prediction for day n + 1 is at least 3 less than CP then sell
# else hold


decision <- c()

for(i in 1:nrow(predictions)){
  if (predictions$closePredictions[i] >= predictions$openPredictions[i]+0.5)
    decision[i] = 1
  else if (predictions$closePredictions[i] <= predictions$openPredictions[i]-0.5)
    decision[i] = -1
  else
    decision[i] = 0
}


predictions <- cbind(predictions, decision)
predictions
buy <- c()
sell <- c()
buyTrades = 1
sellTrades = 1

buyOK <- TRUE

for(i in 1:(nrow(predictions)-1)){
  if ((predictions$decision[i] == 1) & buyOK){
    buy[buyTrades] = predictions$openPredictions[i+1]
    buyTrades = buyTrades + 1
    buyOK <- FALSE}
  else if ((predictions$decision[i] == -1) & !buyOK){
    sell[sellTrades] = predictions$openPredictions[i+1]
    sellTrades = sellTrades + 1
    buyOK <- TRUE}
}

buy
sell
buy - sell
profit = sum(buy - sell)
profit

  