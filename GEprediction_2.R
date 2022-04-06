library(quantmod)
library(gramEvol)
library(TTR)
library(PerformanceAnalytics)

stock <- 'NFLX'
fromDate <- '2019-01-01'
toDate <- '2022-01-01'
windowSize <- 10
testTrainSplit <- .20
predictionWindow <- 100

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
                var = grule(mydata$x10, mydata$x9, mydata$x8, mydata$x7, mydata$x6, mydata$x5, mydata$x4, mydata$x3, 
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
  for(i in 1:predictionWindow){
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
highPredictions <- dataPrediction(dataHigh)
lowPredictions <- dataPrediction(dataLow)
volumePredictions <- dataPrediction(dataVolume)

predictions <- data.frame(cbind(openPredictions, highPredictions, lowPredictions, 
                                closePredictions, volumePredictions, adjustedPredictions))
predictions <- xts(predictions, order.by = seq(as.Date(toDate), by="days", 
                                               length.out = predictionWindow))

dataWithPrediction <- rbind(NFLX,predictions)
chartSeries(dataWithPrediction$NFLX.Close)
addSMA(n=50, on=1, col='blue')
addSMA(n=100, on=1,col='red')


sma_nflx_predictedLow <-SMA(predictions$closePredictions,n=3)
sma_nflx_predictedHigh <-SMA(predictions$closePredictions,n=6)

# SMA 5 signal
sma5_nflx_predicted <- Lag(
  ifelse(Lag(predictions$closePredictions) < Lag(sma_nflx_predictedLow) & predictions$closePredictions > sma_nflx_predictedLow,1,
         ifelse(Lag(predictions$closePredictions) > Lag(sma_nflx_predictedLow) & predictions$closePredictions < sma_nflx_predictedLow,-1,0)))
sma5_nflx_predicted[is.na(sma5_nflx_predicted)] <- 0

# SMA 10 signal
sma10_nflx_predicted <- Lag(
  ifelse(Lag(predictions$closePredictions) < Lag(sma_nflx_predictedHigh) & predictions$closePredictions > sma_nflx_predictedHigh,1,
         ifelse(Lag(predictions$closePredictions) > Lag(sma_nflx_predictedHigh) & predictions$closePredictions < sma_nflx_predictedHigh,-1,0)))
sma10_nflx_predicted[is.na(sma10_nflx_predicted)] <- 0

# SMA 5 and 10 Crossover Signal
sma_nflx_predicted <- Lag(
  ifelse(Lag(sma_nflx_predictedLow) < Lag(sma_nflx_predictedHigh) & sma_nflx_predictedLow > sma_nflx_predictedHigh,1,
         ifelse(Lag(sma_nflx_predictedLow) > Lag(sma_nflx_predictedHigh) & sma_nflx_predictedLow < sma_nflx_predictedHigh,-1,0)))
sma_nflx_predicted[is.na(sma_nflx_predicted)] <- 0

#SMA buy and sell strategy
sma_nflx_strat <- ifelse(sma_nflx_predicted > 1,0,1)
for (i in 1 : length(predictions$closePredictions)) {
  sma_nflx_strat[i] <- ifelse(sma_nflx_predicted[i] == 1,1,ifelse(sma_nflx_predicted[i] == -1, 0 ,sma_nflx_strat[i-1]))
}

#SMA Position = 1 means holding and SMA Position = 0 means no holdings
#SMA Signal = 1 mean buy stock and SMA Signal = -1 mean sell stock
sma_nflx_strat[is.na(sma_nflx_strat)] <- 0
sma_nflx_stratcomp <- cbind(sma_nflx_predictedLow, sma_nflx_predictedHigh, sma_nflx_predicted, sma_nflx_strat, tail(dataWithPrediction$NFLX.Close, n = 100))
colnames(sma_nflx_stratcomp) <- c('SMA(5)','SMA(10)','SMA SIGNAL','SMA POSITION', 'Close')



chartSeries(predictions)
addSMA(n=6, on=1, col='blue')
addSMA(n=3, on=1,col='red')
chartSeries(NFLX)
addSMA(n=10, on=1, col='blue')
addSMA(n=200, on=1,col='red')

