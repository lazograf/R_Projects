
#----------------------------------------------------------#
#         Technical Analysis & Deep Learning framework     #
#----------------------------------------------------------#



library(quantstrat)
library(xts)
library(quantmod)
if (!exists('.blotter')) .blotter <- new.env()
.blotter <- new.env()
.strategy <- new.env()
library(png)



initdate <- "2015-07-14"
from <- "2015-07-15"
to <- "2019-09-30"

#timezone to UTC
Sys.setenv(TZ = "UTC")

#currency to USD 
currency("USD")


# Retrieve SPY from yahoo
getSymbols("SPY", src="yahoo",from= from, to=to, adjust=TRUE)

stock("SPY", currency="USD")


#trade size and initial equity
tradesize <- 100000
initeq <- 10000000

strategy.st <- "firststrat"
portfolio.st <- "firststrat"
account.st <- "firststrat"

# Remove the existing strategy if it exists
rm.strat(strategy.st)

#-----------------------------------------------------------

# Initialize the portfolio
initPortf(portfolio.st, symbols = "SPY", initDate = initdate, currency = "USD")

# Initialize the account
initAcct(account.st, portfolios = portfolio.st, initDate = initdate, currency = "USD", initEq = initeq)

# Initialize the orders
initOrders(portfolio.st, initDate = initdate)

# Store the strategy
strategy(strategy.st, store = TRUE)

#--------------------------------------------------------

neural_network <- function(data_strategy) {
  
  library(keras)
  library(caret)
  library(TTR)
  library(reticulate)
  use_python("#########################")
  
  
  #Storing the data and creating the indicators
  SPY2 <- getSymbols("SPY", 
                     from = "2008-01-01", 
                     to = "2019-09-30", 
                     src =  "yahoo", 
                     adjust =  TRUE, auto.assign = FALSE)
  
  #Various features are calculated for our NN, but in this example we will not use all of them.
  #Experimentation with different groups of features is made easier, since their calculation is
  #already included in the script
  
  h_l <- Hi(SPY2) - Lo(SPY2)
  colnames(h_l) <- "hi_minus_lo"
  c_o <- Cl(SPY2) - Op(SPY2)
  colnames(c_o) <- "cl_minus_op"
  sma21 <- SMA(Cl(SPY2), n=21)
  colnames(sma21) <- "sma21"
  sma50 <- SMA(Cl(SPY2), n=50)
  colnames(sma50) <- "sma50"
  sma200 <- SMA(Cl(SPY2), n=200)
  colnames(sma200) <- "sma200"
  ema50 <- EMA(Cl(SPY2),n=50)
  colnames(ema50) <- "ema50"
  rsi <- RSI(Cl(SPY2), n=14)
  variance <- rollapply(data = Cl(SPY2), width = 5, var)
  colnames(variance) <- "variance"
  mad <- runMAD(Cl(SPY2), n = 5) #median absolute deviation
  colnames(mad) <- "mad"
  #bbands <- BBands(HLC(SPY), n=21)
  
  vhf <- VHF(HLC(SPY2), n = 15) #vertical horizontal filter: aims to detect trend changes
  #The VHF is calculated by subtracting the n-period lowest low from the n-period highest high and
  #dividing that result by the n-period rolling sum of the close price changes.
  colnames(vhf) <-"vhf"
  
  roc <- ROC(Cl(SPY2), n=14)
  colnames(roc) <- "roc"
  momentum <- momentum(Cl(SPY2),n=50)
  colnames(momentum) <- "momentum50"
  
  ultimate_osc <- ultimateOscillator(HLC(SPY2), n = c(7, 14, 28), wts = c(4, 2, 1)) #The Ultimate
  # Oscillator is a momentum oscillator designed to capture momentum across threedifferent time frames.
  #Created by Larry Williams in 1976.
  
  colnames(ultimate_osc) <- "ulti_oscil"
  
  #1)Buying Pressure - Calculate the Buying Pressure by subtracting the lesser of the low
  #or prior close from the close for the period.
  #2)True Range - Calculate the True Range by subtracting the lesser of the low or prior close from the higher of the high or prior close for the period.
  #3)Averages - For 7-, 14-, and 28-day periods, calculate the ratio of Buying Pressure to True Range.
  #4)Ultimate Oscillator - Create a weighted average of the three averages to generate the
  #Ultimate Oscillator value.
  
  williamsR <- WPR(HLC(SPY2), n = 14)
  colnames(williamsR) <- "williamsR"
  
  roc_vol <- ROC(Vo(SPY2), 14)
  colnames(roc_vol) <- "roc_vol"
  
  
  
  SPY2$Price_Rise <- ifelse(Cl(SPY2) > lag(Cl(SPY2),k=-1),0,1) #CORRECT:" > " #OLD MISTAKE: ifelse(Cl(SPY)<Lag(Cl(SPY)),1,0)
  # lag with k=-1 means forward one value, the opposite off the typical lag in time series
  
  
  
  
  #The dataset
  SPY_data <- SPY2
  
  indicators_names <- c("hi_minus_lo","cl_minus_op","sma21","sma50","rsi",
                        "variance","mad","vhf","roc","roc_vol", "ulti_oscil")
  
  
  indicators <- list(h_l, c_o, sma21, sma50, rsi, variance, mad, vhf, roc,
                     roc_vol, ultimate_osc)
  
  
  for (i in seq(indicators_names)) {
    
    SPY_data <- merge.xts(SPY_data,indicators[[i]]) #by default joins on common dates
    
  }
  
  
  SPY_data <- na.omit(SPY_data) #remove completely rows with NA in ANY column
  SPY_data <- subset(SPY_data, select = -c(1:6))
  head(SPY_data) ; tail(SPY_data)
  
  # ----------# ----------# ----------# ----------# ----------# ----------# ----------# ----------
  
  #Features and the target for the Neural Network
  features <- subset(SPY_data,select = -c(1))
  target <- subset(SPY_data, select = c(1))
  head(features)
  head(target)
  
  
  
  #split_data variable is a workaround method, which is used in order to restrict our NN
  #to train only for data prior (dates) to those that our indicators will be applied. After
  #the training the NN will provide predictions, which will be utilized as signals and will
  #be grouped together with the indicators' signals.
  
  split_data <- length(SPY2$SPY.Close) - length(SPY2$SPY.Close["2015-07-15/2019-09-30"])
  
  train_X <- features[1:split_data,]
  test_X <- features[(split_data+1):(length(features[,1])),]
  length(train_X[,1]) + length(test_X[,1]) == length(features[,1]) #sanity check for the features split
  
  train_Y <- target[1:split_data,]
  test_Y <- target[(split_data+1):(length(target[,1])),]
  length(train_Y[,1]) + length(test_Y[,1]) == length(target[,1]) #sanity check for the target split
  
  
  
  
  #Normalizing train and test features 
  
  matrix_train_X <- as.matrix(train_X)
  matrix_test_X <- as.matrix(test_X)
  
  preprocessing <- preProcess(matrix_train_X, method = c("center","scale")) #(X-Xbar / sd) : normalizing
  trainX <- predict(preprocessing,matrix_train_X)
  testX <- predict(preprocessing,matrix_test_X)
  
  trainY <- as.matrix(train_Y)
  testY <- as.matrix(test_Y)
  
  NN <- keras_model_sequential() 
  NN %>% 
    layer_dense(units = 128, activation = "sigmoid", input_shape = c(dim(trainX)[2]),
                kernel_initializer = "glorot_uniform") %>%
    #layer_dropout(rate = 0.2) %>%
    layer_dense(units = 32, activation = "sigmoid") %>%
    layer_dense(units = 8, activation = "sigmoid") %>%
    layer_dense(units = 1, activation = "sigmoid")
  
  
  NN %>% compile(
    loss = 'binary_crossentropy',
    optimizer = optimizer_adam(),
    metrics = c('binary_accuracy')
  )
  
  history <- NN %>% fit(
    trainX, trainY, 
    epochs = 600 , batch_size = 100, 
    shuffle = TRUE
  )
  
  
  
  predictions <- predict(NN,testX)
  predictions <- ifelse(predictions>0.5,1,0)
  dates <- index(test_X)
  predictions_nn <- xts(predictions, order.by = dates)
  colnames(predictions_nn) <- "neural_network"
  return(predictions_nn)
  
}

AI_predictions <- neural_network()




#--------------------------------------------------------
# Add a 50-day SMA indicator to strategy.st
add.indicator(strategy = strategy.st, 
              
              name = "SMA", 
              arguments = list(x=quote(Cl(mktdata)),n=50), 
              label = "SMA50")

#--------------------------------------------------------


# Add a 21-day SMA indicator to strategy.st
add.indicator(strategy = strategy.st, 
              name = "SMA", 
              arguments = list(x=quote(Cl(mktdata)),n=21), 
              label = "SMA21")


#--------------------------------------------------------
#The average RSI of RSI with 4 days lookback and 3 days lookback.

calc_RSI_avg <- function(price, n1, n2) {
  # RSI 1 takes an input of the price and n1
  RSI_1 <- RSI(price = price, n = n1)
  # RSI 2 takes an input of the price and n2
  RSI_2 <- RSI(price = price, n = n2)
  
  # RSI_avg is the average of RSI_1 and RSI_2
  RSI_avg <- (RSI_1 + RSI_2)/2
  
  colnames(RSI_avg) <- "RSI_avg"
  return(RSI_avg)
}

add.indicator(strategy.st, name = "calc_RSI_avg",
              arguments = list(price = quote(Cl(mktdata)), n1 = 3, n2 = 4),
              label = "RSI_3_4")




#--------------------------------------------------------
#David Varadi Oscillator (DVO)

DVO <- function(HLC, navg = 2, percentlookback = 63) {
  
  ratio <- Cl(HLC)/((Hi(HLC) + Lo(HLC))/2)
  avgratio <- SMA(ratio, n = navg)
  out <- runPercentRank(avgratio, n = percentlookback, exact.multiplier = 1) * 100
  colnames(out) <- "DVO"
  return(out)
}



# Add the DVO indicator to your strategy
add.indicator(strategy = strategy.st, name = "DVO", arguments = list(
  HLC = quote(HLC(mktdata)), navg = 2, percentlookback = 63),label = "DVO_2_126")

#--------------------------------------------------------------------------------------------------------------

# applyIndicators to test the indicators
tail(applyIndicators(strategy = strategy.st, mktdata = OHLC(SPY)))

mktdata <- merge.xts(mktdata,AI_predictions)
tail(mktdata,10)

mktdata <- na.omit(mktdata)
#--------------------------------------------------------

#SIGNALS

#--------------------------------------------------------


add.signal(strategy.st, name = "sigComparison", 
           
           arguments = list(columns = c("SMA21", "SMA50"), 
                            relationship = "gt"),
           label = "longfilter")


#--------------------------------------------------------

add.signal(strategy.st, name = "sigComparison",
           
           arguments = list(columns = c("SMA21", "SMA50"),
                            relationship = "lt"),
           label = "filterexit")

#--------------------------------------------------------

add.signal(strategy.st, name = "sigThreshold", 
           
           arguments = list(column = "DVO_2_126", 
                            threshold = 20, 
                            relationship = "lt", 
                            cross = FALSE), 
           label = "longthreshold")


#--------------------------------------------------------

add.signal(strategy.st, name = "sigThreshold", 
           arguments = list(column = "DVO_2_126", 
                            threshold = 70, 
                            relationship = "gt", 
                            cross = TRUE), 
           label = "thresholdexit")

#--------------------------------------------------------

add.signal(strategy.st, name = "sigThreshold",
           arguments = list(column = "RSI_3_4", threshold = 20,
                            relationship = "lt", cross = FALSE),
           label = "longthreshold2")

#--------------------------------------------------------

add.signal(strategy.st, name = "sigThreshold",
           arguments = list(column = "RSI_3_4", threshold = 70,
                            relationship = "gt", cross = TRUE),
           label = "thresholdexit2")


#--------------------------------------------------------
#################### N.N. ##############################

#N.N. BUY
add.signal(strategy.st, name = "sigThreshold", 
           
           # Reference the column of neural network
           arguments = list(column = "neural_network", 
                            
                            threshold = 0.5, 
                            relationship = "gt", 
                            cross = FALSE), 
           
           
           label = "threshold_buy")
#N.N. SELL
add.signal(strategy.st, name = "sigThreshold", 
           
           arguments = list(column = "neural_network", 
                            threshold = 0.5, 
                            relationship = "lt", 
                            cross = FALSE), 
           label = "threshold_sell")


#--------------------------------------------------------

add.signal(strategy.st, name = "sigFormula",
           
           arguments = list(formula = "longfilter & longthreshold & longthreshold2 & threshold_buy", 
                            cross = TRUE),
           label = "longentry")

#--------------------------------------------------------


# Create your dataset: test
test_init <- mktdata
test2 <- applySignals(strategy = strategy.st, mktdata = test_init)
tail(test2,10) #for debugging
head(test2,10) ##for debugging

#--------------------------------------------------------

#RULES

#--------------------------------------------------------

add.rule(strategy.st, name = "ruleSignal", 
         arguments = list(sigcol = "filterexit", sigval = TRUE, orderqty = "all", 
                          ordertype = "market", orderside = "long", 
                          replace = FALSE, prefer = "Open"), 
         type = "exit")


#--------------------------------------------------------

add.rule(strategy.st, name = "ruleSignal", 
         arguments = list(sigcol = "thresholdexit", sigval = TRUE, orderqty = "all", 
                          ordertype = "market", orderside = "long", 
                          replace = FALSE, prefer = "Open"), 
         type = "exit")


#--------------------------------------------------------

add.rule(strategy.st, name = "ruleSignal", 
         arguments = list(sigcol = "thresholdexit2", sigval = TRUE, orderqty = "all", 
                          ordertype = "market", orderside = "long", 
                          replace = FALSE, prefer = "Open"), 
         type = "exit")


#--------------------------------------------------------

# Create an entry rule,  when all conditions line up to enter into a position
add.rule(strategy.st, name = "ruleSignal", 
         
         arguments=list(sigcol = "longentry", 
                        
                        sigval = TRUE, 
                        orderqty = 500,
                        ordertype = "market",
                        orderside = "long",
                        replace = FALSE, 
                        prefer = "Open"),
         type = "enter")


#--------------------------------------------------------

test_init3 <- mktdata
test_init3 <- head(test_init3,-1)



#-------------------------------------------------------#
#        ****    APPLYING THE STRATEGY  ****            #
#-------------------------------------------------------#

# Use applyStrategy() to apply your strategy. Save this to out
out <- applyStrategy(strategy = strategy.st, portfolios = portfolio.st,
                     mktdata = test_init3)

updatePortf(portfolio.st)
daterange <- time(getPortfolio(portfolio.st)$summary)[-1]

updateAcct(account.st, daterange)
updateEndEq(account.st)



#--------------------------------------------------------
# Get the tradeStats for your portfolio
tstats <- tradeStats(Portfolios = portfolio.st)
print(tstats)

# Print the profit factor
print(tstats$Profit.Factor)


#--------------------------------------------------------

# Use chart.Posn to view system's performance on SPY
plot.new()
chart.Posn(Portfolio = portfolio.st, Symbol = "SPY")

#--------------------------------------------------------



#--------------------------------------------------------
#A Sharpe ratio is a metric that compares the average reward to the average risk taken.

portpl <- .blotter$portfolio.firststrat$summary$Net.Trading.PL
SharpeRatio.annualized(portpl, geometric=FALSE)


#--------------------------------------------------------
# Get instrument returns
instrets <- PortfReturns(portfolio.st)

# Compute Sharpe ratio from returns
SharpeRatio.annualized(instrets)

#Printing the Ending Equity
result <- getAccount(account.st)
Ending.Equity <- tail(result$summary$End.Eq, n = 1)
print(Ending.Equity)



#---------------------------------------------------------------------------------------------




