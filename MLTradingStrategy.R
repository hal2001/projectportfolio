#Machine Learning Statistical Arbitrage
#Clear the workspace 
rm(list = ls())

#Uploading necessary packages 
require(quantmod)
require(nnet)
require(e1071)
require(caTools)
require(MASS)
stocks  <- c("F", "SPY", "DJIA", "HAL", "MSFT", "SWN", "SJM", "SLG", "STJ")
stockdata  <- list()

#Uploading the symbols of our choice
for (i in stocks){
  stockdata[[i]]  <- getSymbols(i, src = "yahoo", auto.assign = FALSE,  return.class = "xts", from = "2010-01-01", to = "2013-01-01")
}

#Creating Matrix of close prices 
df  <- matrix(nrow = nrow(stockdata[[1]]), ncol = length(stockdata))

for (i in 1:length(stockdata)){
  df[,i]  <- stockdata[[i]][ ,6]
}

#Calculating Returns
return_df  <- matrix(nrow = nrow(df), ncol = ncol(df))

for (j in 1:ncol(return_df)){
  for(i in 1:nrow(return_df) - 1){
    return_df[i,j]  <- (df[i+1, j]/df[i,j]) - 1
  }
}


#Removing last row since it is an NA VALUE
return_df  <- return_df[-nrow(return_df), ]

#Making DataFrame with all values except label IE all columns except for Ford since we are trying to predict this
#Determing Which Variables Are Unnecessary 
pca_df  <- return_df[, -1]

pca  <- prcomp(pca_df)

summary(pca)
#Run Summary To Determin the Proportion of the Variance you are getting back 
#In this case, since PCs 7 and 8 contribute relatively little, we will exclude variables 
#7 and 8. I have said the cutoff is 1%, and although 7 barely meets this threshold, I think 6 variables will be enough 

#Editing Existing Return Data 
new_returns  <- return_df[, 1:7]
colnames(new_returns)  <- stocks[1:7]

###########################################################################################################
#This is the point at which we can start to choose algorithms to pick. As stated, I will choose two algorithms: 
#Support Vector Regression and a Neural Network. For now, I will use packages. If we have enough time, 
#We should work on making a Nueral Network. If we really have enough time, we can try to recreate the support vector regression. 
#I warn you that these are not necessarily the easiest to recreate, so this is why I think that this should be approached at a later time
###########################################################################################################

#Preventing overfitting usually requires some sort of variable sampling technique. Here I am doing something called 
#Cross Validation. This is where we randomly sample the obeservations within each given stock (within this context)
#To see the stability of this process, let us see how this performs over time

#Algorithm 1: Support Vector Regression
results  <- c()
mse_svr  <- c()

for (i in 1:50){
  
  #Crossvalidating data to pevent overfitting
  samplerows  <- sample(1:nrow(new_returns), replace = TRUE)
  test_df  <- new_returns[samplerows, ]

  #Fitting Support Vector Regression
  svr  <- svm(test_df[,1] ~ test_df[,2] + test_df[,3] + test_df[,4] + test_df[,5]
              + test_df[,6] + test_df[,7], test_df, kernel = "polynomial", type = "eps-regression")
  
  #This represent our predicted values from this regression
  y_h  <- as.vector(svr$fitted)
  
  #Testing The Mean Squared Error Amongst Other Stats 
  error_df  <- data.frame(test_df[,1], y_h)
  colnames(error_df)  <- c("Actual", "Predicted")
  
  #Mean Squared Error  
  mse_svr[i]  <-  sum((error_df[,2] - mean(error_df[,1]))^2)
                  
  #Appending MSE
  results  <- append(results, mse_svr[i])
  
}

#Summary of Sum of Squares Data With R^2 
svr_summary  <- list("mean" = mean(results), "max" = max(results), "min" = min(results), "stdev" = sd(results))
print(svr_summary)

###################################################################################
#Method 2: Ridge Regression as the Activation Functon for a Neural Network
#I apologize, the functions with built in packlages weren't working. Instead, I built alot of the models by hand 
#Feel free to validate whether these results look reasonable to you, I will be doing the same. 

#Ridge Regression Function
ridge  <- function(x,y,lambda){
  I  <- diag(ncol(x))
  beta_0  <- mean(y)
  gamma  <- lambda*I
  beta_h  <- (ginv(t(x)%*%x + t(gamma)%*%gamma)) %*% (t(x) %*% y)
  RSS <- (sum(y - x%*%beta_h)^2)
  y_h  <- x%*%beta_h  
  return(y_h)
}

#Initializain the weights randomly
syn0  <- rep(0, 6)
results  <- c()
mse_nnet  <- c()
#Forward Propagation Algorithm 
for (i in 1:50){
  
    #Crossvalidating data to pevent overfitting
    samplerows  <- sample(1:nrow(new_returns), replace = TRUE)
    test_df  <- new_returns[samplerows, ]
    
    #X and Y inputs
    layer_0 = test_df[,2:ncol(test_df)]
    y = test_df[,1]
    
    #Ridge Regression Function
    ridreg  <- ridge(x = layer_0, y = test_df[,1], lambda = 1)
    
    #Multipying Activation Function by Weights
    ridgreg  <- ridreg%*%t(syn0)
    
    #Error 
    layer1_error  <- y - ridreg
    layer1_d  <- layer1_error*ridreg
    
    #Updating Weights
    syn0  <- t(layer_0)%*%layer1_d
    
    #Sum of Squared Errors
    mse_nnet[i]  <- sum((ridreg - mean(y))^2)
    results  <- append(results, mse_nnet[i])
}

nnet_summary  <- list("mean" = mean(results), "max" = max(results), "min" = min(results), "stdev" = sd(results))
print(nnet_summary)
###################################################################################
###################################################################################
#Part 3: Using the Neural Network to Price tHe Equiies within the Stat Arb Meta-algorithm
#This is where the technique actually will become useful, and more importantly will 
#start to show its usefulness
###################################################################################
###################################################################################
#Making a Ridge Regression Function
ridge_nnet  <- function(x, y, lambda){
   
    #Inputs 
    layer_0 = x
    
    #Initializain the weights randomly
    syn0  <- rep(0, 6)
    results  <- c()
    error = 100
    
    #Forward Propagation Algorithm 
    for (i in 1:1000){
    
    #Ridge Regression Function
    ridreg  <- ridge(x = x,  y = y, lambda = lambda)
    ridgreg  <- ridreg%*%t(syn0)
    
    #Error 
    layer1_error  <- y - ridreg
    layer1_d  <- layer1_error*ridreg
    
    #Updating Weights
    syn0  <- t(layer_0)%*%layer1_d
    
    #Sum of Squared Errors
    sse_nnet  <- sum((ridreg - y)^2)
    } 
    return(ridreg) 
}
#Getting Current Data for Stocks We're Analyzing
new_stocks  <- stocks[1:7]
newstockdata  <- list()
for (i in new_stocks){
  newstockdata[[i]]  <- getSymbols(i, src = "yahoo", auto.assign = FALSE, return.class = "xts", from = "2013-01-02", to = "2016-01-02")
}

#Creating Data Frame For Price Data 
price_data  <- matrix(nrow = nrow(newstockdata[[1]]), ncol = length(newstockdata))
for (i in 1:length(new_stocks)){ 
  price_data[,i]  <- newstockdata[[i]][,6]
}

#Calculating Returns 
close_returns  <- open_returns  <- matrix(nrow = nrow(price_data), ncol = ncol(price_data))
colnames(close_returns)  <- colnames(open_returns)  <- new_stocks
for (j in 1:ncol(price_data)){
  for(i in 1:nrow(price_data) - 1){
    close_returns[i,j]  <- (price_data[i+1,j]/price_data[i,j]) - 1
    open_returns[i,j]  <- (price_data[i+1,j]/price_data[i,j]) - 1
  }
}
#Making DataFrame of First 60 Days of Close Returns and Remainder Close Returns 
algo_returns  <- rbind(close_returns[1:60,], open_returns[61:nrow(open_returns), ])

#Removing NA Row
algo_returns  <- algo_returns[1:755, ]

#Stat Arb Meta Algorithm Data 
statarb_returns  <- algo_returns[, -1]

statarb_y  <- algo_returns[, 1]

#Prediting Y Returns 
y_h  <- ridge_nnet(y = statarb_y, x = statarb_returns, lambda = .001)

#Creating Data Frame For Errrs
errors  <- cbind(statarb_y, y_h)

#Mse for this iteration
mse  <- sum((errors[,1] - mean(errors[,2]))^2)

#Residuals In a Given Period
residuals  <-  (errors[,2] - errors[,1])
###################################################################################
#Other Data: Stdev of Residuals and Returns 
#Calculating Rolling Statistics along 60 day scale:  Std_Residuals
std_resid  <- runSD(residuals, 60)
resid_df  <- cbind(std_resid, residuals)
#Removing NA Values
resid_df  <- resid_df[60:nrow(resid_df), ]
#Calculating the Signal 
signal  <- (resid_df[,2]/resid_df[,1])
resid_df  <- cbind(resid_df, signal)
colnames(resid_df)  <- c("Stdev", "Residuals", "Signal")
###################################################################################
#Stat Arb Algorithm 
###################################################################################
#Simple Example of a One Stock Portfolio
portfolio  <- matrix(nrow = nrow(resid_df), ncol = 1)
portfolio[1,1]  <-  10000

#Calculating Ford Returns using Modified Stat Arb Algorithm
ford  <- statarb_y[60:nrow(statarb_returns)]
for (i in 2:nrow(portfolio)){
  if (resid_df[i - 1, 3] >= 3){
    portfolio[i]  <- (1.25)*portfolio[i - 1] * (1 + -ford[i - 1])
  } else if (resid_df[i - 1, 3] <= -3){
    portfolio[i]  <- (1.25)*portfolio[i - 1] * (1 + ford[i - 1])
  } else if (resid_df[i - 1, 3] >= 1.5){
    portfolio[i]  <-  portfolio[i - 1] * (1 + -ford[i - 1])
  } else if (resid_df[i - 1, 3] <= - 1.5){
    portfolio[i]  <- portfolio[i - 1] * (1 + ford[i - 1])
  } else {
    portfolio[i]  <- portfolio[i - 1]
  }
}
#Return Data on Strategy 
ret  <- c()
for (i in 1:length(portfolio)){
  ret[i]  <- (portfolio[i+1]/portfolio[i]) - 1
}
#Removing NA Data
ret <- ret[!is.na(ret)]
###################################################################################
#Summary Statistics on Ford Algorithm: Daily Basis
strat_summary  <- list("Max Return" = max(ret), "Min Return" = min(ret), "Avg Return" = mean(ret), "Std.Dev" = sd(ret),
                             "No. Trades" = length(which(ret != 0)), "Cumulative Return" = (portfolio[length(portfolio)]/portfolio[1]) - 1,
                       "Sharpe Ratio" = (mean(ret) - 0)/sd(ret))

print(strat_summary)
#Plot Strategy Return 
x  <- seq(1,length(portfolio), 1)
plot(x, portfolio, main = "Return of Strategy Over Time", xlab = "Trading Days", ylab = "Portfolio Value Initialized at $10K", 
     col = "cadetblue", lwd = 1.5, type = "l")

