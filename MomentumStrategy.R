#Replication of a Case Study of Using Momentum and Value at the Asset Level
#Taweh Beysolow 

#Clear the workspace 
rm(list = ls())

#Changing the working directroy
setwd("/Users/tawehbeysolow/Desktop")

#Upload the necessary packages 
require(quantmod)
require(strucchange)

#Uploading the necessary data
#Price Data
uk_equities  <- read.csv("uk_data.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
japan_equities  <- read.csv("topix_data.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
us_equities  <- read.csv("snp_data.csv", header = TRUE, sep =  ",", stringsAsFactors = FALSE)
commodities  <- read.csv("commodities_data.csv", sep = ",", stringsAsFactors = FALSE)
canada_equities  <- read.csv("tsx_data.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
us_reit  <- read.csv("reit_data.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
credit  <- read.csv("igc_data.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
cash  <- read.csv("cash_data.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
pacific_equities  <- read.csv("pacific_data.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
europe_equities  <- read.csv("europe_data.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
treasuries  <- read.csv("treasuries_data.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
  
#Obtaining proper time periods for each set of equities 
#Date Range is from 1975 through Dec 2013
date  <- as.numeric(us_equities[1248:1716, 1])
us_data  <- as.numeric(us_equities[1248:1716, 2])
cape  <- as.numeric(us_equities[1260:1716, 11])
japan_data  <- as.numeric(japan_equities[1:469, 2])
commodities_data  <- as.numeric(commodities[1:469, 2])
canada_data  <- as.numeric(canada_equities[1:469, 2])
credit_data  <- as.numeric(credit[1:469, 2])
cash_data  <- as.numeric(cash[497:965, 2])
uk_data  <- as.numeric(gsub(",", "", uk_equities[1:469, 2]))
reit_data  <- as.numeric(gsub(",", "", us_reit[1:469, 2]))
pacific_data  <- as.numeric(gsub(",", "", pacific_equities[1:469, 2]))
europe_data  <- as.numeric(gsub(",", "", europe_equities[1:469, 2]))
treasuries_data  <- as.numeric(treasuries[1:469, 2])

#Combining Data Frame of Total Values 
df  <- data.frame(us_data, japan_data,  uk_data, reit_data,  pacific_data,  europe_data, commodities_data, canada_data, cash_data, credit_data,  treasuries_data)

###################################################################
#Calculating Returns 
returns  <- matrix(nrow = nrow(df), ncol = ncol(df))
for (j in 1:ncol(df)){
  for (i in 1:nrow(df)){  
    returns[i,j]  <- df[i + 1, j]/df[i,j] - 1
  }
}
colnames(returns)  <- colnames(df)

returns  <- returns[1:nrow(returns) - 1, ]
###################################################################
#Calculating 12 month Lagged Returns
lagged_returns  <- matrix(nrow = nrow(returns), ncol = ncol(returns))
yearlyindex  <- seq(1, 446, 12)

for (i in 1:ncol(df)){  
  lagged_returns[,i]  <- SMA(returns[,i], 12)   
}

#Removing NA Values
lagged_returns  <- lagged_returns[12:nrow(lagged_returns), ]

#Adding Labels to Matrix of Lagged Return Data
row.names(lagged_returns)  <- date[13:length(date)]
colnames(lagged_returns)  <- colnames(df)

#Creating Matrix of Initial Weights For the Value Strategy
weights_momentum <- matrix(nrow = nrow(lagged_returns), ncol = ncol(lagged_returns))
weights_momentum[1,]  <- c(.209, .077, .040, .10, .016, .074, .10, .014, .15, .10, .10)
addition  <- .02/11
weights_momentum[1,]  <- weights_momentum[1,] + addition
colnames(weights_momentum)  <- colnames(lagged_returns)

#Equal Weight Portfolio 
equalweights  <- rep(.10, ncol(weights_value))

#Centering Level Data 
centeringlevel <- c(.06, .06, .06, .06, .06, .06,  .01, .06, 1, .015, .03)

signal  <- matrix(nrow = nrow(lagged_returns), ncol = ncol(lagged_returns))
colnames(signal)  <- colnames(df)

##############################################################
#Momentum Portfolio Weight Update
###################################################################
for (j in 1:ncol(lagged_returns)){
  for (i in 2:nrow(lagged_returns)){
    if(lagged_returns[i,j] > 0){
      weights_momentum[i,j]  <- weights_momentum[1,j]*(3/2)
    } else if (lagged_returns[i,j] <= 0){
      weights_momentum[i,j]  <- weights_momentum[1,j]*(1/2)
    }    
  }
}

#Leverage Constraint Placed on Each Asset
resid_momentum <- matrix(nrow = nrow(weights_value), ncol = ncol(weights_value))

for(i in 1:nrow(weights_momentum)){
      if (sum(weights_value[i,], na.rm = TRUE) - .15  > 1){
        resid_momentum[i,]  <-  (sum(weights_momentum[i,]*resid_momentum[i,]))/(max(weights_momentum[i,], 1))
      } else {
        resid_momentum[i,]  <- 1
  }
}

#Rescaling Weights
weights_momentum  <- weights_momentum*resid_momentum


####################################################################
#Returns on Portfolios: Initializing Size
reduced_returns <- momentumport_returns  <- valueport_returns <- returns[12:nrow(returns) , ]

#Value Portfolio Returns
#valueport_returns  <- reduced_returns*weights_value

#Momentum Portfolio Returns
for (j in 1:ncol(weights_momentum)){
  for(i in 1:nrow(weights_momentum)){
    momentumport_returns[i,j]  <- reduced_returns[i,j]*weights_momentum[i,j]
  }
}


#Simulated Return Serieis 
mom_init <- matrix(nrow = nrow(reduced_returns), ncol = ncol(reduced_returns))
mom_init[1, ]  <- rep(100000, ncol(reduced_returns))

for (j in 1:ncol(reduced_returns)){
  for (i in 2:nrow(reduced_returns)){
    mom_init[i,j]  <- mom_init[ i - 1, j]*(1 + momentumport_returns[i - 1, j])
  }
}

cum_mom  <- cumret_mom  <- c()
for (i in 1:nrow(reduced_returns)){
  cum_mom[i]  <- sum(mom_init[i,])
}

for(i in 2:nrow(reduced_returns)){
  cumret_mom[i - 1]  <- (cum_mom[i]/cum_mom[i - 1]) - 1
}

###############################################################################################################
#Summary Statsistics of Value Strategy
###############################################################################################################
cat("Average Return of Strategy", print(mean(cumret_mom)))
cat("Standard Deviation of Strategy", print(sd(cumret_mom)))
cat("Minimum Return of Strategy", print(min(cumret_mom)))
cat("Maximum Return of Strategy", print(max(cumret_mom)))

