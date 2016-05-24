#Independent Study: Testing Mean Reversion 
#Taweh Beysolow 
#Professor Yusif Simaan 
#Fordham University 2016

#Clear the workspace 
rm(list = ls())

#Upload necessary packages 
require(quantmod)
require(quantstrat)
require(strucchange)

#Change Directory 
setwd("/Users/tawehbeysolow/Desktop")

#Load Csv File 
data <- read.csv('snp.csv', header = TRUE, stringsAsFactors = TRUE)

#Load Ticker Names 
names  <- as.character(data[,1])
symbols  <- list()

for (i in names){

  symbols[[i]]  <- getSymbols(i, src = 'yahoo', auto.assign=FALSE, return.class = "xts")
  
}

#Getting Market Data 
getSymbols("SPY")
spy.open  <- as.numeric(Op(SPY))
spy.close  <- as.numeric(Ad(SPY))
soret  <- c()
scret <- c()
a  <- length(spy.open)

for (i in 1:a){
  
  soret[i]  <- (spy.open[i+1]/spy.open[i]) - 1
  scret[i]  <- (spy.close[i+1]/spy.close[i]) - 1
  
}

#Removing NA Values 
soret  <- soret[!is.na(soret)]
scret  <- scret[!is.na(scret)]

#Final Return Vector 
spyret  <- c(scret[1:60], soret[61:length(soret)])
spysd  <- runSD(spyret, n = 60)
spysd  <- spysd[!is.na(spysd)]

#Getting Dates 
date  <- row.names(symbols[[1]])

#Get Open and Close Columns for Each Stock 
Open  <- list()
Close  <- list()
m  <- length(symbols)
n  <- nrow(symbols[[1]])

for (j in 1:m){
  
    Open[[j]]  <- as.numeric(symbols[[j]][,1])
    Close[[j]]  <- as.numeric(symbols[[j]][,6])

}
  
#Calculate Returns from Open and Close Data 
#Allocating Space to Open and Close Return Lists 
Oret  <- Open
Cret  <- Close

  for(k in 1:n){
    
    for(j in 1:m){
      
    Oret[[j]][k]  <- (Open[[j]][k+1]/Open[[j]][k]) - 1
    Cret[[j]][k]  <- (Close[[j]][k+1]/Close[[j]][k]) - 1
    
  }
  
}

#Removing NA values
for(j in 1:m){
  
Oret[[j]]  <- Oret[[j]][!is.na(Oret[[j]])]
Cret[[j]]  <- Cret[[j]][!is.na(Cret[[j]])]

}

#Final Return Vector 
ret  <- list()

for (j in 1:m){
  
  ret[[j]]  <- c(Cret[[j]][1:60], Oret[[j]][61:n])
  ret[[j]]  <- ret[[j]][!is.na(ret[[j]])]
}


#Calculating Rolling Window Statistics 
#Allocating necessary space to lists 
avret  <- std <- corr  <- beta  <- alpha  <- resid <-  list()

#List names in order of definition 
#60 day moving avg, std. dev., correlation, beta, alpha, and std. dev. or residuals 

#Calculating average return and standard deviation
for(j in 1:m){
    
    avret[[j]]  <- SMA(ret[[j]], n = 60)
    std[[j]]  <- rollapply(ret[[j]], width = 60, FUN = sd)

  }

#Removing NA Values 
for (j in 1:m){
  
  avret[[j]]  <- avret[[j]][!is.na(avret[[j]])]
  
}

#Calculating Correlation
for(j in 1:m){
  
  corr[[j]]  <- rollapply(cbind(ret[[j]],spyret), width = 60, cor, by.column = FALSE)

}

#Cleaning Data 
for (j in 1:m){
  
  t  <- which(corr[[j]] == 1)
  corr[[j]]  <- corr[[j]][-t]
  
}

#Calculating Beta
for (j in 1:m){
  
  beta[[j]]   <- corr[[j]]*(std[[j]]/spysd)
}

#Removing NA Values 
for (j in 1:m){
  
  beta[[j]]  <- beta[[j]][!is.na(beta[[j]])]
}

#Calculating Alpha 
for (j in 1:m){
  
  alpha[[j]]  <- ret[[j]][60:length(ret[[j]])] - (beta[[j]]*spyret[60:length(spyret)])
  
}

#Calculating Residual Risk 
for (j in 1:m){
  
  resid[[j]]  <- std[[j]] - (beta[[j]]*spysd)
  
}


#Short and Long Portfolio Weights 
wshort  <- wlong  <- list()

for (j in 1:m){
  
  wshort[[j]]  <- -beta[[j]] + 1
  wlong[[j]]  <- 1 - wshort[[j]]
  
}

#Signal 
signal  <- list()

for (j in 1:m){
  
  signal[[j]]  <- alpha[[j]]/resid[[j]]

}

##################################################
#Mean Reversion Algorithm
##################################################
#Initializing portfolio values 
port  <- matrix(nrow = length(avret[[1]]), ncol = length(avret))

port[1,]  <- 1000


for (j in 1:length(avret)){
  
  for (u in 2:length(avret[[1]])){
    
    if (signal[[j]][u] > 2) {
    
      port[u,j]  <- ((wshort[[j]][u-1]*-spyret[u+59]) + (wlong[[j]][u-1])*ret[[j]][u+59])
  
      port[u,j]  <- (port[u,j] + 1)*port[u-1,j]
      
    } else  if (signal[[j]][u] < -2){
      
      port[u,j]  <- port[u-1,j]*(ret[[j]][u+59] + 1)

      
  } else {
    
    port[u,j]  <- port[u-1,j]
    
      }
    }
  }

#Removing Stocks that have less that 5 years worth of return data
lens  <- as.matrix(summary(ret))
lens  <- lens[, c(-2,-3)]
lens  <- as.numeric(lens)
rem  <- which(lens < 252*5)
q  <- 252*5

port  <- port[, -rem]
port  <- port[1:q,]
colnames(port)  <- names[-rem]

port.val  <- c()

for (y in 1:q){
  
port.val[y]  <- sum(port[y ,1:ncol(port)])

}

#Calculating Returns 
port.ret  <- c()

for (y in 1:q){
  
  port.ret[y]  <- (port.val[y+1]/port.val[y]) - 1

}

port.ret  <- port.ret[!is.na(port.ret)]


#Summary Statistics of Portfolio Returns 
Ret.Sum  <- data.frame("max" = max(port.ret),"min" = min(port.ret), 
           "mean" = mean(port.ret) , "Std.Dev" = sd(port.ret))

#Number of Trades 
trades  <- signal

for (j in 1:length(avret)) {
  
  for (u in 1:length((avret[[1]]))) {
    
    if (signal[[j]][u] >= 2) {
      
      trades[[j]][u]  <- 1
      
    } else if  (signal[[j]][u] < -2 ){
      
      trades[[j]][u]  <- 1

    } else {
      
      trades[[j]][u]  <- 0
    }
  }
}

#Transforming to Trade Data to Matrix 
trade  <- matrix(ncol = length(trades), nrow = length(trades[[1]]), byrow = TRUE)

for (j in 1:length(trades)){
  
  for (u in 1:length(trades[[1]])){
    
    trade[u,j]  <- trades[[j]][u]
    
  }
  
}


numtrades  <- c()
  
  for (u in 1:nrow(trade)){
  
  numtrades[u]  <- sum(trade[u,])
  
  }
  
#Summary of Trades 
Trades.Sum  <- data.frame("max"= max(numtrades), "min" = min(numtrades), 
           "median" = median(numtrades), "mean" = mean(numtrades), "Total" = sum(numtrades))

#Portfolio Statistics: Annual
max  <- max(port.ret)
min  <- min(port.ret)
rf  <- 0.0001

#Average Returns
y_1  <- mean(port.ret[1:250])
y_2  <- mean(port.ret[252:504])
annual.ret  <- c(y_1, y_2)
av  <- mean(annual.ret)

#Standard Deviation of Returns
std  <- sd(annual.ret)

#Sharpe Ratio 
sharpe  <- (av - rf)/std

#Plotting Portfolio Value
x <- seq(1, 504, 1)
plot(x, port.val[1:504], main = "Portfolio Value Over Time",
     xlab = "Days", ylab = "Portfolio Value", type = "l", col = "cadetblue", lwd = 1.5)


#Cumulative Return 
cum.ret  <- (port.val[504]/port.val[1]) - 1

#Summary of Portfolio Data
Port.Sum  <- data.frame("Mean"= av, "Std.Dev" = std)
Port.Sum  <- list(Port.Sum, "Yearly Ret." = annual.ret)

#Summary of Strategy 
Strat.Summary  <- list("Returns" = Ret.Sum,
 "Trades" = Trades.Sum, "Portfolio" = Port.Sum, "Cum. Return" = cum.ret,
 "Sharpe" = sharpe)

print(Strat.Summary)


