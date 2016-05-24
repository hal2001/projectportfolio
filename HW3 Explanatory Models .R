#Explanatory Models HW3 - Taweh Beysolow 
#Clear the workspace
rm(list = ls())

#uploade packages
require(MASS)

#Set working directory 
setwd("/Users/tawehbeysolow/Desktop")

#Upload Data Files
inflation  <- read.csv("inflation.csv", header = TRUE, stringsAsFactors = TRUE)
tax  <- read.csv("tax payments.csv", header = TRUE, stringsAsFactors = TRUE)
internet  <- read.csv("internet.csv", header = TRUE, stringsAsFactors = TRUE)
trade  <- read.csv("trade.csv", header = TRUE, stringsAsFactors = TRUE)
gdp_capita  <- read.csv("GDP Capita.csv", header = TRUE, stringsAsFactors = TRUE)
gdp_growth  <- read.csv("GDP Growth .csv", header = TRUE, stringsAsFactors = TRUE)
gini  <- read.csv("GINI.csv", header = TRUE, stringsAsFactors = FALSE)


#Cleaning Data
country <- inflation[,1]
n  <- nrow(inflation)

#Extracting  2011
inflation_clean  <- inflation[2:n,56]
tax_clean  <- tax[2:n,56]
internet_clean  <- internet[2:n,56]
trade_clean  <- trade[2:n,56]
gdp.cap_clean  <- gdp_capita[2:n,56]
gdp.growth_clean  <- gdp_growth[2:n,56]
gini.clean  <- gini[1:n, 56]


#Removing Countries/Rows Without Gini Indices 
na  <- which(is.na(gini.clean) == TRUE)
gini.clean  <- gini.clean[!is.na(gini.clean)]
inflation_clean  <- inflation_clean[-na]
tax_clean  <- tax_clean[-na]
internet_clean  <- internet_clean[-na]
trade_clean  <- trade_clean[-na]
gdp.cap_clean  <- gdp.cap_clean[-na]
gdp.growth_clean  <- gdp.growth_clean[-na]
country  <- as.vector(country[-na])

#Data Frame of Variables 
data_frame  <- data.frame(inflation_clean, tax_clean, 
                    internet_clean, trade_clean, gdp.cap_clean, gdp.growth_clean, gini.clean)
rem  <- c(1, 13, 39, 62, 55, 10,
          41, 33, 35, 58, 8, 11, 21, 24, 31, 44, 48, 52, 68)

data_frame  <- data_frame[-rem,]

row.names(data_frame)  <- country[-rem]

cor(data_frame)

#Linear Regression between each variable to check correlations
#Manual Backwards stepwise regression performed based off of statistical significance
#Removing least significant variable to greatest
#objective: maximize R^2 value

#Gini curve is assumed to not be a linear function, hence log transformation of variables

gini.1 <- lm(log(data_frame[,7]) ~ data_frame[,1] + data_frame[,2] + data_frame[,3] +
                   log(data_frame[,4]) + log(data_frame[,5]) + data_frame[,6])

summary(gini.1)


#Gini.1 equation with no log transformation
gini.2  <- lm(data_frame[,7] ~ data_frame[,1] + data_frame[,2] + data_frame[,3] +
                data_frame[,4] + data_frame[,5] + data_frame[,6])

summary(gini.2)

#We observe higher R^2 value, so we will continue manual backwards stepwise regression now removing 
#log transformations based on statistical significance from least to greatest in removal order 
gini.3  <- lm(log(data_frame[,7]) ~  data_frame[,2] + data_frame[,3] +
                log(data_frame[,4]) + log(data_frame[,5]) + data_frame[,6])

summary(gini.3)
#Statisticalsignificance keeps decreasing. As such, we conclude that gini.1 is the best model 

par(mfrow = c(2,2)) 
plot(gini.1)



#Log Transformed Data Frame 
log_df  <- data.frame(data_frame[,1], data_frame[,2] , data_frame[,3] ,
                        log(data_frame[,4]) , log(data_frame[,5]) , data_frame[,6], log(data_frame[,7]))

colnames(log_df)  <- c("inflation_clean", "tax_clean", "internet_clean", 
                       "trade_clean", "gdp.cap_clean", "gdp.growth_clean", "gini.clean")



#Comparing Correlations between log trasnformed equation and linear equation
cor(log_df)
cor(data_frame)

pairs(log_df)
pairs(data_frame)

#Comparing Theoretical Gini Variables to Actual 
gini_df  <- data.frame(row.names(data_frame), gini.1$fitted.values, log_df[,7])
Residual <- (log_df[,7]-gini.1$fitted.values)

#Making Data Frame
gini_df  <- cbind(gini_df, Residual)
colnames(gini_df) <- c("Country", "Predicted", "Actual", "Residual")
mean_y  <- mean(gini_df[,2])
n  <- nrow(gini_df)

#More summary statistics on the data
SST  <- sum((Residual - mean_y)^2)
corr  <- cor(gini_df[, 2:3])

gini_df  <- list("Gini Data" = gini_df, "SST" = SST)


#Running linear regression against predicted and actual gini index vectors
gini_final.lm  <- lm(gini_df[[1]][,2] ~ gini_df[[1]][,3])
summary(gini_final.lm)

#Residual Plots
par(mfrow = c(2,2))
plot(gini_final.lm)
corr
