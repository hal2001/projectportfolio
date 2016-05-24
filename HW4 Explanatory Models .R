#HW 4 Explanatory Models Taweh Beysolow 
#Clear the workspace
rm(list = ls())

#Set the working Directory 
setwd("/Users/tawehbeysolow/Desktop")

#Upload Necessary Packages 
require(ggplot2)
require(lattice)
require(nnet)
require(pROC)
require(ROCR)
#Upload the necessary data 
data  <- read.csv("SpeedDating.csv", header = TRUE, stringsAsFactors = TRUE)

#Question 1
#Extracting all the columns
agem  <- data$AgeM
agef  <- data$AgeF
intm  <- data$IntelligentM
intf  <- data$IntelligentF
ambm  <- data$AmbitiousM
ambf  <- data$AmbitiousF
sharm  <- data$SharedInterestsM
sharf  <- data$SharedInterestsF
male  <- data[,1]
female  <- data[,2]
ram  <- data$RaceM
raf <- data$RaceF
dmat  <- table(male, female)
dmat

sd  <- dmat[2,2]/276
sd

#Question 2 
second.date  <- c()

for (i in 1:length(male)){
  
  if (data[i,1] + data[i,2] == 2){
    
    second.date[i]  <- 1
  } else {
    
    second.date[i]  <- 0
  }
  
}


data  <- cbind(data, second.date)

#Scatterplot of data with color coding
mj  <- jitter(male, factor = 1)
fj  <- jitter(female, factor = 1)

plot(mj, fj, main = "Male over Female Decisions", xlab = "Male Decision",
     ylab = "Female Decision", col = ifelse(second.date == 1, "blue", "red"))

#Question 3 
length(which(is.na(data)))

#Question 4
#Mosaic Plot
racedf  <- table(ram,raf)
mosaicplot(racedf, main="Races Observed Within Study",
           xlab="Male Races", ylab="Female Races", las=TRUE,
           col=c("firebrick", "navyblue", "green", "cadetblue", "red"), cex.axis=1.2)

#Question 5 
#Model 1
lr1  <- glm(second.date ~ agem + agef + intm +intf + ambm + ambf + sharm + sharf, 
            family = binomial(link = "logit"), data = data)
summary(lr1)

lr2  <- step(lr1)

lr2  <- glm(second.date ~ agem + sharf + ambf + ambm, family = binomial(link = "logit"), data = data)
summary(lr2)

lr3  <- glm(second.date ~ sharm + sharf, family = binomial(link = "logit"), data = data)
summary(lr3)

#lr3 is the best model 
logmod  <- lr3

#Observing Residuals
par(mfrow = c(2,2))
plot(logmod)


#Question 5
#Removing NA Values 
second.edit  <- second.date
df  <- cbind(sharm,sharf)

rem  <- which(is.na(df[,1]))
df  <- df[-rem,]
second.edit  <- second.edit[-rem]
re  <- which(is.na(df[,2]))
df  <- df[-re,]
second.edit  <- second.edit[-re]

logcurve  <- function(x1 = df[,1], x2 = df[,2]){
  l  <- list()
  second <- exp(as.vector(logmod$coef[1] + (logmod$coef[2]*x1) + (logmod$coef[3]*x2)))
  e.y  <- second/(1+second)
  odd  <- (second/(1+second))/(1/(1+second))
  l  <- c(odd)
  return(l)

}


#Question 6
one.edit  <- second.date[-re]
one.edit  <- one.edit[-rem]
newtable  <- table(one.edit, second.edit)


#Determine Threshold 
thres  <- 1
thres.2  <- .20
thres.3  <- mean(logcurve())
thres.4  <- mean(mean(logcurve(), median(logcurve())))

f  <- logcurve()
f.2  <- logcurve()
f.3  <- logcurve()
f.4  <- logcurve()

#Make New Confusion Matrix 
f  <- ifelse((f >= thres), 1, 0)
f.2  <- ifelse((f.2 >= thres.2), 1, 0)
f.3 <- ifelse((f.3 >= thres.3), 1, 0)
f.4  <- ifelse((f.4 >= thres.4), 1, 0)

#Compare with original table
nt <- table(second.edit, f)
nt.1  <- table(second.edit, f.2)
nt.2  <- table(second.edit, f.3)
nt.3  <- table(second.edit, f.4)

nt.3 

#Question 7 
summary(logmod)

#Question 8
#Determine best thresholds for logistic regression model 


#Question 9 
#Calulate accuracy, sensitivity, specifiticy and construct ROC Curve 
#Accuracy 
tp  <- nt.3[1,1]
fp  <- nt.3[1,2]
fn  <- nt.3[2,1]
tn  <- nt.3[2,2]
acc  <- (tp + tn)/ length(f)
sen  <- (tp)/sum(nt.3[,1])
spec  <- tn/sum(nt.3[,2])

acc
sen 
spec

#Construct ROC Curve
roc(response = second.edit, predictor = f.4, plot=TRUE, las=TRUE,   legacy.axes=TRUE, lwd=5,
    main="ROC for Speed Dating Analysis", cex.main=1.6, cex.axis=1.3, cex.lab=1.3)
