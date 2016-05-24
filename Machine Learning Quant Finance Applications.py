#Correlation-One Assessment
#Machine Learning Test 
#Taweh Beysolow II 

#Uploading necessary modules
import pandas as pan
import numpy as np
from sklearn import linear_model
import os
from sklearn.preprocessing import StandardScaler
from sklearn.svm import SVR

#Changing the working directory  (step to be changed per user)
os.chdir('/Users/tawehbeysolow/Desktop')

#Upload the Data 
data = pan.read_csv('stock_returns_base150.csv', header = 'infer',
                    delimiter = ',')
                    
#Removing Empty Data
data = data[0:103]
data = pan.DataFrame(data)

#Inputs
x = data.drop(['date','S1'], axis = 1)
y = data.loc[:,'S1']
x_train = x.loc[0:49]
y_train = y.loc[0:49]

########################################################################
#Initial Exploratory Analysis
#Determining which variables are important for predicting S1 
# To do this, we will perform Principal Componenet Analysis on the data
########################################################################

#Standardizing Values 
x_std = StandardScaler().fit_transform(x_train)

#Calculating Covariance Matrix of Training Data 
Sigma = np.cov(x_std.T)

#Eigendecomposition 
evals, evec = np.linalg.eig(Sigma)

#Selecting Princiapl Components 
for ev in evec:
    np.testing.assert_array_almost_equal(1.0, np.linalg.norm(ev))
    

epairs = [(np.abs(evals[i]), evec[:, i]) for i in  range(len(evals))]

epairs.sort()
epairs.reverse()

#Analyzing the variance 
var = sum(evals)
exvar = [(i/var)*100 for i in sorted(evals, reverse = True)]
cumvar = np.cumsum(exvar)


#We now will take only the principal compoentns greater than 1
pcs = []

for i in range(len(exvar) - 1):
    
    if (exvar[i] >= 1):
        
        p = exvar[i]
        pcs.append(p)

pcs = pan.DataFrame(pcs)
pcs.index = [1, 2, 3, 4, 5]
exvar = pan.DataFrame(exvar)
exvar.index = [1,2,3,4,5,6,7,8,9]

#Only the first five principal components meet our criterion
x_train = x_train.drop(['S7', 'S8', 'S9', 'S10'], axis = 1)


############################################################
#Predicting the y values  based off of our x values
#We compare the performance of the training set 
#We shall compare the SST for different methods and discuss 
#The benefits and downfalls of each 
############################################################
#Objective: Iterate over these machine learning algorithms
#to obtain average Sum of Squares

ss1 = []
ss2 = []
ss3 = []
ss4 = []
ss5 = []

for i in range(100):
    

    #Method 1: Ridge Regression 
    #Performing Cross Validation 
    rows = np.random.choice(range(50), size = 50, replace = False)
    x_t = x_train.loc[rows]
    y_t = y_train.loc[rows]

    #Using Ridge regression to predict y values
    rid = linear_model.Ridge(alpha = 1.0)
    y_h = rid.fit(x_t, y_t)
    y_h = rid.predict(x_t)
    y_h = pan.DataFrame(y_h)
    y_t = pan.DataFrame(y_t)

    #Data Frame of Predicted and Actual Values
    ydf = pan.concat([y_t, y_h], axis = 1)
    ydf.columns = ['Actual', 'Predicted']

    #R Squared Value and Sum of Squared Errors
    r_2 = rid.score(x_t, y_t)
    train_sse = ((ydf['Actual'] - ydf['Predicted'])**2).sum()
    

    #############################################################
    #Method 2: Support Vector Regression 
    svr = SVR()

    #Crpss Validation of Data 
    rows = np.random.choice(range(50), size = 50, replace = False)
    x_tr = x_train.loc[rows]
    y_tr = y_train.loc[rows]

    #Support Vector Regression
    yh2 = svr.fit(x_tr, y_tr)
    yh2 = svr.predict(x_tr)
    yh2 = pan.DataFrame(yh2)
    ydf2 = pan.concat([y_tr, yh2], axis = 1)
    ydf2.columns = ['Actual', 'Predicted']

    #R squared value and Sum of Squared Errors
    sse2 = sum((ydf2['Actual'] - ydf2['Predicted'])**2)
    r2_2 = svr.score(x_tr, y_tr)

    
    #################################################################
    #Method 3: Kernel Ridge Regression
    from sklearn.kernel_ridge import KernelRidge

    #Cross Validate the Data
    rows = np.random.choice(range(50), size = 50, replace = False)
    x_tr2 = x_train.loc[rows]
    y_tr2 = y_train.loc[rows]

    #Kernel Ridge Regression
    kr = KernelRidge()
    kr.fit(x_tr2, y_tr2)
    yh3 = kr.predict(x_tr2)
    yh3 = pan.DataFrame(yh3)
    ydf3 = pan.concat([y_tr2, yh3], axis = 1)
    ydf3.columns = ['Actual', 'Predicted']

    #R Sqaured value and Sum of Squares 
    sse3 = sum((ydf3['Actual'] - ydf3['Predicted'])**2)
    r3_2 = kr.score(x_tr2, y_tr2)


    ##################################################################
    #Method 4: Neural Network using Ridge Regression 
    #Cross Validate the Data
    rows = np.random.choice(range(50), size = 50, replace = False)

    #Input Data Array
    x_tr4 = x_train.loc[rows]

    #Output Data Array
    y_tr4 = y_train.loc[rows]

    #Building the Network 
    #Initialize the weights randomly, mean of 0 
    syn0 = 2*np.random.random((5,1)) - 1
    syn0 = syn0.T

    for iter in range(100):
        
        #Forward Propagation
        l0 = x_tr4
        rid = linear_model.Ridge(alpha = 1.0)
        l1 = rid.fit(l0, y_tr4)
        l1 = rid.predict(l0)
        
        #Error
        l1_e = y_tr4 - l1
        l1_d = l1_e * l1
    
        #Update Weights
        syn0 += np.dot(l0.T,l1_d)


    yh4 = l1
    yh4 = pan.DataFrame(yh4)
    ydf4 = pan.concat([y_tr4, yh4], axis = 1)
    ydf4.columns = ['Actual', 'Predicted']

    #R Sqaured value and Sum of Squares 
    sse4 = sum((ydf4['Actual'] - ydf4['Predicted'])**2)
    r4_2 = rid.score(x_tr4, y_tr4)

    ############################################################
    #Method 5: Stochastic Gradient Descent 
    from sklearn.linear_model import SGDRegressor

    #Cross Validate the Data
    rows = np.random.choice(range(50), size = 50, replace = False)
    x_tr5 = x_train.loc[rows]
    y_tr5 = y_train.loc[rows]

    #Gradient Descent
    grad = SGDRegressor()
    grad.fit(x_tr5, y_tr5)
    yh5 = grad.predict(x_tr5)
    yh5 = pan.DataFrame(yh5)

    #Data Frame of Values
    ydf5 = pan.concat([y_tr5, yh5], axis = 1)
    ydf5.columns = ['Actual', 'Predicted']

    #R Squared and Sum of Squares
    sse5 = sum((ydf5['Actual'] - ydf5['Predicted'])**2)
    r5_2 = grad.score(x_tr5, y_tr5)
    
    ss1.append(train_sse)
    ss2.append(sse2)
    ss3.append(sse3)
    ss4.append(sse4)
    ss5.append(sse5)

##########################################################################
#Summary Statistics of Machine learning Models 
##########################################################################

#Ridge Regression 
min1 = min(ss1)
max1 = max(ss1)
av1 = np.mean(ss1)
sd1 = np.std(ss1)
ran1 = max1 - min1
sum1 = pan.DataFrame([min1, max1, av1, sd1, ran1, r_2])
sum1.index = ['Min', 'Max', 'Mean', 'Std', 'Range', 'R Sq']

#Support Vector Regression 
min2 = min(ss2)
max2 = max(ss2)
av2 = np.mean(ss2)
sd2 = np.std(ss2)
ran2 = max2 - min2
sum2 = pan.DataFrame([min2, max2, av2, sd2, ran2, r2_2])
sum2.index = ['Min', 'Max', 'Mean', 'Std', 'Range', 'R Sq']

#Kernel Ridge Regression 
min3 = min(ss3)
max3 = max(ss3)
av3 = np.mean(ss3)
sd3 = np.std(ss3)
ran3 = max3 - min3
sum3 = pan.DataFrame([min3, max3, av3, sd3, ran3, r3_2])
sum3.index = ['Min', 'Max', 'Mean', 'Std', 'Range', 'R Sq']

#Neural Network 
min4 = min(ss4)
max4 = max(ss4)
av4 = np.mean(ss4)
sd4 = np.std(ss4)
ran4 = max4 - min4
sum4 = pan.DataFrame([min4, max4, av4, sd4, ran4, r4_2])
sum4.index = ['Min', 'Max', 'Mean', 'Std', 'Range', 'R Sq']

#Stochastic Gradient Descent 
min5 = min(ss5)
max5 = max(ss5)
av5 = np.mean(ss5)
sd5 = np.std(ss5)
ran5 = max5 - min5
sum5 = pan.DataFrame([min5, max5, av5, sd5, ran5, r5_2])
sum5.index = ['Min', 'Max', 'Mean', 'Std', 'Range', 'R Sq']


print("Summary SSE Ridge Regression: \n", sum1, "\n")
print("Summary SSE Support Vector Regression: \n", sum2, "\n")
print("Summary SSE Kernel Ridge Regression: \n", sum3, "\n")
print("Summary SSE Neural Network: \n", sum4, "\n")
print("Summary SSE Stochastic Gradient Descent: ", sum5, "\n")

#######################################################################
#Additional Data Analysis 
#######################################################################

#Does S1 go up cumulatively on an open to close basis over this period 
s1 = np.cumsum(y_train)
s1df = pan.concat([s1, y_train], axis = 1)
s1df.columns = ['Cumulative', 'Change per Period']
s1cum = np.sum(y_train)

import matplotlib.pyplot as plt
x = np.r_[1:50, 50]
plt.plot(x, s1, 'r')
plt.xlabel('Days')
plt.ylabel('Cumulative Change in S1 from Beginning')

#Comparing S1 with Predicted S1 values without Cross Validation 
s1_h = svr.predict(x_train)
s1_h = np.cumsum(s1_h)
s1_h = pan.DataFrame(s1_h)

#Adding Predicted S1 to plot
plt.plot(x, s1_h, 'g')

sdf = pan.concat([s1, s1_h], axis = 1)
sdf.columns = ['Actual', 'Predicted']

#Sum of Errors in Non Cross- Validated Data
se = sdf['Actual'] - sdf['Predicted']
sdf = pan.concat([sdf, se], axis = 1)
sdf.columns = ['Actual', 'Predicted', 'Error']
sesum = np.sum(se**2)


#Summary Data by period on Cros Validated Data
er = ydf2['Actual'] - ydf2['Predicted']
er = pan.DataFrame(er)

fdf2 = pan.concat([ydf2, er], axis = 1)
fdf2.columns = ['Actual', 'Predicted', 'Error']
fdf2.index = [data['date'][0:50]]


###########################################################################
#Analyzing Effect of Volatility on Model 
########################################################################### 

#Friday to Monday Volatility 
fm = [1,2,6,7, 11, 12, 16, 17, 21, 22, 26, 27, 31, 32, 36, 37, 41, 42, 
      46, 47]

tth = [0, 3, 4, 5, 8, 9, 10, 13, 14, 15, 18, 19, 20, 23, 24, 25, 28, 29, 30, 33, 34, 35,
    38, 39, 40, 43, 44, 45, 48, 49]


#Sum of Squares 
ssrf = sdf['Error'][fm]
ssrt = sdf['Error'][tth]
ssrf = sum(ssrf**2)
ssrt = sum(ssrt**2)
sst = np.sum([ssrf, ssrt])

sos = [ssrf, ssrt, sst]
sos = pan.DataFrame(sos)

frimon  = sdf['Actual'][fm]
sd_frimon = np.sum(np.std(frimon))

#Volatility on days other than Friday and Monday
tueth= sdf['Actual'][tth]
sd_tueth = np.sum(np.std(tueth))

#Total Volatility 
sd_total = np.sum(np.std(y_train))

#Comparing Volatilities 
#Number of Days per Period
tot = len(sdf['Actual'])
f_m = len(frimon)
t_th = len(tueth)

day = [f_m, t_th, tot]
day = pan.DataFrame(day)

#Percentage of Total Days 
dtot = len(sdf['Actual'])/len(sdf['Actual'])
dfm = len(frimon)/len(sdf['Actual'])
dtth = len(tueth)/len(sdf['Actual'])

pct = [dfm, dtth, dtot]
pct = pan.DataFrame(pct)

#Summary of Volatility
voldf = pan.DataFrame([sd_frimon, sd_tueth, sd_total])
voldf = pan.concat([voldf, day, pct, sos], axis = 1)

voldf.columns = ['Vol', '# Days', '% Days', 'SSRs']
voldf.index = ['F - M' , 'Tu - Th', 'Total']

print(voldf)

#####################################################
#Final Answer 
#####################################################

#Predicting out of sameple with Support Vector Regression
x_te = data.loc[50:99]
x_te = x_te.drop(['date', 'S1', 'S7', 'S8', 'S9', 'S10'], axis = 1)
y_te2 = svr.predict(x_te)

#Creating Final Data File 
date = data['date'][50:100]
finalans = pan.DataFrame(y_te2)
finalans.index = date

#Writing Data To CSV File 
finalans.to_csv('predictions.csv', index_label = ['Date', 'Value'],
                                sep = ',')