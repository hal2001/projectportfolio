#Machine Learning Methods Applied to Business Solutions
#Finding a way to accurately predict when demand has been adequately fulfilled
#Taweh Beysolow II 
import os
import pandas as pan
import numpy as np
from sklearn import linear_model
from sklearn import preprocessing
from sklearn.svm import SVC
from sklearn.decomposition import PCA
from sklearn import ensemble
from sklearn import metrics
from sklearn import discriminant_analysis 

#Changing the Directory 
os.chdir('/Users/tawehbeysolow/Downloads/Forecasting_Case_Study')

#Uploading the Necessary Files 
test_data = pan.read_csv('test_data.csv', header = 0, sep = ',')
train_data = pan.read_csv('train_data.csv', header = 0, sep = ',')
pred_rslt = pan.read_csv('pred_rslt.csv', header = 0, sep = ',')

##############################################################################################
##############################################################################################
#Data Preparation
##############################################################################################
##############################################################################################
x_train = pan.DataFrame(train_data.loc[:, ['User', 'Reason', 'Item', 'Product_Line', 'PNT', 'Price', 'Sys_p90', 'Sys_p50', 'User_p90', 
                                          'Has_Promo', 'User_p50', 'Sold', 'target', 'Snapshot']]).dropna()
x_train = x_train.reset_index()
y_train = pan.DataFrame(x_train.loc[:, 'target'])
categorical_vars = ['User', 'Reason', 'Item', 'Product_Line', 'Snapshot']
x_cat = x_train.loc[:, categorical_vars]
x_train = x_train.drop(labels = ['index', 'target','User', 'Reason', 'Item', 'Product_Line', 'Snapshot'], axis = 1)

#Handling Vategorical Data/Trasnforming it to be used in Algorithms Later
for i in categorical_vars:
    le = preprocessing.LabelEncoder()
    le.fit(x_cat.loc[:, i])
    x_cat.loc[:,i] = le.transform(x_cat.loc[:,i])

#Converting Has_Promo to Binary Numeric Variable
for i in range(0, len(x_train.Has_Promo)):
    if (x_train.loc[i, 'Has_Promo'] == 'Y'):
        x_train.loc[i, 'Has_Promo'] = 1
    elif (x_train.loc[i, 'Has_Promo'] == 'N'):
        x_train.loc[i, 'Has_Promo'] = 0
    else:
        x_train.loc[i, 'Has_Promo'] = ""

#Merging the Transformed Data Frames
x_train = pan.concat([x_train, x_cat], axis = 1)

##############################################################################################
##############################################################################################
#Variable Selection
##############################################################################################
##############################################################################################

#Principal Components Analysis on Quantitative Data 
pca = PCA(n_components = x_train.shape[1])
pca.fit(x_train)
exp_var = pan.DataFrame(pca.explained_variance_ratio_*100)
exp_var.index = x_train.columns
exp_var.columns = ['Explained Variance per Principal Component (%age form)']

#Our Threshold for what Principal Components we keep will be .50 percent, as I will give consideration to rounding
exp_var = exp_var[exp_var >= 1].dropna()
print(exp_var)

##############################################################################################
##############################################################################################
#Model Building and Comparison of Methods
##############################################################################################
##############################################################################################
#Method 1/Baseline: Logistic Regression
rows = np.random.choice(len(x_train), size = len(x_train), replace = False)
x_t = x_train.loc[rows, exp_var.index]
y_t = y_train.loc[rows]

#Logisitc Regression
log = linear_model.LogisticRegression()
log_fit = log.fit(x_t, y_t.loc[:, 'target'])

#Predicted Response Variable and Data Frame of Predicated and Actual Values
y_h = pan.DataFrame(log.predict(x_t))

#Results and Relevant Statisticus
c_matrix = pan.DataFrame(metrics.confusion_matrix(y_t, y_h))
accept = np.nansum(c_matrix.loc[:,0])
reject = np.nansum(c_matrix.loc[:,1])
precision_log = c_matrix.loc[0,0]/accept
recall_log = c_matrix.loc[0,0]/(c_matrix.loc[0,0] + c_matrix.loc[1,1])
log_score = log.score(x_t, y_t)

#Data Frame of Results
log_results = pan.DataFrame([accept, reject, precision_log, recall_log, log_score])
log_results.index = ['Accept', 'Reject', 'Precision', 'Recall', 'F1 Score']
log_results.columns = ['Logistic Regression']
print(log_results)

##############################################################################################
#Method 2: Shupport Vector Machine 
#Cross Validating Data 
rows = np.random.choice(len(x_train), size = len(x_train), replace = False)
x_t = x_train.loc[rows, exp_var.index]
y_t = y_train.loc[rows]

#Support Vector Classiiation 
svc = SVC()
svc_fit = svc.fit(X = x_t, y = y_t.loc[:, 'target'])

#Predicted Response Variable and Data Frame of Predicated and Actual Values
yh = pan.DataFrame(svc.predict(x_t))

#Results and Relevant Statisticus
con_matrix = pan.DataFrame(metrics.confusion_matrix(y_t, yh))
accept = np.nansum(con_matrix.loc[:,0])
reject = np.nansum(con_matrix.loc[:,1])
precision_svm = con_matrix.loc[0,0]/accept
recall_svm = con_matrix.loc[0,0]/(con_matrix.loc[0,0] + con_matrix.loc[1,1])
svm_score = svc.score(x_t, y_t)

#Data Frame of Results
svm_results = pan.DataFrame([accept, reject, precision_svm, recall_svm, svm_score])
svm_results.index = ['Accept', 'Reject', 'Precision', 'Recall', 'F1 Score']
svm_results.columns = ['Support Vector Machine']
print(svm_results)

#############################################################################################
#Method 3: Random Forest
#Cross Validating Data 
rows = np.random.choice(len(x_train), size = len(x_train), replace = False)
x_t = x_train.loc[rows, exp_var.index]
y_t = y_train.loc[rows]

#Decision Tree Algorithm
f = ensemble.RandomForestClassifier()
f_fit = f.fit(x_t, y_t.loc[:, 'target'])

#Predicted Response Variable and Data Frame of Predicated and Actual Values
yh2 = pan.DataFrame(f_fit.predict(x_t))

#Results and Relevant Statistics
con_matrix2 = pan.DataFrame(metrics.confusion_matrix(y_t, yh2))
accept = np.nansum(con_matrix2.loc[:,0])
reject = np.nansum(con_matrix2.loc[:,1])
precision_f = con_matrix2.loc[0,0]/accept
recall_f = con_matrix2.loc[0,0]/(con_matrix2.loc[0,0] + con_matrix2.loc[1,1])
f_score = f.score(x_t, y_t)

#Data Frame of Results
f_results = pan.DataFrame([accept, reject, precision_f, recall_f, f_score])
f_results.index = ['Accept', 'Reject', 'Precision', 'Recall', 'F1 Score']
f_results.columns = ['Random Forest']
print(f_results)

##############################################################################################
#Method 4: Quadratic Discriminant Analysis 
#Cross Validating Data 
rows = np.random.choice(len(x_train), size = len(x_train), replace = False)
x_t = pan.DataFrame(x_train.loc[rows, exp_var.index])
y_t = pan.DataFrame(y_train.loc[rows])

#Building/Fitting QDA Model
qda = discriminant_analysis.QuadraticDiscriminantAnalysis()
qda_fit = qda.fit(x_t, y_t.loc[:, 'target'])

#Predicted Response Variable and Data Frame of Predicated and Actual Values
yh3 = pan.DataFrame(qda.predict(x_t))

#Results and Relevant Statistics
con_matrix3 = pan.DataFrame(metrics.confusion_matrix(y_t, yh3))
accept = np.nansum(con_matrix3.loc[:,0])
reject = np.nansum(con_matrix3.loc[:,1])
precision_qda = con_matrix3.loc[0,0]/accept
recall_qda = con_matrix3.loc[0,0]/(con_matrix3.loc[0,0] + con_matrix3.loc[1,1])
qda_score = qda.score(x_t, y_t)

#Data Frame of Results 
qda_results = pan.DataFrame([accept, reject, precision_qda, recall_qda, qda_score])
qda_results.index = ['Accept', 'Reject', 'Precision', 'Recall', 'F1 Score']
qda_results.columns = ['Quadratic Discrim. Analysis']
print(qda_results)

##############################################################################################
#Method 5: Perceptron
#Cross Validating Data 
rows = np.random.choice(len(x_train), size = len(x_train), replace = False)
x_t = pan.DataFrame(x_train.loc[rows, exp_var.index])
y_t = pan.DataFrame(y_train.loc[rows])

#Building/Fitting K-Means Model
per = linear_model.Perceptron(penalty = 'l2')
per_fit = per.fit(x_t, y_t.loc[:, 'target'])

#Predicted Response Variable and Data Frame of Predicated and Actual Values
yh4 = pan.DataFrame(per.predict(x_t))

#Results and Relevant Statistics
con_matrix4 = pan.DataFrame(metrics.confusion_matrix(y_t, yh4))
accept = np.nansum(con_matrix4.loc[:,0])
reject = np.nansum(con_matrix4.loc[:,1])
precision_per = con_matrix4.loc[0,0]/accept
recall_per = con_matrix4.loc[0,0]/(con_matrix4.loc[0,0] + con_matrix4.loc[1,1])
per_score = per.score(x_t, y_t)

#Data Frame of Results 
per_results = pan.DataFrame([accept, reject, precision_per, recall_per, per_score])
per_results.index = ['Accept', 'Reject', 'Precision', 'Recall', 'F1 Score']
per_results.columns = ['Perceptron']
print(per_results)

##############################################################################################
##############################################################################################
#Predicting Reponse Variable for Test Set
#Model Chose: Random Forest
##############################################################################################
##############################################################################################
x_test = test_data.loc[:, exp_var.index]

#Replacing Missing Price Variables
na_index = x_test['Price'][np.isnan(x_test['Price'])].index
mean_price = np.mean(x_test['Price'])
x_test.loc[na_index, 'Price']  = mean_price

#Decision Tree Algorithm
f = ensemble.RandomForestClassifier()

#Predicted Response Variable and Data Frame of Predicated and Actual Values
y_test = pan.DataFrame(f_fit.predict(x_test))
Id = pan.DataFrame(np.r_[1:len(y_test), 1])


##############################################################################################
##############################################################################################
#Writing Output to File 
##############################################################################################
##############################################################################################

output = pan.concat([Id, y_test], axis = 1)
output.columns = ['id', 'pred']

output.to_csv('tawehbeysolow_pred.csv', index = False)