#Machine Learning Final Project: Princiapl Component Analysis on the Iris Data Set
#Taweh Beysolow 
#Prof. Antonio Moretti 
#Fordham University 
#Code referenced from Sebastian Rashka 

import numpy as np
import matplotlib.pyplot as plt
from sklearn.preprocessing import StandardScaler
import pandas as pan

data = pan.read_csv('https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data',
    header=None,
    sep=',')

#Initilizing Data 
X = data.ix[:, 0:3].values
y = data.ix[:, 4].values

#Visualizing the 3 different flower classes 
labels = {1: 'Iris-Setosa', 2: 'Iris-Versicolor', 
         3:'Iris-Virginica'}

features = {0: 'sepal length (cm)', 1: 'sepal width (cm)', 2: 'petal length (cm)',
            3: 'petal width (cm)'}
 
           
with plt.style.context('seaborn-whitegrid'):
    plt.figure(figsize = (8,6))
    for cnt in range(4):
        plt.subplot(2,2, cnt+1)
        for lab in ('Iris-setosa', 'Iris-versicolor', 'Iris-virginica'):
            plt.hist(X[y==lab, cnt],
                     label = lab,
                     bins = 10, 
                     alpha = 0.3)
        plt.xlabel(features[cnt])
        plt.legend(loc='upper right', fancybox=True, fontsize=8)

        plt.tight_layout()
        plt.show()

#Standardizing Values 
X_std = StandardScaler().fit_transform(X)

#Calculating the Covariance Matrix 
Sigma = np.cov(X_std.T)
print('Covariance Matrix \n%s' %Sigma)

#Perofming Eigendecomposition 
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

with plt.style.context('seaborn-whitegrid'):
    plt.figure(figsize=(6, 4))

    plt.bar(range(4), exvar, alpha=0.5, align='center',
            label='individual explained variance')
    plt.step(range(4), cumvar, where='mid',
             label='cumulative explained variance')
    plt.ylabel('Explained variance ratio')
    plt.xlabel('Principal components')
    plt.legend(loc='best')
    plt.tight_layout()


#Projection Matrix 
#Choosing the first two principal components of this data 
w = np.hstack((epairs[0][1].reshape(4,1), epairs[1][1].reshape(4,1)))
print('W: \n', w)


#Projection onto New Feature Space 
Y = X_std.dot(w)

with plt.style.context('seaborn-whitegrid'):
    plt.figure(figsize=(6, 4))
    for lab, col in zip(('Iris-setosa', 'Iris-versicolor', 'Iris-virginica'),
                        ('blue', 'red', 'green')):
        plt.scatter(Y[y==lab, 0],
                    Y[y==lab, 1],
                    label=lab,
                    c=col)
    plt.xlabel('Principal Component 1')
    plt.ylabel('Principal Component 2')
    plt.legend(loc='lower center')
    plt.tight_layout()
    plt.show()
                        



