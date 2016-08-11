#Machine Learning Algorithm
#Impleneting Several Different Machine Learning Algorithms 
#Taweh Beysolow 

#import necessary packages 
import numpy as np
import pandas as pan
import math

#Creating Randomized Data 
y = pan.DataFrame(np.random.sample(100))
x = pan.DataFrame(np.random.sample(100))

#Univariate Gradient Descent
def uniGradient(x, y, alpha = .001, epsilon = .0001, maxiter = 1000):        
    #Initializing Variables
    n = len(y)
    theta_0 = 0
    theta_1 = 0
    #Sum of Squares 
    cost = np.nansum((theta_0 + theta_1*x) - y**2)
    error = 0
    converged = False
    iter = 0
    #Gradient Descent Algorithm 
    while not converged:  
        #Gradient for both Parameters
        grad_0 = (1/n)*np.sum([(theta_0 + theta_1*x.loc[i] - y.loc[i]) for i in x])
        grad_1 = (1/n)*np.sum([(theta_0 + theta_1*x.loc[i] - y.loc[i])*x.loc[i] for i in x]) 

        #Reassigning Theta Parameters
        temp_0 = theta_0 - alpha * grad_0
        temp_1 = theta_1 - alpha * grad_1        
        theta_0 = temp_0
        theta_1 = temp_1

        #Calculating the Sum of Squares
        error = np.nansum((theta_0 + theta_1*x) - y**2)     
        
        if (abs(cost - error) <= epsilon):
            converged = True
        cost = error
        iter += 1
        if (iter == maxiter):
            converged = True
        
    output = list([error, theta_1, theta_0, iter, float(abs(cost - error))])
    return output

uniGradient(x,y)

#Sigmoid Function Fo Be Used in Neural Network 
def sigmoid(x, deriv = False):
    if(deriv == True):
        return x*(1-x)
    return 1/(1 +np.exp(-x))
#Single Layer Nueral Network using Logistic Regression as Activation Function 
#Continue Working On This
def twoLayer(x,y, maxiter = 1000, epsilon = .0001):
   #Initalizing Variables
    weights = pan.DataFrame(np.random.random((x.shape[0])))
    converged = False
    iter = 0
    cost = 0
    y = pan.DataFrame(y)
    #Forward Propagation Algorithm
    while not converged:
      layer_0 = x
      layer_1 = pan.DataFrame(sigmoid(layer_0*weights))
      #Calculating the error and finding the gradient
      error  = y - layer_1
      mse = np.nansum((layer_1 - np.mean(y))**2)
      gradient = error * sigmoid(layer_1, deriv = True)
      
      if (abs(cost - mse) <= epsilon):
          converged = True
      iter += 1
      cost = mse
      #Updating Weights
      weights += gradient*layer_1
      if(iter == maxiter):
          converged = True
            
    output = list([layer_1, iter, error])
    return output

twoLayer(x,y)  

#Unfinished from this point forward  
'''
#K Means Clustering Algorithm 
#Still Working on this 
x = pan.DataFrame(np.random.random((100,100)))
def kmeansCluster(x, k = 10, epsilon = .10, maxiter = 1000):
    X = x.T
    k_means = []
    max_k = X.shape[0]
    if (k > max_k):
        print('K chosen is larger than observations present. K chosen instead is', max_k)
        k = max_k
    #Finding the meas of K Clusters
    for i in range(0, k):
        k_means.append(np.mean(X.loc[i, :]))

    #Assignment Step 
    centroid_assignment = pan.DataFrame()
    
    for u in range(0, k):
        assign = []
        for i in range(0, X.shape[0]):
            for j in range(0, X.Shape[1]):
                if (X.loc[j,i] - k_means[u] <= epsilon):
                    assign.append()
##########################################################################################
#Multivariate Gradient Descent Algorithm
#Not Finished Yet, Please Ignore 

def multiGradient(x, y, alpha = .001, epsilon = .0001, maxiter = 1000):
    #Initalize Variables
    n = len(y)
    theta = pan.DataFrame(np.repeat(0, n))
    theta = theta.T
    #Sum of Squares
    cost = (1/n)*np.sum((theta.loc[:, 0] + theta.loc[:,1:n] * x) - y**2)
    error = 0 
    iter = 0
    converged = False 
    while not converged:           
        #Gradient Descent Algorithm 
        grad_0 = (1/n)*np.sum([(theta.loc[:,0] + theta.loc[:, 1:n]*x.loc[i] - y.loc[i]) for i in x])
        grad_k = (1/n)*np.sum([(theta.loc[:,0] + theta.loc[:, 1:n]*x.loc[i] - y.loc[i])*x.loc[i,:] for i in x])
        temp_0 = theta.loc[:,0] - alpha * grad_0
        temp_k = theta.loc[:, 1:n] - alpha*grad_k
        theta_0 = temp_0
        theta_k = temp_k
        
        #Hypothesized Lin e
        h_0 = theta_0 + theta_k*x
        
        error = np.sum((h_0 - y)**2)
        
        if (abs(cost - error) <= epsilon):
            converged = True
            
        cost = error
        iter += 1
        if (iter == maxiter):
            converged = True
        

    output = list([error, h_0, iter, abs(cost - error)])
    return output
    
'''



    

    