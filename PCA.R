#########################################################
## Stat 202A - Homework 7
## Author: 
## Date : 
## Description: This script implements PCA and logistic
## regression.
#########################################################

#############################################################
## INSTRUCTIONS: Please fill in the missing lines of code
## only where specified. Do not change function names, 
## function inputs or outputs. You can add examples at the
## end of the script (in the "Optional examples" section) to 
## double-check your work, but MAKE SURE TO COMMENT OUT ALL 
## OF YOUR EXAMPLES BEFORE SUBMITTING.
##
## Very important: Do not use the function "setwd" anywhere
## in your code. If you do, I will be unable to grade your 
## work since R will attempt to change my working directory
## to one that does not exist.
#############################################################

## First load in the Rcpp script containing your 
## QR decomposition code. Make sure your cpp file
## is named 'QR.cpp' and is in the current 
## working directory. Finally, make sure your 
## QR function in Rcpp is named "myQRC"

## DO NOT CHANGE THE FOLLOWING 5 LINES
library(Rcpp)
filename <- 'QR.cpp'
sourceCpp(filename)
if(filename != 'QR.cpp')
  stop("I didn't bother reading the instructions!!!!!")


######################
## Function 1: PCA  ##
######################

myEigen_QR <- function(A, nIter){
  
  ## Perform PCA on matrix A using your QR function, myQRC.
  ## A: Square matrix
  ## nIter: Number of iterations
  
  ## FILL IN CODE HERE ##
  ## Don't forget to centralize A ##
  
  r = dim(A)[1]
  c = dim(A)[2]
  
  V = matrix(rnorm(r*r), nrow=r)
  
  for (i in 1:nIter){
    list_qr = myQRC(V)
    
    Q = list_qr[[1]]
    V = A %*% Q
  }
  list_qr = myQRC(V)
  D = diag(list_qr[[2]])
  V = list_qr[[1]]
  
  ## Function should output a list with D and V, where
  ## D is a vector of eigenvalues of A, and V is the 
  ## matrix of eigenvectors of A (in the same order as 
  ## the eigenvalues in D.)
  return(list("D" = D, "V" = V))
  
}

#####################################
## Function 2: Logistic Regression ##
#####################################

## First, define expit (sigmoid) function
expit <- function(x){
  1 / (1 + exp(-x))
}

myLogistic <- function(X, Y, epsilon = 1e-6){
  
  ## X is an n x p matrix of explanatory variables
  ## Y is an n dimensional vector of binary responses
  ## (e.g. Yi is 0 or 1).
  ## Do NOT simulate data in this function. n and p
  ## should be determined by X.
  ## Use myQRC inside of this function
  
  ## FILL CODE HERE ##
  X = cbind(rep(1,nrow(X)),X)
  
  r = dim(X)[1]
  c = dim(X)[2]
  
  beta_logistic =array(0,c(c,1))
  
  while (TRUE) {
    eta = X %*% beta_logistic
    pr = expit(eta)
    
    w = pr * (1-pr)
    z = eta + (Y-pr)/w
    sw = sqrt(w)
    
    mw = matrix(rep(sw, c),ncol = c)
      
    x_work = mw * X
    y_work = sw * z
    
    n = dim(x_work)[1]
    p = dim(x_work)[2]
    Z = cbind(x_work,y_work)
    list_qr = myQRC(Z)
    R = list_qr[[2]]
    R1 = R[1:(p),1:(p)]
    Y1 = R[1:(p),(p+1)]
    beta_new = solve(R1,Y1)
    
    err = sum(abs(beta_new-beta_logistic))
    
    beta_logistic = beta_new
    
    if(err < epsilon){
      break;
    } 
  }
  
  ## Function returns beta_logistic, the solution to 
  ## the logistic regression problem
  return(beta_logistic)

}


# X = matrix(rnorm(4*5), nrow=4)
# A = t(X) %*% X
# print(eigen(A))
# print(myEigen_QR(A,1000))
# 
# n = 100
# p =10
# X = matrix(rnorm(n*p), nrow=n)
# Y = sample(x=c(0,1), size = n, replace = TRUE)
# print(myLogistic(X, Y))
# print(glm(Y~X, family="binomial"))
