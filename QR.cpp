/*
####################################################
## Stat 202A - Homework 6
## Author: 
## Date : 
## Description: This script implements sweep and QR
## operations in Rcpp
####################################################
 
###########################################################
## INSTRUCTIONS: Please fill in the missing lines of code
## only where specified. Do not change function names, 
## function inputs or outputs. MAKE SURE TO COMMENT OUT ALL 
## OF YOUR EXAMPLES BEFORE SUBMITTING.
##
## Very important: Do not change your working directory
## anywhere inside of your code. If you do, I will be unable 
## to grade your work since R will attempt to change my 
## working directory to one that does not exist.
###########################################################
 
 */ 

# include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;


/* ~~~~~~~~~~~~~~~~~~~~~~~~~ 
 Sign function for later use 
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

// [[Rcpp::export()]]
double signC(double d){
  return d<0?-1:d>0? 1:0;
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~ 
 Problem 1: Sweep operator 
 ~~~~~~~~~~~~~~~~~~~~~~~~~ */

// [[Rcpp::export()]]
NumericMatrix mySweepC(const NumericMatrix B, int m){
  
  // See R code from previous assignment for description
  // of inputs and outputs. Note the "const" in front 
  // of NumericMatrix B; this is so you don't accidentally
  // change B inside your code.
  
  /* Fill in code below */
  
  mat A1 = as<mat>(B);
  int rows = A1.n_rows;
  int cols = A1.n_cols;
  
  for(int k = 0; k < m; k++){
    for(int i = 0; i < rows; i++){
      for(int j = 0; j < rows; j++){
        if(i != k && j !=k){
          A1(i,j) = A1(i,j) - (A1(i,k)*A1(k,j))/A1(k,k);
        }
      }
    }
    
    for(int i = 0; i < rows; i++){
      if(i != k){
        A1(i,k) = A1(i,k)/A1(k,k);
      }
    }
    
    for(int j = 0; j < rows; j++){
      if(j != k){
        A1(k,j) = A1(k,j)/A1(k,k);
      }
    }
    A1(k,k) = (-1)/A1(k,k);
  }
  
  NumericMatrix A = wrap(A1);
  // Return swept matrix A
  return(A);
  
}


/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
 Problem 2: QR decomposition 
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */  

// Now let's use Armadillo  

// [[Rcpp::export()]]
List myQRC(const mat A){ 
  
  // A is the input matrix with dimension n x m.
  
  /* Fill in code below */
  
  mat A1 = A;
  int n = A1.n_rows;
  int m = A1.n_cols;
  
  mat R = A1;
  mat Q = eye(n,n);
  
  for(int k = 0; k < m; k++){
    colvec x(n);
    x.fill(0);

    for(int g = k; g<n; g++){
      x(g) = R(g,k);
    }
    colvec v = x;

    int sign = 0;
    if(x(k) > 0){
      sign = 1;
    }
    else if(x(k) < 0){
      sign = -1;
    }
    v(k) = x(k)+ sign*norm(x,2);

    double s= norm(v,2);
    if(s != 0){
      colvec u = v;
      for(int g = 0; g<v.size(); g++){
        u(g) = v(g)/s;
      }

      R = R - 2*(u*(u.t()*R));
      Q = Q - 2*(u*(u.t()*Q));
    }
  }
  List output;
  // Return a list with two named objects, Q and R
  output["Q"] = Q.t();
  output["R"] = R;
  return(output);
}

  
