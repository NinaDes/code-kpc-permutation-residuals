HSIC <- function(X,    # the first variable 
                 Y,    # the second variable 
                 sig,  # width of gaussian kernel 
                 p,    # total number of permutation
                 test){# type of test (1 = perm, 2 = gamma, 3 = both)   
                 
  
  X = as.matrix(X)
  Y = as.matrix(Y)
  
  K = GaussKern(X = X, sig = sig) 
  L = GaussKern(X = Y, sig = sig)
  
  CK = Centering(K = K)
  CL = Centering(K = L)
  
  number = HS_norm(CK = CK, CL = CL)
  
  perm = NULL
  gamma = NULL
  
  if(test ==1)
  {
    perm = Permutation_test(K = K, CL = CL, p = p, sig = sig, number = number)
    return(perm)
  }
  
  if (test == 2)
  {
    gamma = Gamma_test(X = X, Y = Y, K = K, L = L, CK = CK, CL = CL, number = number, alpha = alpha)
    return(gamma)
  }
  


  
  # the test is H0 : PxPy  = Pxy
  #             H1 : PxPy != Pxy
  
}