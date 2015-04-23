Permutation_test <- function(K,     # vector which element will be permutated
                             CL,     # centered kernel matrix of Y
                             p,      # number of permutation
                             sig,    # width of Gaussian Kernel
                             number, # hsic number
                             ... ){
  
  n = nrow(K)
  
  hsic = vector(length = p)
  
  for( i in 1:p)
  {
#   print(i)
#   perm = randcomb(X, n)
#   K = GaussKern(X = perm, sig = sig)
    inds = sample(n) 
    CK = Centering(K = K[inds,inds])
     
    hsic[i] = HS_norm(CK = CK, CL = CL) 
    
  }
  
#  t = quantile(x = hsic, probs = 1-alpha)
  
  
 
    return ( mean(hsic>number) )
  
}
  
  
