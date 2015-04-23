Gamma_test <- function(X,      # data vector 1
                       Y,      # data vector 2
                       K,      # Gaussian Kernel of X
                       L,      # Gaussian Kernel of Y
                       CK,     # centered Gaussian Kernel of X
                       CL,     # centered Gaussian Kernel of Y
                       number, # hsic number, 
                       alpha,  # test level 
                       ...){

  
  m = ncol(K)
  
  
  mux = 1/(m*(m-1))*sum(K-diag(K)*diag(ncol(K)))
  muy = 1/(m*(m-1))*sum(L-diag(L)*diag(ncol(L)))
  
  media = 1/m * (1 + mux*muy - mux - muy )
  
  varian = (2*(m-4)*(m-5)/(m*(m-1)*(m-2)*(m-3)))*HS_norm(CK,CK)*HS_norm(CL,CL)
  
  al = (media)^2/varian
  be  = varian/(media)
  
  return(1-pgamma(q = number, shape = al, rate = 1/be))
  
#   t  = qgamma(p = 1-alpha, shape = al, rate = 1/be)
#   
#   if( number > t)
#   {
#     print('Gamma: CORRELATED')
#     return (1)
#   }
#   
#   else
#   {
#     print('Gamma: INCORRELATED')
#     return(0)
#   }
  
}