Centering <- function(K, # Kernle matrix in input
                      ...){
  
  n= nrow(K)
  
  H = diag(n) -1/n*rep(1,n)%*%t(rep(1, n))
  
  C = H%*%K%*%H
  
  C
}