GaussKern <- function(X,            # data matrix
                      sig,   # the width of the gaussian kernel
                      ...){
  
  K = kernelMatrix(rbfdot(sigma = 1/sig), x = X)  
  K
}
  