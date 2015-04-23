NewMatrXY <- function(S,   # the S matrix of SVD: USV
                      U   # the S matrix of SVD: USV
                       ){# regularisation parameter
  
  S = S^2
  
  G = U%*%diag(S)%*%t(U)
  
  G
  
  
}