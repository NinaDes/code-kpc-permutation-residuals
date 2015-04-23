NewMatrZ <- function(S,   # the S matrix of SVD: USV
                    U,   # the S matrix of SVD: USV
                    eps){# regularisation parameter
  
  S = S^2/(S^2+eps)
  
  G = U%*%diag(S)%*%t(U)
  
  G
  
  
}