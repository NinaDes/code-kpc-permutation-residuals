CalculationZ <- function(x,       # data
                        sig,     # gaussian kernel width
                        numcol,  # number of colum in Incomplete Cholesky Decomposition
                        H,       # centering matrix
                        eps){    # regularisation parameter
  
  
  
  PG = IncCD (x, sig, numcol)
  
  tS = tSVD(PG, H)
  U = tS$U
  S = tS$S
  
  G = NewMatrZ(S = S, U = U, eps = eps)
  
  G
  
}