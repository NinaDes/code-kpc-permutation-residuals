IncCD <- function(x,       # data
                  sig,     # width of gaussian kernel
                  numcol){ # number of colum in Ibcomplete Cholesky Decomposition
  
  a = inchol(as.matrix(x), kernel = 'rbfdot', kpar = list(sigma = 1/sig), maxiter = numcol)
  PG = a@.Data
  
  PG
  
  
}