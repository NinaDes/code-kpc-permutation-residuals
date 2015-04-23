condIndipTestP_Resid <- function (x,        # the first variable for cond ind
                            y,        # the second variable for cond ind
                            z,        # the set of variables to wich I condition
                            sig,      # width of gaussian kernel
                            numcol,   # number of column of incomplete cholesky decomposition 
                            eps,      # regularisation parameter
                            p,        # number of permutation of 1-alpha level test
                            paral){   # 1 if I don't want to parallelize or a number n >1 to parallelize with n core 
  
  
  
  
  
  
  
  n = nrow(as.matrix(x))
  H = diag(n)-1/n*matrix(rep(x = 1, length.out = n*n), n, n)
  
  #   # Incomplete Cholesky Decomposition
  #   PxGx = IncCD (x, sig, numcol)
  #   PyGy = IncCD (y, sig, numcol)
  #   PzGz = IncCD (z, sig, numcol)
  #   
  #   # Centering Matrix
  #   
  #   # Thin Single Value Decomposition 
  #   Ux = tSVD(PxGx, H)$U
  #   Sx = tSVD(PxGx, H)$S
  #   
  #   Uy = tSVD(PyGy, H)$U
  #   Sy = tSVD(PyGy, H)$S
  #   
  #   Uz = tSVD(PzGz, H)$U
  #   Sz = tSVD(PzGz, H)$S
  #   
  #   # New matrix definition
  #   Gx = NewMatr(S = Sx, U = Ux, eps = eps)
  #   Gy = NewMatr(S = Sy, U = Uy, eps = eps)
  #   Gz = NewMatr(S = Sz, U = Uz, eps = eps)
  
  Gx = CalculationXY(x, sig, numcol, H)
  Gy = CalculationXY(y, sig, numcol, H)
  Gz = CalculationZ(z, sig, numcol, H, eps)
  
  number = HSnorm(Gx, Gy, Gz)
     
   if(paral==1)
   {
     pval = PermTest_Resid(number, Gx, y, z, Gz, p, sig, numcol, H)
   }
   
   else
   {
     pval = PermTest_ResidPARAL(number, Gx, y, z, Gz, p, sig, numcol, H, paral)
   }
   
  
  return(pval)
  
  
  
} 