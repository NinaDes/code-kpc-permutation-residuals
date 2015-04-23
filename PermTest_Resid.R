PermTest_Resid <- function(number, # the HSIC of x and y given z
                           Gx,     # Gx matrix
                           y,      # data y
                           z,      # data z
                           Gz,     # Gz matrix
                           p,      # number of permutation
                           sig,    # gaussian kernel width
                           numcol, # number of column in Incomplete Cholesky Decomposition
                           H){      # centering matrix
  
  
  library(mgcv)
  
  z = as.matrix(z)
  
  n = length(y)
  
  
  regr = data.frame(y, z)
  nc = ncol(regr)
  colnames(regr) = paste("x",1:nc,sep="")
  form = create_formula(1, 2:nc, "x")
  
  fit  = gam(form, dat = regr)
  
  
  ypred = fit$fitted.values
  res   = fit$residuals
  
  hsic = vector(length=p )
  
  for (i in 1:p ) # for each permutation
  {
    
#     print(paste0('Permutation #',i))
    
    yper = ypred + res[sample(n)]
    
    Gyper = CalculationXY(yper, sig, numcol, H) # I calculate the HSIC
    hsic[i] = HSnorm(Gx, Gyper, Gz )               # with the new permutated data
  }
  
  return( mean(hsic>number)  )  # I return the pvalue
  
  
  
}