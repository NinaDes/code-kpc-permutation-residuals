HSnorm <- function(Gx,   # matrix Gx
                   Gy,   # matrix Gy
                   Gz){  # matrix Gz
  
  n = nrow(Gx)
  
  1/(n*n)*tr( Gx%*%Gy  -2*Gx%*%Gz%*%Gy + Gx%*%Gz%*%Gy%*%Gz)
  
  
}