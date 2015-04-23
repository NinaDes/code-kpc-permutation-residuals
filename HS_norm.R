HS_norm <- function(CK, #centered kernel matrix #1
                    CL, #centered kernle matrix #2
                    ...){
   n = nrow(CK)
#   
#   hsic = 0
#   
#   for(i in 1:n)
#   {
#     for( j in 1:n)
#     {
#       hsic = hsic + CK[i,j]*CL[i,j]
#     }
#   }
# 
# hsic = 1/(n*n)*hsic
# hsic
  
  
  sum(CK*CL)/(n*n)
}

