tSVD <- function(PG,   # matrix to which I want to do the SVD 
                 H){   # centering matrix
  
  a = svd(x = H%*%PG)
  
  list(U = a$u, S = a$d)
  
  
  
}