adiac_D <- function(G, # a matrix of teh graph
                    i, # the node whose neighb I want to know
                    ...){
  
  ## I LOOK FOR ONLY <->
# MODE 1  
  ad = NULL
  
  for (j in 1 : ncol(G)) 
  {
    if (G[i, j]==1 && G[j, i]==1)
    {
        ad = c(ad,j)
    }
  }
  
  ad 
  
# MODE 2  
#   b = NULL
#   
#   a = which( G[i, ]==1 )
#   if (length(a)>0)
#   {
#     b = a[ which( G[a, i]==1 ) ]
#     
#   }
#   
#   b
 
#MODE 3  
#  which(G[i,]==1 & G[,i]==1)
  
}