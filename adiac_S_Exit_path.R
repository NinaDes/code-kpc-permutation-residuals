adiac_S_Exit_path <- function(G,  # a matrix of teh graph
                         i,  # the node whose neighb I want to know
                         ...){
  
  ## I LOOK FOR ONLY ->
  
  # MODE 1  
  #   ad = NULL
  #   
  #   for (j in 1 : ncol(G)) 
  #   {
  #     if (G[i, j]==1 && G[j, i]==0)
  #     
  #     {
  #       ad = c(ad,j)
  #     }
  #   }
  
  #  ad = which(G[i, ]==1 && G[ ,i ]==0) 
  #  ad 
  
  
  
  # MODE 2  
  #    b = NULL
  #    
  #    a = which(G[i, ]==1)
  #    b = a[which(G[a, i]==0)]
  #     
  #    b
  # MODE 3  
  # which(G[i,]>=1 )
  
    
    a = which(G[i,]==1 & G[,i]==0)
    b = which(G[i,]==2 & G[,i]==0)
    
    a = c(a, b)
    
    a = sort(a)
    
    a
    
}