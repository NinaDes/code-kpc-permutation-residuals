adiac_S_Enter <- function(G,  # a matrix of teh graph
                          i){  # the node whose neighb I want to know

  
  ## I LOOK FOR ONLY i <-
  

  which(G[i,]==0 & G[,i]!=0)
}