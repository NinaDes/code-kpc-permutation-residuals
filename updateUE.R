updateUE <- function(G,           # adjacency matrix
                     upd,         # the nodes that has been updated
                     formerUpd){  # the node that needed to be updated but still havn't been
  
  n = ncol(G)
  
  undiEdge = list()                    # in the first n elements of the list I put a vector which rappresent the node to which th i-th elem (i = 1:n) is related with undirected edge
  undiEdge[[n+1]] = vector(length = n) # in the n+1 element of the list I put a vector long n which count how many undirected edge has the i-th element(i = 1:n) 
  undiEdge[[n+2]] = rep(0,n)           # in the n+2 element of the list I put a binary vector long n in which 1 means: this node has been previously updated
                                                                                                   #0 means: this node hasn't been previously updated
  
  if(length(upd)>0)
  {
    undiEdge[[n+2]][upd]=1
  }
  
  if(length(formerUpd)>0)
  {
    undiEdge[[n+2]][which(formerUpd==1)]=1
  }
  
  
  for ( i in 1:n)
  {
#    undiEdge[[i]] = which(G[i,]==1 & G[,i]==1)
    
    # doing this I can modify also collider:
   undiEdge[[i]] = which(G[i,]!=0 & G[,i]!=0)
   
    
    undiEdge[[n+1]][i] = length( undiEdge[[i]] )
  }
  
    undiEdge
} 