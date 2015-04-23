SecondStage <- function(data,        # data
                        G_ad,        # the adjacency matrix
                        G_dir,
                        undiEdge,
                        s,
                        alpha,
                        sig,
                        p,
                        test){ 
  

  
  G = G_ad
  
  n = ncol(G)
#   H = G
  
  numUndiEdge = undiEdge[[n+1]]
  updated     = undiEdge[[n+2]]
  
  s = s

  aux = 0
  
  maxUndiEdge = max(numUndiEdge)
  print(paste0('maxundiedge=',maxUndiEdge))
  while(maxUndiEdge >= s)
  {
    print(paste0('s=',s))
    nodeSa = which(numUndiEdge == s) # I consider only node that has s undirected edge
    nodeSb = which(numUndiEdge < s & numUndiEdge > 0 & updated ==1  ) # I consider nodes that has less than s undirected edge and has been updated in the previous step
    nodeS = c(nodeSa, nodeSb)
    print(paste0('I test the node',nodeS))
    if(length(nodeS)>0)
    {
      for ( i in 1:length(nodeS)) # for each of this nodes that has [1,s] undirected edge
      {   
        print(paste0('i=',i,'==> I consider the node',nodeS[i]))
        undiEdge[[n+2]][nodeS[i]] = 0
        
        vectUndEdgeUi = undiEdge[[ nodeS[i] ]] # is a vector containing the nodes to which nodeS[i] is related with undir edge   
        parent = adiac_S_Enter(G, nodeS[i])  # parents of nodeS[i]
        print(paste0('It is connected to nodes',vectUndEdgeUi))
        print(paste0('His partets are',parent))
        t = s
        
          while ( t > 0 ) # considero tutti i sottinsiemi con cardinalitò pari a t. aprto dal nodo nodeS[i]
          {          
            if( t <= length(vectUndEdgeUi) )
            {
              print(paste0('t=',t))
              if(length(vectUndEdgeUi) == 1)
              {
                sub = vectUndEdgeUi
                sub = t(as.matrix(sub))
              }
              
              else
              {
                sub = combn(vectUndEdgeUi, t)               
              }
              j = 1
              
              while ( j <= ncol(sub) )
                #for( j in 1:ncol(sub) ) # per ogni sottinsieme
              {     
                print(paste0('I check the subset',sub[,j]))
                if( NOTImmort(G, nodeS[i], sub[,j]) == 1)
                {
                  print(paste0('j=',j))
                  
                  ir = indipResid(data, G, undiEdge, nodeS[i], parent, sub[,j], alpha, sig, p , test) ## risposta binaria 0 o 1
                  print(paste0('==> ir=',ir))
                  if( ir == 1)
                  {
                    print('Enter')
                    
                    compl = vectUndEdgeUi[ -c(which(vectUndEdgeUi == sub[,j]))  ]                  
                    
                    G[sub[,j], nodeS[i]] = 1
                    G[nodeS[i], sub[,j]] = 0
                    
                    G_dir[sub[,j], nodeS[i]] = 1
                    G_dir[nodeS[i], sub[,j]] = 0
                    #                     Fai apparire questa modifica in undiEdge
                    
                    if(length(compl)>0)
                    {
                      G[compl, nodeS[i]] = 0
                      G[nodeS[i], compl] = 1
                      
                      G_dir[compl, nodeS[i]] = 0
                      G_dir[nodeS[i], compl] = 1
                    }
                    
                    aux = G_dir
                    
                    options(warn=-1)
                    FD = Final_Direction(G, G_dir)
                    options(warn=0)
                    
                    G     = FD$G_ad
                    G_dir = FD$G_dir
                    
                    diffFD = which(aux!=G_dir)
                    
                    if(length(diffFD) > 0)
                    { 
                      print('I did some update with FINAL DIRECTION:')
                      upd = rep(0,n)
                      
                      for( k in 1:length(diffFD))
                      {
                        upd[ floor( (diffFD[k]-1)/n) + 1 ] = 1
                        upd[ (diffFD[k]-1)%%n +1 ] = 1
                        
                        print(floor( (diffFD[k]-1)/n) + 1)
                        print( (diffFD[k]-1)%%n +1 )
                      }
                      
                      upd[vectUndEdgeUi] = 1
                      upd = which(upd==1)
                      
                    }
                    else
                    {
                      upd = vectUndEdgeUi
                    }
                    
                    undiEdge = updateUE(G, c(nodeS[i], upd), undiEdge[[n+2]] ) ## SOME UPDATE SHOULD HAVE HAPPENED DURING Final_Direction???
#                     t = length(vectUndEdgeUi) +1  # ISN'T IT UNUSEFULL?
#                     j = ncol(sub) +1
#                     i = length(nodeS)
                    t = 1
#                     aux = 1
                    
                    K = SecondStage(data = data, G_ad = G, G_dir = G_dir, undiEdge = undiEdge, s, alpha = alpha, sig = sig, p = p, test = test) ## AM I SURE ABOUT THIS???
                    
                    return(K)
#                     break
                  } # end  if( ir = 1)
                  
#                   if(aux==1)
#                   {
#                     aux = 0
#                     break
#                   }
                } # end if( NOTImmort(G, nodeS[i], sub[,j]) == 1)
                j = j+1
                
              } # end while ( j <= ncol(sub) )
            }

          t = t-1
          } # end while ( t > 0 )

      } # end for( i in 1:length(nodeS) )
        
    } # end if(length(nodeS)>0)
    
    s = s+1
    
  }# end while(maxUndiEdge >= s)
# s = s + 1
G


}
