indipResid <- function(data,        # the data
                       G,           # adj matrix
                       undiEdge,    # list with the undir edge
                       v,           # the node I'm investigating
                       parent,      # parents of this node v
                       S,           # some ( not always all ) nodes to which v is undirected connected
                       alpha,       # test level
                       sig,         # gaussian width kernel
                       p,           # number of permutatio
                       test){       # which test to do in hsic (1 = permut, 2 = gamma)
  
    
  regr = data.frame(data[, c(v, parent, S)])
  nc = ncol(regr)
  colnames(regr) = paste("x",1:nc,sep="")
  form = create_formula(1, 2:nc, "x")
  
  res1  = gam(form, dat = regr)$residuals
  
  
  dep1 = HSIC(X = res1, Y = data[, S], sig = sig, p = p, test = test)
  print(paste0('The first p value is', dep1,'   (I return 0 is dep1<alpha. To create the arrow I want them indep)'))
  
  if (dep1 < alpha) 
  {
    return(0)
  }
  
  for( i in 1:length(S) ) # for every node to which v is undirectly connected
  {
    Sj = undiEdge[[ S[i] ]] # all the undirected edge connected to S[i]
    Sj = Sj[ -c(which(Sj==v)) ]
    
    
    parentJ = adiac_S_Enter(G, S[i])
    
    
    # I test the null set
    
    regr = data.frame(data[, c(S[i], parentJ, v)])
    nc = ncol(regr)
    colnames(regr) = paste("x",1:nc,sep="")
    form = create_formula(1, 2:nc, "x")
    
    res2 = gam(form, dat = regr)$residuals
    
    dep2 = HSIC(X = res2, Y = data[, v ], sig = sig, p = p, test = test)
    print(paste0('The second is',dep2,'   I return 0 if dep2>alpha'))
    
    if(dep2 > alpha)
    {
      return(0)
    }
    
    if(length(Sj)>0)
    {
      for(j in 1:length(Sj) ) # faccio le varie combinazioni dei diversi sottinsiemi
      {        
        if( length(Sj) ==1)
        {
          sub = Sj
          sub = as.matrix(sub)
        }
        else
        {
          sub = combn(Sj ,j)
        }
        
        for( k in 1:ncol(sub))
        {
          regr = data.frame(data[, c(S[i], parentJ, sub[k], v)])
          nc = ncol(regr)
          colnames(regr) = paste("x",1:nc,sep="")
          form = create_formula(1, 2:nc, "x")
          
          res2 = gam(form, dat = regr)$residuals
        
          dep2 = HSIC(X = res2, Y = data[, c(v, sub[,k]) ], sig = sig, p = p, test = test)
          print(paste0('The second is',dep2,'   I return 0 if dep2>alpha'))
          
          if(dep2 > alpha)
          {
            return(0)
          }
          
        }
        
      }
    }
    

     
      
    
    
  }
  return(1)
  
}