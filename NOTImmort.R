NOTImmort <- function(G, # adiacency matrix
                      v, # a node
                      s){# a vector
  # I want to check if orienting  s(i) -> v create an immortality
  # I return:
  
  # 1 if it NO ONE s create immortality
  # 0 if there is at least one s creating immortality 
  
  n = length(s)
  
  ent = adiac_S_Enter(G, v)# the nides entering in v
    
#   if(length(ent)==0)  # if there aren't any edge entering in v, I surely don't create an immortality
#   {
#     return(1)
#   }
#   
  allent = c(s, ent)
  
  if(length(s) > 1)
  {
    for ( i in 1:(length(s)-1))  # for each element to be tested
    {
      for ( j in (i+1):length(allent)) # for each element entering in v       # CAN BE BETTER
      {
        if( G[s[i], allent[j]] == 0 & G[allent[j], s[i]] == 0 )  # if at least one of this edge is NOT connected to s, there is an immortality
        {
          return(0)
        }
      }
    }
    return (1)
  }


  if(length(s)==1 && length(ent)==0)
  {
    return(1)
  }


  if(length(s)==1 && length(ent)>0)
  {
    for ( j in (2):length(allent)) # for each element entering in v       # CAN BE BETTER
    {
      if( G[s[1], allent[j]] == 0 & G[allent[j], s[1]] == 0 )  # if at least one of this edge is NOT connected to s, there is an immortality
      {
        return(0)
      }
    }
    
    return(1)
  }
  
  
}
  