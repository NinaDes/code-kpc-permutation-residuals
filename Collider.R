Collider <- function(G,# skeleton matrix
                     sepstep, # separation set
                     ...){
  n = ncol(G)
  G_ad = matrix(data = 0, nrow = n, ncol = n) # the NEW MATRIX
  coll = list()
  l=1
  
  for(i in 1:n) # for each node i
  {
    ad_i = adiac_D(G, i) # adjacent of node i
    if( length(ad_i) > 1 ) # if node i has more than 1 neib
    {
      for(j in 1:(length(ad_i)-1)) # take every node j adjacent to i
      {
        for(k in (j+1):length(ad_i))# and another one 
        {
          #          if (G[ad_i[j], ad_i[k]]==0 && sum(sepstep[[ ad_i[j] ]][[ ad_i[k] ]]==i)==0) # if ad_i[j] and ad_i[k] are not adjacent and i is not in sepstep[[ ad_i[j] ]][[ ad_i[k] ]] ESISTE ALTRO MRODO DI SUM
          if (G[ad_i[j], ad_i[k]]==0 && !(i%in%sepstep[[ ad_i[j] ]][[ ad_i[k] ]])) # if ad_i[j] and ad_i[k] are not adjacent and i is not in sepstep[[ ad_i[j] ]][[ ad_i[k] ]] ESISTE ALTRO MRODO DI SUM
          {
            G_ad[ad_i[j], i] = 2 # create the collider NUMBER 2!!
            G_ad[ad_i[k], i] = 2
            coll[[l]] = c(ad_i[j], i, ad_i[k])
            l = l+1
          }
        }
      }
    }
  }
  
  G_dir = G_ad # in G_dir there are only the edge with one arrow
  
  for(i in 1:(n-1)) # I enter value in G_ad (there were only collider, I enter the other edge)
  {
    for(j in (i+1):n) 
    {
      if(G[i, j]==1 && G_ad[i, j]==0 && G_ad[j, i]==0) # unuseful G[j, i]==1
      {
        G_ad[i, j] = 1
        G_ad[j, i] = 1
      }
    }
  }
  
  Co = list(G_dir = G_dir, G_ad = G_ad, collider = coll )
  
}

