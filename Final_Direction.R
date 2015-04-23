Final_Direction <- function(G_ad,# NON FINISHED adjacency matrix
                            G_dir, # direction matrix
                            ...){
  
  n = nrow(G_ad)
  change = 0
  while(change!=1) # while some changing in the structure are possible
  {
    H=G_ad
    for(i in 1:n) # for each node i
    {
      ad_ex_i = adiac_S_Exit_path(H, i) # adjacet such as i-> j #### FORSE è MEGLIO G_ad???
      
      # ad_ex_i = adiac_S_Exit(G_ad, i) # adjacet such as i-> j
      # ad_ex_i = adiac_S_Exit(G_dir, i) # adjacet such as i-> j
      
      if(length(ad_ex_i)>0) # if i has adj such as i-> j
      {
        for(j in 1:length(ad_ex_i))
        {
          # ad_d_j = adiac_D(G_ad, ad_ex_i[j]) # adjacent such as j - k
          ad_d_j = adiac_D(H, ad_ex_i[j]) # adjacent such as j - k
          
          if( length(ad_d_j) > 0) # if j as adj such as j - k
          {
            for(k in 1:length(ad_d_j))
            {
              #if(G_ad[i, ad_d_j[k]]==0 && G_ad[ad_d_j[k], i]==0) # if there are no edge between i and ad_d_j[k]
              if(H[i, ad_d_j[k]]==0 && H[ad_d_j[k], i]==0) # if there are no edge between i and ad_d_j[k]
                
              {
                G_ad[ad_d_j[k], ad_ex_i[j]] = 0  # it become i-> j -> k
                G_ad[ad_ex_i[j], ad_d_j[k]] = 1  # it become i-> j -> k
                
                G_dir[ad_ex_i[j], ad_d_j[k]] = 1 # an arrow more
                G_dir[ad_d_j[k], ad_ex_i[j]] = 0 # an arrow more
                
              }
            }
          }
          
        }
      }
      
    }    
    for(i in 1:n) # for each node i
    {
      ad_d = adiac_D(G_ad, i) # adj of i: i - j
      
      if(length(ad_d)>0)
      {
        for(j in 1:length(ad_d)) # for each ad_d
        {
          if(path_N(G_dir, i, ad_d[j])!=0) # if there is a directed path between i and ad_d[j] 
          {
            G_dir[i, ad_d[j]] = 1 # I orient i->ad_d[j]
            G_ad[ad_d[j], i] = 0
          }
        }
      }
    }
    
    
    change = all(H==G_ad)  
  }
  
  list(G_ad = G_ad, G_dir = G_dir)
 
  
}