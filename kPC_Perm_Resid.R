kPC_Perm_Resid <- function(data,  # the available data
                           alpha, # level of the test
                           sig,   # gaussian kernel width
                           numcol,# number of column in incomplete chol decomposition 
                           eps,   # regularisation parameter
                           p,     # number of permutation in perm test of hsic
                           test,  # type of test in hsic (1 = perm, 2 = gamma) 
                           paral){# 1 if I don't want to parallelize or a number n >1 to parallelize with n core    
  
  if(test > 2)
  {
    print('YOU MUST CHOOSE OR test = 1 FOR Permutation test OR test = 2 FOR Gamma test')
    return
  }
  
  
  source('Source_kPC_Perm_Resid.R')
  Source_kPC_Perm_Resid()
  
  # BE CAREFULL
  # G      adjac matr with all the doublearrow edge   for step A B C
  # G_dir  adjac matrix with only the directed edge   for step D, I create it at the end of step C
  # G_ad   adjac matrix: the RIGHT ONE
  # H      
  
  ## STEP A and B
  
  Sk      = Skeleton_P_Res(data, alpha, sig, numcol, eps, p, test, paral )
  sepstep = Sk$sepstep
  G       = Sk$G  
  pval    = Sk$pval 
  
  ## STEP C 
  
  Co = Collider(G, sepstep)
  
  G_dir = Co$G_dir
  G_ad  = Co$G_ad   
  Coll  = Co$collider
  
  
  
  ##STEP D
  undiEdge = updateUE(G_ad, NULL, NULL)
  s = 1
  
  SecSta  = SecondStage(data, G_ad, G_dir, undiEdge, s, alpha, sig, p, test)
  
  Adjacency_Matrix = SecSta
  
  
  par(mfrow=c(3,1))
  plot( as(G,'graphNEL'), main='Skeleton Paper' )
  plot( as(G_ad,'graphNEL'), main='Collider' )
  plot( as(Adjacency_Matrix,'graphNEL'), main='Final graph' )
  
  list(mat = Adjacency_Matrix, pval = pval, Coll = Coll, Gskel = G, Gcoll = G_ad)
  
  
  
  
#     ##STEP D
#     options(warn = -1)
#     FD = Final_Direction(G_ad, G_dir)
#     options(warn = 0)
#     Adjacency_Matrix = FD$G_ad
#     
#     list(mat = Adjacency_Matrix, pval = pval)
} 
