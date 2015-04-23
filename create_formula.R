
create_formula <- function(target.ind,preds.inds,var.str="x") 
  {
  
  preds <- paste(paste(var.str,preds.inds,sep=""),collapse=",")
  as.formula(paste(c(var.str,target.ind," ~ s(",preds,")"),collapse=""))
}


