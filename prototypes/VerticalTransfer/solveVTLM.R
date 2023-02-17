solveVTLM<-function(VTLM, npool, nlayer){
  dm<-dim(VTLM@mat@mat)
  if(dm[1] != npool*nlayer) stop("Number of pools and layers must correspond to dimension of LVTM model")
  
  Ct<-getC(VTLM)
  C14t<-getF14(VTLM)
  ntime<-nrow(Ct)
  
  arrayC<-array(dim=c(nlayer, npool, ntime))
  arrayC14<-array(dim=c(nlayer, npool, ntime))
  for(i in 1:ntime){
    arrayC[,,i]<-matrix(Ct[i,], ncol=npool, nrow=nlayer, byrow=TRUE)
    arrayC14[,,i]<-matrix(C14t[i,], ncol=npool, nrow=nlayer, byrow=TRUE)
  }
  return(list(C=arrayC, C14=arrayC14))
}
