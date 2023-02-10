#' Collect C and C14 from a VTLM object aggregated by pool
#' 
#' This function collects the amount of C and C14 and aggregates the information by pool across layers.
#' It computes a weighted average of radiocarbon aggregated by the amount of C in the layer.
#' 
#' @param Ct The amount of C per pool and layer as obtained by a call of getC to the VTLM model
#' @param P14t The radiocarbon signature per pool and layer as obtained by a call of getF14 to the VTLM model
#' @param nlayer Number of layers
#' @param npool Number of pools

collect_by_pool<-function(Ct,P14t, nlayer, npool){
  AFM=AbsoluteFractionModern_from_Delta14C(P14t)
  collect<-matrix(1, ncol=1, nrow=nlayer)%x%diag(1,nrow=npool,ncol=npool)
  Cp<-Ct%*%collect
  AFM_Ct<-AFM*Ct
  F14t_p<-(AFM_Ct%*%collect)/Cp
  return(list(F14C=Delta14C_from_AbsoluteFractionModern(F14t_p), C=Cp))
}
