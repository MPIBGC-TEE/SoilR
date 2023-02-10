#' Collect C and C14 from a VTLM object aggregated by layer
#' 
#' This function collects the amount of C and C14 and aggregates the information by depth layer.
#' It computes a weighted average of radiocarbon aggregated by the amount of C in the pools.
#' 
#' @param Ct The amount of C per pool and layer as obtained by a call of getC to the VTLM model
#' @param P14t The radiocarbon signature per pool and layer as obtained by a call of getF14 to the VTLM model
#' @param nlayer Number of layers
#' @param npool Number of pools

collect_by_layer<-function(Ct,P14t, nlayer, npool){
  AFM=AbsoluteFractionModern_from_Delta14C(P14t)
  collect<-diag(nrow=nlayer,ncol=nlayer)%x%matrix(c(1,1),nrow=npool,ncol=1) 
  Cl<-Ct%*%collect
  AFM_Ct<-AFM*Ct
  F14t_l<-(AFM_Ct%*%collect)/Cl
  return(list(F14C=Delta14C_from_AbsoluteFractionModern(F14t_l), C=Cl))
}
