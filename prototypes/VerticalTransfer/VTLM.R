#' Vertical Transfer Linear Model
#' 
#' This function implements a general model for vertical transfer of carbon and
#' radiocarbon along a vertical profile. It combines a SoilR model of any number of pools
#' with a vertical profile with any number of layers. It assumes the pool model
#' at steady state, but can predict the non-equilibrium dynamics of radiocarbon.
#' 
#' @param Model A SoilR model of class Model_14
#' @param lyrs A vector with the depth layers to simulate
#' @param latIn A vector with the later inputs for the depth intervals. Must be of same size as lyrs
#' @param d A scalar value with the downward transport rate
#' @param u A scalar value with the upward transport rate
#' 
VTLM<-function( 
    Model,
    lyrs,
    latIn,
    d,
    u
  ){
  
  nlyrs<-length(lyrs)
  if(inherits(Model@mat, "ConstLinDecompOp")){
   P<-Model@mat@mat # Compartmental matrix from Pool model
  } else if(inherits(Model@mat, "BoundLinDecompOp")){
    P<-Model@mat@map(Inf) # Compartmental matrix from Pool model
   }
  else stop("Only SoilR models of class ConstLinDecompOp or BoundLinDecompOp are allowed")
  
  npools<-dim(P)[1]
  L<-diag(1,nlyrs,nlyrs) # Identity matrix representing the layers
  
  D<-L%x%P # Decomposition matrix with decomposition rates for the pools at each layer
  
  H<-diag(-d, nlyrs)
  H[cbind(c(2:nlyrs),c(1:(nlyrs-1)))]<-d # Matrix with vertical downward transport rates
  h<-diag(1, npools) # Expansion matrix by number of pools
  
  G<-diag(-u, nlyrs)
  G[1]<-0 # Remove first element so no vertical transport occurs upwards
  G[cbind(c(1:(nlyrs-1)),c(2:nlyrs))]<-u # Matrix with upward mixing rates
  
  V<-(H%x%h)+(G%x%h) # Vertical transfer matrix with downwards and upwards rates
  
  M<-D+V
  
  In<-matrix(c(Model@inputFluxes@map(Inf), rep(0,(nlyrs*npools)-npools)), ncol=1) + latIn
  
  xss<-as.numeric(solve(-M)%*%In)
  A<-systemAge(A=M,u=In)$meanPoolAge
  ivF14<-ConstFc(as.numeric(Delta14C_from_AbsoluteFractionModern(exp(A/-8033))), "Delta14C")

  
  SoilRModel<-Model_14(t=Model@times, A=M, ivList=xss, initialValF=ivF14, 
                       inputFluxes=as.numeric(In), inputFc=Model@c14Fraction)
  return(SoilRModel)
}
