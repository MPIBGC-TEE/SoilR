#' Vertical Transfer Linear Model
#' 
#' This function implements a general model for vertical transfer of carbon and
#' radiocarbon along a vertical profile. It combines a SoilR model of any number of pools
#' with a vertical profile with any number of layers. It assumes the pool model
#' at steady state, but can predict the non-equilibrium dynamics of radiocarbon.


VTLM<-function( 
    Model=DepthModel,
    lyrs=c(1:3),
    latIn=rep(0,3),
    u=0.01,
    d=0.05
  ){
  
  nlyrs<-length(lyrs)
  P<-Model@mat@mat # Compartmental matrix from Pool model
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
  
  In<-matrix(c(Model@inputFluxes@map, rep(0,(nlyrs*npools)-npools)), ncol=1)
  
  xss<-as.numeric(solve(-M)%*%In)
  ivF14<-ConstFc(rep(0,npools*nlyrs),"Delta14C")
  
  
  SoilRModel<-Model_14(t=Model@times, A=M, ivList=xss, initialValF=ivF14, 
                       inputFluxes=In, inputFc=Model@c14Fraction)
  return(SoilRModel)
}