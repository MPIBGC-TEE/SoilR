#' Implementation of a three pool model with series structure
#' 
#' This function creates a model for three pools connected in series. It is a
#' wrapper for the more general function \code{\link{GeneralModel}}.
#' 
#' 
#' @param t A vector containing the points in time where the solution is
#' sought.
#' @param ks A vector of length 3 containing the values of the decomposition
#' rates for pools 1, 2, and 3.
#' @param a21 A scalar with the value of the transfer rate from pool 1 to pool
#' 2.
#' @param a32 A scalar with the value of the transfer rate from pool 2 to pool
#' 3.
#' @param C0 A vector of length 3 containing the initial amount of carbon for
#' the 3 pools.
#' @param In A scalar or data.frame object specifying the amount of litter
#' inputs by time.
#' @param xi A scalar or data.frame object specifying the external
#' (environmental and/or edaphic) effects on decomposition rates.
#' @param solver A function that solves the system of ODEs. This can be
#' \code{\link{euler}} or \code{\link{deSolve.lsoda.wrapper}} or any other user
#' provided function with the same interface.
#' @param pass if TRUE Forces the constructor to create the model even if it is
#' invalid
#' @return A Model Object that can be further queried
#' @seealso There are other \code{\link{predefinedModels}} and also more
#' general functions like \code{\link{Model}}.
#' @references Sierra, C.A., M. Mueller, S.E. Trumbore. 2012. Models of soil
#' organic matter decomposition: the SoilR package version 1.0. Geoscientific
#' Model Development 5, 1045-1060.
#' @examples
#' t_start=0 
#' t_end=10 
#' tn=50
#' timestep=(t_end-t_start)/tn 
#' t=seq(t_start,t_end,timestep) 
#' ks=c(k1=0.8,k2=0.4,k3=0.2)
#' C0=c(C10=100,C20=150, C30=50)
#' In = 50
#' 
#' Ex1=ThreepSeriesModel(t=t,ks=ks,a21=0.5,a32=0.2,C0=C0,In=In,xi=fT.Q10(15))
#' Ct=getC(Ex1)
#' Rt=getReleaseFlux(Ex1)
#' 
#' plot(t,rowSums(Ct),type="l",ylab="Carbon stocks (arbitrary units)",
#' xlab="Time (arbitrary units)",lwd=2,ylim=c(0,sum(Ct[1,]))) 
#' lines(t,Ct[,1],col=2)
#' lines(t,Ct[,2],col=4)
#' lines(t,Ct[,3],col=3)
#' legend("topright",c("Total C","C in pool 1", "C in pool 2","C in pool 3"),
#' lty=c(1,1,1,1),col=c(1,2,4,3),lwd=c(2,1,1,1),bty="n")
ThreepSeriesModel<- function 
     (t,    	
      ks,	
      a21, 
      a32, 
      C0,	
      In,    
      xi=1,  
      solver=deSolve.lsoda.wrapper,  
      pass=FALSE  
    )	
    { 
      t_start=min(t)
      t_end=max(t)
      if(length(ks)!=3) stop("ks must be of length = 3")
      if(length(C0)!=3) stop("the vector with initial conditions must be of length = 3")
      if(length(In)==1){
          inputFluxes=BoundInFluxes(
            function(t){matrix(nrow=3,ncol=1,c(In,0,0))},
            t_start,
            t_end
        )
      }
      if(inherits(In, "data.frame")){
         x=In[,1]  
         y=In[,2]  
         inputFlux=splinefun(x,y)
          inputFluxes=BoundInFluxes(
            function(t){matrix(nrow=3,ncol=1,c(inputFlux(t),0,0))},
            min(x),
            max(x)
          )
        }
      A=-1*abs(diag(ks))
      A[2,1]=a21
      A[3,2]=a32
      if(length(xi)==1) fX=function(t){xi}
      if(inherits(xi, "data.frame")){
        X=xi[,1]
        Y=xi[,2]
        fX=splinefun(X,Y)
       }
      Af=BoundLinDecompOp(
            function(t){fX(t)*A},
            t_start,
            t_end
      )
      Mod=GeneralModel(t=t,A=Af,ivList=C0,inputFluxes=inputFluxes,pass=pass)
     return(Mod)
}
