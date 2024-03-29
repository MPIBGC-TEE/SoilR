#' Implementation of a three pool model with feedback structure
#' 
#' This function creates a model for three pools connected with feedback. It is
#' a wrapper for the more general function \code{\link{GeneralModel}}.
#' 
#' 
#' @param t A vector containing the points in time where the solution is
#' sought.
#' @param ks A vector of length 3 containing the values of the decomposition
#' rates for pools 1, 2, and 3.
#' @param a21 A scalar with the value of the transfer rate from pool 1 to pool
#' 2.
#' @param a12 A scalar with the value of the transfer rate from pool 2 to pool
#' 1.
#' @param a32 A scalar with the value of the transfer rate from pool 2 to pool
#' 3.
#' @param a23 A scalar with the value of the transfer rate from pool 3 to pool
#' 2.
#' @param C0 A vector containing the initial concentrations for the 3 pools.
#' The length of this vector is 3
#' @param In A data.frame object specifying the amount of litter inputs by
#' time.
#' @param xi A scalar or data.frame object specifying the external
#' (environmental and/or edaphic) effects on decomposition rates.
#' @param solver A function that solves the system of ODEs. This can be
#' \code{\link{euler}} or \code{\link{deSolve.lsoda.wrapper}} or any other user
#' provided function with the same interface.
#' @param pass if TRUE forces the constructor to create the model even if it is
#' invalid
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
#' In = 60
#' 
#' Temp=rnorm(t,15,1)
#' TempEffect=data.frame(t,fT.Daycent1(Temp))
#' 
#' Ex1=ThreepFeedbackModel(t=t,ks=ks,a21=0.5,a12=0.1,a32=0.2,a23=0.1,C0=C0,In=In,xi=TempEffect)
#' Ct=getC(Ex1)
#' Rt=getReleaseFlux(Ex1)
#' 
#' plot(
#' t,
#' rowSums(Ct),
#' type="l",
#' ylab="Carbon stocks (arbitrary units)",
#' xlab="Time (arbitrary units)",
#' lwd=2,
#' ylim=c(0,sum(Ct[51,]))
#' ) 
#' lines(t,Ct[,1],col=2)
#' lines(t,Ct[,2],col=4)
#' lines(t,Ct[,3],col=3)
#' legend(
#' "topleft",
#' c("Total C","C in pool 1", "C in pool 2","C in pool 3"),
#' lty=c(1,1,1,1),
#' col=c(1,2,4,3),
#' lwd=c(2,1,1,1),
#' bty="n"
#' )
#' 
#' plot(
#' t,
#' rowSums(Rt),
#' type="l",
#' ylab="Carbon released (arbitrary units)",
#' xlab="Time (arbitrary units)",
#' lwd=2,
#' ylim=c(0,sum(Rt[51,]))
#' ) 
#' lines(t,Rt[,1],col=2)
#' lines(t,Rt[,2],col=4)
#' lines(t,Rt[,3],col=3)
#' legend(
#' "topleft",
#' c("Total C release",
#' "C release from pool 1",
#' "C release from pool 2",
#' "C release from pool 3"),
#' lty=c(1,1,1,1),
#' col=c(1,2,4,3),
#' lwd=c(2,1,1,1),
#' bty="n"
#' )
#' 
#' Inr=data.frame(t,Random.inputs=rnorm(length(t),50,10))
#' plot(Inr,type="l")
#' 
#' Ex2=ThreepFeedbackModel(t=t,ks=ks,a21=0.5,a12=0.1,a32=0.2,a23=0.1,C0=C0,In=Inr)
#' Ctr=getC(Ex2)
#' Rtr=getReleaseFlux(Ex2)
#' 
#' plot(
#' t,
#' rowSums(Ctr),
#' type="l",
#' ylab="Carbon stocks (arbitrary units)",
#' xlab="Time (arbitrary units)",
#' lwd=2,
#' ylim=c(0,sum(Ctr[51,]))
#' ) 
#' lines(t,Ctr[,1],col=2)
#' lines(t,Ctr[,2],col=4)
#' lines(t,Ctr[,3],col=3)
#' legend("topright",c("Total C","C in pool 1", "C in pool 2","C in pool 3"),
#' lty=c(1,1,1,1),col=c(1,2,4,3),lwd=c(2,1,1,1),bty="n")
#' 
#' plot(t,rowSums(Rtr),type="l",ylab="Carbon released (arbitrary units)",
#' xlab="Time (arbitrary units)",lwd=2,ylim=c(0,sum(Rtr[51,]))) 
#' lines(t,Rtr[,1],col=2)
#' lines(t,Rtr[,2],col=4)
#' lines(t,Rtr[,3],col=3)
#' legend(
#' "topright",
#' c("Total C release",
#' "C release from pool 1",
#' "C release from pool 2",
#' "C release from pool 3"
#' ),
#' lty=c(1,1,1,1),
#' col=c(1,2,4,3),
#' lwd=c(2,1,1,1),
#' bty="n")
ThreepFeedbackModel<- function 
     (t,      
      ks,	
      a21, 
      a12, 
      a32, 
      a23, 
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
          t_start,
          t_end
      )
      }
      A=-1*abs(diag(ks))
      A[2,1]=a21
      A[1,2]=a12
      A[3,2]=a32
      A[2,3]=a23
      if(length(xi)==1){
        fX=function(t){xi}
        Af=BoundLinDecompOp(function(t){fX(t)*A},t_start,t_end)
      }
      if(inherits(xi, "data.frame")){
        X=xi[,1]
      	Y=xi[,2]
        fX=splinefun(X,Y)
        Af=BoundLinDecompOp(function(t){fX(t)*A},min(X),max(X))
       }
      Mod=GeneralModel(t=t,A=Af,ivList=C0,inputFluxes=inputFluxes,solver,pass)
      return(Mod)
}
