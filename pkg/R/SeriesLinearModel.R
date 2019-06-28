#' General m-pool linear model with series structure
#' 
#' This function creates a model for m number of pools connected in series. It
#' is a wrapper for the more general function \code{\link{GeneralModel}}.
#' 
#' 
#' @param t A vector containing the points in time where the solution is
#' sought.
#' @param m.pools An integer with the total number of pools in the model.
#' @param ki A vector of lenght m containing the values of the decomposition
#' rates for each pool i.
#' @param Tij A vector of length m-1 with the transfer coefficients from pool j
#' to pool i. The value of these coefficients must be in the range [0, 1].
#' @param C0 A vector of length m containing the initial amount of carbon for
#' the m pools.
#' @param In A scalar or data.frame object specifying the amount of litter
#' inputs by time.
#' @param xi A scalar or data.frame object specifying the external
#' (environmental and/or edaphic) effects on decomposition rates.
#' @param solver A function that solves the system of ODEs. This can be
#' \code{\link{euler}} or \code{\link{deSolve.lsoda.wrapper}} or any other user
#' provided function with the same interface.
#' @param pass if TRUE Forces the constructor to create the model even if it is
#' invalid
#' @seealso There are other \code{\link{predefinedModels}} and also more
#' general functions like \code{\link{Model}}.
#' @references Sierra, C.A., M. Mueller, S.E. Trumbore. 2012. Models of soil
#' organic matter decomposition: the SoilR package version 1.0. Geoscientific
#' Model Development 5, 1045-1060.
#' @examples
#' #A five-pool model
#' t_start=0 
#' t_end=10 
#' tn=50
#' timestep=(t_end-t_start)/tn 
#' t=seq(t_start,t_end,timestep) 
#' ks=c(k1=0.8,k2=0.4,k3=0.2, k4=0.1,k5=0.05)
#' Ts=c(0.5,0.2,0.2,0.1)
#' C0=c(C10=100,C20=150, C30=50, C40=50, C50=10)
#' In = 50
#' #
#' Ex1=SeriesLinearModel(t=t,m.pools=5,ki=ks,Tij=Ts,C0=C0,In=In,xi=fT.Q10(15))
#' Ct=getC(Ex1)
#' #
#' matplot(t,Ct,type="l",col=2:6,lty=1,ylim=c(0,sum(C0)))
#' lines(t,rowSums(Ct),lwd=2)
#' legend("topright",c("Total C","C in pool 1", "C in pool 2","C in pool 3",
#' "C in pool 4","C in pool 5"),
#' lty=1,col=1:6,lwd=c(2,rep(1,5)),bty="n")
SeriesLinearModel<- function 
     (t,    	
      m.pools, 
      ki,	
      Tij, 
      C0,	
      In,    
      xi=1,  
      solver=deSolve.lsoda.wrapper,  
      pass=FALSE  
    )	
    { 
      t_start=min(t)
      t_end=max(t)
      if(length(ki)!=m.pools) stop("ki must be of length = m.pools")
      if(length(C0)!=m.pools) stop("the vector with initial conditions must be of length = m.pools")
      if(length(In)==1){
          inputFluxes=BoundInFlux(
            function(t){matrix(nrow=m.pools,ncol=1,c(In,rep(0,m.pools-1)))},
            t_start,
            t_end
        )
      }
      if(class(In)=="data.frame"){
         x=In[,1]  
         y=In[,2]  
         inputFlux=splinefun(x,y)
          inputFluxes=BoundInFlux(
            function(t){matrix(nrow=m.pools,ncol=1,c(inputFlux(t),rep(0,m.pools-1)))},
            min(x),
            max(x)
          )
        }
      A=-1*abs(diag(ki))
      a=abs(ki[-length(ki)])*Tij
      ij=matrix(c((2:m.pools),(1:(m.pools-1))),ncol=2)
      A[ij]=a
      if(length(xi)==1) {
        fX=function(t){xi}
        tAs=t_start
        tAe=t_end
      }
      if(class(xi)=="data.frame"){
        X=xi[,1]
        Y=xi[,2]
        fX=splinefun(X,Y)
        tAs=min(X)
        tAe=max(X)
       }
      Af=BoundLinDecompOp(
            function(t){fX(t)*A},
            tAs,
            tAe
      )
      Mod=GeneralModel(t=t,A=Af,ivList=C0,inputFluxes=inputFluxes,pass=pass)
     return(Mod)
}
