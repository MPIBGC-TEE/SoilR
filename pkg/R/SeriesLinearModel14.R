#' General m-pool linear C14 model with series structure
#' 
#' This function creates a radiocarbon model for m number of pools connected in
#' series. It is a wrapper for the more general function
#' \code{\link{GeneralModel_14}}.
#' 
#' 
#' @param t A vector containing the points in time where the solution is
#' sought.
#' @param m.pools An integer with the total number of pools in the model.
#' @param ki A vector of length m containing the values of the decomposition
#' rates for each pool i.
#' @param Tij A vector of length m-1 with the transfer coefficients from pool j
#' to pool i. The value of these coefficients must be in the range [0, 1].
#' @param C0 A vector of length m containing the initial amount of carbon for
#' the m pools.
#' @param F0_Delta14C A vector of length m containing the initial amount of the
#' radiocarbon fraction for the m pools.
#' @param In A scalar or data.frame object specifying the amount of litter
#' inputs by time.
#' @param xi A scalar or data.frame object specifying the external
#' (environmental and/or edaphic) effects on decomposition rates.
#' @param inputFc A Data Frame object containing values of atmospheric Delta14C
#' per time. First column must be time values, second column must be Delta14C
#' values in per mil.
#' @param lambda Radioactive decay constant. By default lambda=-0.0001209681
#' y^-1 . This has the side effect that all your time related data are treated
#' as if the time unit was year.
#' @param lag A positive scalar representing a time lag for radiocarbon to
#' enter the system.
#' @param solver A function that solves the system of ODEs. This can be
#' \code{\link{euler}} or \code{\link{deSolve.lsoda.wrapper}} or any other user
#' provided function with the same interface.
#' @param pass if TRUE Forces the constructor to create the model even if it is
#' invalid
#' @seealso There are other \code{\link{predefinedModels}} and also more
#' general functions like \code{\link{Model}}.
#' @references Sierra, C.A., M. Mueller, S.E. Trumbore. 2014. Modeling
#' radiocarbon dynamics in soils: SoilR version 1.1. Geoscientific Model
#' Development 7, 1919-1931.
#' @examples
#' years=seq(1901,2009,by=0.5)
#' LitterInput=700 
#' 
#' Ex=SeriesLinearModel14(
#' t=years,ki=c(k1=1/2.8, k2=1/35, k3=1/100), m.pools=3,
#' C0=c(200,5000,500), F0_Delta14C=c(0,0,0),
#' In=LitterInput, Tij=c(0.5, 0.1),inputFc=C14Atm_NH
#' )
#' R14m=getF14R(Ex)
#' C14m=getF14C(Ex)
#' C14t=getF14(Ex)
#' 
#' par(mfrow=c(2,1))
#' plot(C14Atm_NH,type="l",xlab="Year",
#' ylab="Delta 14C (per mil)",xlim=c(1940,2010)) 
#' lines(years, C14t[,1], col=4)
#' lines(years, C14t[,2],col=4,lwd=2)
#' lines(years, C14t[,3],col=4,lwd=3)
#' legend(
#' "topright",
#' c("Delta 14C Atmosphere", "Delta 14C pool 1", "Delta 14C pool 2", "Delta 14C pool 3"),
#' lty=rep(1,4),col=c(1,4,4,4),lwd=c(1,1,2,3),bty="n")
#' 
#' plot(C14Atm_NH,type="l",xlab="Year",ylab="Delta 14C (per mil)",xlim=c(1940,2010)) 
#' lines(years,C14m,col=4)
#' lines(years,R14m,col=2)
#' legend("topright",c("Delta 14C Atmosphere","Delta 14C SOM", "Delta 14C Respired"),
#' lty=c(1,1,1), col=c(1,4,2),bty="n")
#' par(mfrow=c(1,1))
SeriesLinearModel14<- function 
  (t,      
   m.pools, 
   ki,	
   Tij, 
   C0,	
   F0_Delta14C,  
   In,    
   xi=1,  
   inputFc,
   lambda=-0.0001209681, 
   lag=0, 
   solver=deSolve.lsoda.wrapper,  
   pass=FALSE  
  )	
  { 
    t_start=min(t)
    t_end=max(t)
    if(length(ki)!=m.pools) stop("ki must be of length = m.pools")
    if(length(C0)!=m.pools) stop("the vector with initial conditions must be of length = m.pools")
    if(length(In)==1){
      inputFluxes=BoundInFluxes(
        function(t){matrix(nrow=m.pools,ncol=1,c(In,rep(0,m.pools-1)))},
        t_start,
        t_end
      )
    }
    if(inherits(In, "data.frame")){
      x=In[,1]  
      y=In[,2]  
      inputFlux=splinefun(x,y)
      inputFluxes=BoundInFluxes(
        function(t){matrix(nrow=m.pools,ncol=1,c(inputFlux(t),rep(0,m.pools-1)))},
        min(x),
        max(x)
      )
    }
    A=-1*abs(diag(ki))
    a=abs(ki[-length(ki)])*Tij
    ij=matrix(c((2:m.pools),(1:(m.pools-1))),ncol=2)
    A[ij]=a
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
    Fc=BoundFc(inputFc,lag=lag,format="Delta14C")
    Mod=GeneralModel_14(t=t,A=Af,ivList=C0,initialValF=ConstFc(F0_Delta14C,"Delta14C"),inputFluxes=inputFluxes,inputFc=Fc,di=lambda,pass=pass)
    return(Mod)
  }
