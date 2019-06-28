#' Implementation of the Century model
#' 
#' This function implements the Century model as described in Parton et al.
#' (1987).
#' 
#' 
#' @param t A vector containing the points in time where the solution is
#' sought.
#' @param ks A vector of lenght 5 containing the values of the decomposition
#' rates for the different pools. Units in per week.
#' @param C0 A vector of length 5 containing the initial amount of carbon for
#' the 5 pools.
#' @param In A scalar or data.frame object specifying the amount of litter
#' inputs by time (mass per area per week).
#' @param LN A scalar representing the lignin to nitrogen ratio of the plant
#' residue inputs.
#' @param Ls A scalar representing the fraction of structural material that is
#' lignin.
#' @param clay Proportion of clay in mineral soil.
#' @param silt Proportion of silt in mineral soil.
#' @param xi A scalar or data.frame object specifying the external
#' (environmental and/or edaphic) effects on decomposition rates.
#' @param solver A function that solves the system of ODEs. This can be
#' \code{\link{euler}} or \code{\link{deSolve.lsoda.wrapper}} or any other user
#' provided function with the same interface.
#' @return A Model Object that can be further queried
#' @seealso \code{\link{RothCModel}}. There are other
#' \code{\link{predefinedModels}} and also more general functions like
#' \code{\link{Model}}.
#' @references Parton, W.J, D.S. Schimel, C.V. Cole, and D.S. Ojima. 1987.
#' Analysis of factors controlling soil organic matter levels in Great Plain
#' grasslands. Soil Science Society of America Journal 51: 1173--1179. Sierra,
#' C.A., M. Mueller, S.E. Trumbore. 2012. Models of soil organic matter
#' decomposition: the SoilR package version 1.0. Geoscientific Model
#' Development 5, 1045-1060.
#' @examples
#' t=seq(0,52*200,1) #200 years  
#' LNcorn=0.17/0.004 # Values for corn clover reported in Parton et al. 1987
#' Ex=CenturyModel(t,LN=0.5,Ls=0.1,In=0.1)
#' Ct=getC(Ex)
#' Rt=getReleaseFlux(Ex)
#' 
#' matplot(t,Ct,type="l", col=1:5,lty=1,ylim=c(0,max(Ct)*2.5),
#' ylab=expression(paste("Carbon stores (kg C", ha^-1,")")),xlab="Time (weeks)") 
#' lines(t,rowSums(Ct),lwd=2)
#' legend("topright", c("Structural litter","Metabolic litter",
#' "Active SOM","Slow SOM","Passive SOM","Total Carbon"),
#' lty=1,lwd=c(rep(1,5),2),col=c(1:5,1),bty="n")
#' 
#' matplot(t,Rt,type="l",lty=1,ylim=c(0,max(Rt)*3),ylab="Respiration (kg C ha-1 week-1)",xlab="Time") 
#' lines(t,rowSums(Rt),lwd=2) 
#' legend("topright", c("Structural litter","Metabolic litter",
#' "Active SOM","Slow SOM","Passive SOM","Total Respiration"),
#' lty=1,lwd=c(rep(1,5),2),col=c(1:5,1),bty="n")
CenturyModel<- function 
  (t,      
   ks=c(k.STR=0.094,k.MET=0.35,k.ACT=0.14,k.SLW=0.0038,k.PAS=0.00013),  
   C0=c(0,0,0,0,0),	
   In,    
   LN, 
   Ls, 
   clay=0.2, 
   silt=0.45, 
   xi=1,  
   solver=deSolve.lsoda.wrapper  
  )	
  { 
    t_start=min(t)
    t_end=max(t)
    if(length(ks)!=5) stop("ks must be of length = 5")
    if(length(C0)!=5) stop("the vector with initial conditions must be of length = 5")
    Fm=0.85-0.18*LN
    Fs=1-Fm
    if(length(In)==1){
      inputFluxes=BoundInFlux(
        function(t){matrix(nrow=5,ncol=1,c(In*Fm,In*Fs,0,0,0))},
        t_start,
        t_end
      )
    }
    if(class(In)=="data.frame"){
      x=In[,1]  
      y=In[,2]  
      inputFlux=splinefun(x,y)
      inputFluxes=BoundInFlux(
        function(t){matrix(nrow=5,ncol=1,c(inputFlux(t)*Fm,inputFlux(t)*Fs,0,0,0))},
        min(x),
        max(x)
      )
    }
    Txtr=clay+silt
    fTxtr=1-0.75*Txtr
    Es=0.85-0.68*Txtr
    alpha31=0.45
    alpha32=0.55
    alpha34=0.42
    alpha35=0.45
    alpha41=0.3 
    alpha53=0.004
    alpha43=1-Es-alpha53
    alpha54=0.03
    A=-1*diag(abs(ks))
    A[1,1]=A[1,1]*exp(-3*Ls)
    A[3,3]=A[3,3]*fTxtr
    A[3,1]=alpha31*abs(A[1,1])
    A[3,2]=alpha32*abs(A[2,2])
    A[3,4]=alpha34*abs(A[4,4])
    A[3,5]=alpha35*abs(A[5,5])
    A[4,1]=alpha41*abs(A[1,1])
    A[4,3]=alpha43*abs(A[3,3])
    A[5,3]=alpha53*abs(A[3,3])
    A[5,4]=alpha54*abs(A[4,4])
    if(length(xi)==1) fX=function(t){xi}
    if(class(xi)=="data.frame"){
      X=xi[,1]
      Y=xi[,2]
      fX=splinefun(X,Y)
    }
    Af=BoundLinDecompOp(
      function(t){fX(t)*A},
      t_start,
      t_end
    )
    Mod=GeneralModel(t=t,A=Af,ivList=C0,inputFluxes=inputFluxes,solverfunc=solver,pass=FALSE)
    return(Mod)
  }
