#
# vim:set ff=unix expandtab ts=2 sw=2:
RothCModel<- function #Implementation of the RothCModel
    ### This function implements the RothC model of Jenkinson et al. It is a wrapper for the more general function \code{\link{GeneralModel}}.
    ##references<< Jenkinson, D. S., S. P. S. Andrew, J. M. Lynch, M. J. Goss, and P. B. Tinker. 1990. The Turnover of Organic Carbon and Nitrogen in Soil. 
    ##Philosophical Transactions: Biological Sciences 329:361-368.
    ##Sierra, C.A., M. Mueller, S.E. Trumbore. 2012. Models of soil organic matter decomposition: the SoilR package version 1.0. Geoscientific Model Development 5, 1045-1060.
    (t,      ##<< A vector containing the points in time where the solution is sought.
      ks=c(k.DPM=10,k.RPM=0.3,k.BIO=0.66,k.HUM=0.02,k.IOM=0),	##<< A vector of lenght 5 containing the values of the decomposition rates for the different pools
      C0=c(0,0,0,0,2.7),	##<< A vector of length 5 containing the initial amount of carbon for the 5 pools.
      In=1.7,    ##<< A scalar or data.frame object specifying the amount of litter inputs by time. 
      FYM=0,  ##<< A scalar or data.frame object specifying the amount of Farm Yard Manure inputs by time. 
      DR=1.44, ##<< A scalar representing the ratio of decomposable plant material to resistant plant material (DPM/RPM).
      clay=23.4, ##<< Percent clay in mineral soil. 
      xi=1,  ##<< A scalar or data.frame object specifying the external (environmental and/or edaphic) effects on decomposition rates.
      solver=deSolve.lsoda.wrapper,  ##<< A function that solves the system of ODEs. This can be \code{\link{euler}} or \code{\link{deSolve.lsoda.wrapper}} or any other user provided function with the same interface.
      pass=FALSE  ##<< if TRUE forces the constructor to create the model even if it is invalid 
    )	
    { 
      t_start=min(t)
      t_end=max(t)
      if(length(ks)!=5) stop("ks must be of length = 5")
      if(length(C0)!=5) stop("the vector with initial conditions must be of length = 5")
      if(class(In)!=class(FYM)) stop("Inputs In and FYM must be of the same class, either scalars or data.frame")
      
      if(length(In)==1){
          inputFluxes=BoundInFlux(
            function(t){matrix(nrow=5,ncol=1,c(In*(DR/(DR+1))+(FYM*0.49),In*(1/(DR+1))+(FYM*0.49),0,(FYM*0.02),0))},
            t_start,
            t_end
        )
      }
      if(class(In)=="data.frame"){
#          x=In[,1]  
#          y=In[,2]  
         inputFlux=splinefun(In[,1],In[,2])
         FYMflux=splinefun(FYM[,1],FYM[,2])
          inputFluxes=BoundInFlux(
            function(t){matrix(nrow=5,ncol=1,c(inputFlux(t)*(DR/(DR+1))+(FYMflux(t)*0.49),inputFlux(t)*(1/(DR+1))+(FYMflux(t)*0.49),0,FYMflux(t)*0.02,0))},
            min(In[,1]),
            max(In[,1])
          )
        }

      x=1.67*(1.85+1.60*exp(-0.0786*clay))
      B=0.46/(x+1) # Proportion that goes to the BIO pool
      H=0.54/(x+1) # Proportion that goes to the HUM pool

      ai3=B*ks
      ai4=H*ks

      A=diag(-ks)
      A[3,]=A[3,]+ai3
      A[4,]=A[4,]+ai4

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
      Mod=GeneralModel(t=t,A=Af,ivList=C0,inputFluxes=inputFluxes,solverfunc=solver,pass=pass)
     return(Mod)
### A Model Object that can be further queried 
      ##seealso<< \code{\link{ICBMModel}} 
      ##examples<<
      ##t=0:500 
      ##Ex=RothCModel(t)
      ##Ct=getC(Ex)
      ##Rt=getReleaseFlux(Ex)
      ##
      ##matplot(t,Ct,type="l",col=1:5, ylim=c(0,25),
      ##  ylab=expression(paste("Carbon stores (Mg C ", ha^-1,")")),
      ##  xlab="Time (years)", lty=1)
      ##lines(t,rowSums(Ct),lwd=2)
      ##legend("topleft",
      ##  c("Pool 1, DPM",
      ##    "Pool 2, RPM",
      ##    "Pool 3, BIO",
      ##    "Pool 4, HUM",
      ##    "Pool 5, IOM",
      ##    "Total Carbon"),
      ##  lty=1,
      ##  lwd=c(rep(1,5),2),
      ##  col=c(1:5,1),
      ##  bty="n"
      ##)

      ##matplot(t,Rt,type="l",ylim=c(0,2), lty=1,
      ##        ylab=expression(paste("Respiration (Mg C ", ha^-1, yr^-1,")"))
      ##        ,xlab="Time") 
      ##lines(t,rowSums(Rt),lwd=2) 
      ##legend("topleft",
      ##   c("Pool 1, DPM", 
      ##     "Pool 2, RPM",
      ##     "Pool 3, BIO",
      ##     "Pool 4, HUM",
      ##     "Pool 5, IOM",
      ##     "Total Respiration"), 
      ##   lty=1,
      ##   lwd=c(rep(1,5),2),
      ##   col=c(1:5,1),
      ##   bty="n"
      ##
    }
