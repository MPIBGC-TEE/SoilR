#
# vim:set ff=unix expandtab ts=2 sw=2:
ICBMModel<- function #Implementation of the Introductory Carbon Balance Model (ICBM)
    ### This function is an implementation of the Introductory Carbon Balance Model (ICBM).
    ### This is simply a two pool model connected in series.
    ##references<< Andren, O. and T. Katterer. 1997. ICBM: The Introductory Carbon Balance Model 
    ##for Exploration of Soil Carbon Balances. Ecological Applications 7:1226-1236.

    (t, ##<< A vector containing the points in time where the solution is sougth.
     ks=c(k1=0.8,k2=0.00605), ##<< A vector of length 2 with the decomposition rates for the young and the old pool.
     h=0.13, ##<< Humufication coefficient (transfer rate from young to old pool).
     r=1.32, ##<< External (environmental or edaphic) factor. 
     c0=c(Y0=0.3,O0=3.96), ##<< A vector of length 2 with the initial value of carbon stocks in the young and old pool. 
     In=0,   ##<< Mean annual carbon input to the soil. 
     solver=deSolve.lsoda.wrapper, ##<< A function that solves the system of ODEs. This can be \code{\link{euler}} or \code{\link{deSolve.lsoda.wrapper}} or any other user provided function with the same interface.
     pass=FALSE  ##<< if TRUE forces the constructor to create the model even if it is invalid 
     )
    { 
      t_start=min(t)
      t_end=max(t)
     if(length(ks)!=2) stop("The vector of decomposition rates is not of length = 2")
     if(length(c0)!=2) stop("The vector with initial conditions is not of length = 2")
     
     A=diag(-ks)
     A[2,1]=ks[1]*h
     Ar=A*r
     inputFluxes=BoundInFlux(
        function(t){matrix(nrow=nrow(A),ncol=1,c(In,0))},
        t_start,
        t_end
     )
     Af=BoundLinDecompOp(map=function(t0){Ar},t_start,t_end)
     Mod=GeneralModel(t=t,A=Af,c0,inputFluxes,solver,pass)
     return(Mod)
 
     ##seealso<< There are other \code{\link{predefinedModels}} and also more general functions like \code{\link{Model}}.
     
     ##exampleFunctionsFromFiles<< 
     ##inst/examples/exICBMModel.R exICBMModel_paper
    
}
