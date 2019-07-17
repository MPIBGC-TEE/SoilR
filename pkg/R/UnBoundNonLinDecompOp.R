
#' An S4 class to represent the operation nonlinear nonautonomuous compartmental matrix 
#'
#' @family UnBoundNonLinDecompOp_constructor
setClass(
    Class="UnBoundNonLinDecompOp",
    contains=c("DecompOp"),
    slots=list( matFunc="function")
)
# at the moment we do not need to overload the initialization 
# we will do so when we want to validate arguments beforehand
#setMethod(
#     f="initialize",
#     signature="UnBoundNonLinDecompOp",
#     definition=function 
#     (.Object,f=function(C,t){matrix()})
#     {
#        .Object@matFunc=f
#     return(.Object)
#     }
#)

setMethod(
      f="UnBoundNonLinDecompOp",
      signature=c(
         matFunc="function"
        ,internal_fluxes='missing'
        ,out_fluxes='missing'
        ,numberOfPools='missing'
      ),
      definition=function 
      (matFunc){
        mat=matFunc(0)
        r <- nrow(mat)
        c <- ncol(mat)
        if (r!=c){
           stop(sprintf('The matrix returned by matFunc has to be quadratic!. Your matrix has %s rows and %s columns',r,c))
        }
      return(new("UnBoundNonLinDecompOp",matFunc=matFunc))
     }
)

setMethod(
   f= "getCompartmentalMatrixFunc",
      signature(object="UnBoundNonLinDecompOp"),
      definition=function(object){ object@matFunc }
)


setMethod(
      f="UnBoundNonLinDecompOp",
      signature=c(
         matFunc="missing"
        # assume that we get a list of the right form with elements
        # of being correctly indexed by numbers and the flux functions with a
        # vector argument for the state vector
        ,internal_fluxes='vector'
        # assume that we get a list of the right form with elements
        # of being correctly indexed by numbers and the flux functions with a
        # vector argument for the state vector
        ,out_fluxes='vector'
        ,numberOfPools='numeric'
      ),
      definition=function(
        internal_fluxes
        ,out_fluxes
        ,numberOfPools
        ){
        np=PoolIndex(numberOfPools)
        BFunc<-function(X,t){
            N=matrix(nrow=np,ncol=np,0)
            for (of in out_fluxes){
                src=of@sourceIndex
                if (src> numberOfPools){stop("The index of the source pool must be smaller than the number of pools")}
                N[src,src]=of@func(X,t)
            }
            for (intf in internal_fluxes){
              dest<-intf@destinationIndex
              src<-intf@sourceIndex
              if (dest> numberOfPools){stop("The index of the destination pool must be smaller than the number of pools")}
              if (src> numberOfPools){stop("The index of the source pool must be smaller than the number of pools")}
              N[src,src]=N[src,src]+intf@func(X,t)
            }
            To=diag(nrow=np,-1)
            for (intf in internal_fluxes){
              To[dest,src]=intf@func(X,t)/N[src,src]
            }
            B<-To%*%N
            return(B)
        }
        return(new('UnBoundNonLinDecompOp',matFunc=BFunc))
      }
)
#
#setMethod(
#      f="UnBoundNonLinDecompOp",
#      signature=c(
#         mat="missing"
#        ,internal_flux_rates='missing'
#        ,out_flux_rates='vector'
#        ,numberOfPools='numeric'
#      ),
#      definition=function 
#      (out_flux_rates,numberOfPools){
#        return(
#          UnBoundNonLinDecompOp(
#            internal_flux_rates=numeric()
#            ,out_flux_rates=out_flux_rates
#            ,numberOfPools=numberOfPools
#          )
#        )
#      }
#)
#setMethod(
#      f="UnBoundNonLinDecompOp",
#      signature=c(
#         mat="missing"
#        ,internal_flux_rates='vector'
#        ,out_flux_rates='missing'
#        ,numberOfPools='numeric'
#      ),
#      definition=function 
#      (internal_flux_rates,numberOfPools){
#        return(
#          UnBoundNonLinDecompOp(
#            internal_flux_rates=internal_flux_rates
#            ,out_flux_rates=numeric()
#            ,numberOfPools=numberOfPools
#          )
#        )
#      }
#)
#setMethod(
#      f="UnBoundNonLinDecompOp",
#      signature=c(
#         mat="missing"
#        ,internal_flux_rates='missing'
#        ,out_flux_rates='missing'
#        ,numberOfPools='numeric'
#      ),
#      definition=function 
#      (numberOfPools){
#        return(
#          UnBoundNonLinDecompOp(
#            internal_flux_rates=numeric()
#            ,out_flux_rates=numeric()
#            ,numberOfPools=numberOfPools
#          )
#        )
#      }
#)
#setMethod(
#    f="getFunctionDefinition",
#    signature="UnBoundNonLinDecompOp",
#    definition=function 
#    (object){
#      return(function(t){object@mat})
#    }
#)
#setMethod(
#    f="getTimeRange",
#    signature="UnBoundNonLinDecompOp",
#    definition=function 
#    (object)
#    {
#        return( c("t_min"=-Inf,"t_max"=Inf))
#    }
#)
#setMethod(
#  f= "getMeanTransitTime",
#    signature= "UnBoundNonLinDecompOp",
#    definition=function 
#      (object,
#      inputDistribution 
#      ){
#      f=getFunctionDefinition(object)
#      g=function(t){spectralNorm(f(t))}
#      t_max=function(t_end){
#          t_step=t_end/10
#          t=seq(0,t_end,t_step)
#          norms=sapply(t,g)
#          tm=100*max(norms)
#	  return(tm)
#      } 
#      t_end=20
#      t_end_new=t_max(t_end)
#      while(t_end_new>t_end){
#	  t_end=t_end_new
#	  t_end_new=t_max(t_end)
#      }
#      longTailEstimate=t_end
#      subd=10000
#      t_step=t_end/subd
#      t=seq(0,t_end,t_step)
#      shortTailEstimate=min(sapply(t,g))
#      ttdd=getTransitTimeDistributionDensity(object,inputDistribution,t)
#      int2=splinefun(t,ttdd*t)
#      meanTimeIntegrate=integrate(int2,0,t_end,subdivisions=subd)[["value"]] 
#      return(meanTimeIntegrate)
#   }
#)
#setMethod(
#   f= "getTransitTimeDistributionDensity",
#      signature= "UnBoundNonLinDecompOp",
#      definition=function 
#      (object,
#      inputDistribution, 
#      times 
#      ){
#      sVmat=inputDistribution
#      n=length(inputDistribution)
#      inputFluxes=BoundInFluxes(
#        map=function(t0){matrix(nrow=n,ncol=1,0)},
#        starttime= -Inf, 
#        endtime=+Inf 
#      ) 
#      mod=GeneralModel(times,object,sVmat,inputFluxes)
#      R=getReleaseFlux(mod)
#      TTD=rowSums(R)
#      return(TTD)
#   }
#)
