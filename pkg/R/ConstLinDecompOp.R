convert_to_out_flux_rates<-function(out_flux_rates){
  if (length(out_flux_rates)==0){
    warning('Compartmental system without outfluxes')
    return(out_flux_rates)
  } else 
  {
    if (
        !(
          is.null(names(out_flux_rates)) 
          &inherits(out_flux_rates[[1]],'ConstantOutFluxRate')
        )
    )
    {
      if (inherits(out_flux_rates,'numeric')){
        keys=names(out_flux_rates)
        rates=vector()
        for (key in names(out_flux_rates)){
          rates=append(rates,ConstantOutFluxRate(key,out_flux_rates[[key]]))
        }
        return(rates)
      } else {
        stop('If out_flux_rates is a vector it must be either a vector of instances of class ConstantOutFluxRate or a numeric vector with names of the from "i" representing pool i')
      }
    } 
  }
}
convert_to_internal_flux_rates<-function(internal_flux_rates){
  if (length(internal_flux_rates)==0){
    return(internal_flux_rates)
  } else {
    if (
        !(
            is.null(names(internal_flux_rates)) 
            & inherits(
                internal_flux_rates[[1]] ,'ConstantInternalFluxRate'
              )
        )
    )
    {
      if (inherits(internal_flux_rates,'numeric')){
        keys=names(internal_flux_rates)
        rates=vector()
        for (key in names(internal_flux_rates)){
          rates=append(
                      rates 
                      ,ConstantInternalFluxRate(
                         src_to_dest=key,
                        ,rate_constant=internal_flux_rates[[key]]
                      )
          )
        }
        return(rates)
      } else{
          stop('internal_flux_rates must be either a numeric vector with names of the from "i_to_j" or a vector of instances of class ConstantInternalFluxRate')
      }
    }
  }
}
correctnessOfDecompOp=function(object)
  A=object@mat

#' An S4 class to represent constant (nonautonomuous) compartmental matrix 
#'
#' @family ConstlinDecompOp_constructor
setClass(
    Class="ConstLinDecompOp",
    contains=c("DecompOp"),
    slots=list( mat="matrix")
)
setMethod(
     f="initialize",
     signature="ConstLinDecompOp",
     definition=function 
     (.Object,mat=matrix())
     {
        .Object@mat=mat
     return(.Object)
     }
)

setMethod(
      f="ConstLinDecompOp",
      signature=c(
         mat="matrix"
        ,internal_flux_rates='missing'
        ,out_flux_rates='missing'
        ,numberOfPools='missing'
      ),
      definition=function 
      (mat){
        r <- nrow(mat)
        c <- ncol(mat)
        if (r!=c){
           stop(sprintf('The matrix has to be quadratic!. Your matrix has %s rows and %s columns',r,c))
        }
      return(new("ConstLinDecompOp",mat=mat))
     }
)

setMethod(
      f="ConstLinDecompOp",
      signature=c(
         mat="missing"
        ,internal_flux_rates='vector'
        ,out_flux_rates='vector'
        ,numberOfPools='numeric'
      ),
      definition=function 
      (internal_flux_rates,out_flux_rates,numberOfPools){
        np=PoolIndex(numberOfPools)
        internal_flux_rates<-convert_to_internal_flux_rates(internal_flux_rates)
        out_flux_rates<-convert_to_out_flux_rates(out_flux_rates)
        N=matrix(nrow=np,ncol=np,0)
        for (ofr in out_flux_rates){
            src=ofr@source
            if (src> numberOfPools){stop("The index of the source pool must be smaller than the number of pools")}
            N[src,src]=ofr@rate_constant
        }
        for (ifr in internal_flux_rates){
          dest<-ifr@destination
          src<-ifr@source
          if (dest> numberOfPools){stop("The index of the destination pool must be smaller than the number of pools")}
          if (src> numberOfPools){stop("The index of the source pool must be smaller than the number of pools")}
          N[src,src]=N[src,src]+ifr@rate_constant
        }
        To=diag(nrow=np,-1)
        for (ifr in internal_flux_rates){
          To[dest,src]=ifr@rate_constant/N[src,src]
        }
        B<-To%*%N
        return(new('ConstLinDecompOp',mat=B))
      }
)
setMethod(
      f="ConstLinDecompOp",
      signature=c(
         mat="missing"
        ,internal_flux_rates='missing'
        ,out_flux_rates='vector'
        ,numberOfPools='numeric'
      ),
      definition=function 
      (out_flux_rates,numberOfPools){
        return(
          ConstLinDecompOp(
            internal_flux_rates=numeric()
            ,out_flux_rates=out_flux_rates
            ,numberOfPools=numberOfPools
          )
        )
      }
)
setMethod(
      f="ConstLinDecompOp",
      signature=c(
         mat="missing"
        ,internal_flux_rates='vector'
        ,out_flux_rates='missing'
        ,numberOfPools='numeric'
      ),
      definition=function 
      (internal_flux_rates,numberOfPools){
        return(
          ConstLinDecompOp(
            internal_flux_rates=internal_flux_rates
            ,out_flux_rates=numeric()
            ,numberOfPools=numberOfPools
          )
        )
      }
)
setMethod(
      f="ConstLinDecompOp",
      signature=c(
         mat="missing"
        ,internal_flux_rates='missing'
        ,out_flux_rates='missing'
        ,numberOfPools='numeric'
      ),
      definition=function 
      (numberOfPools){
        return(
          ConstLinDecompOp(
            internal_flux_rates=numeric()
            ,out_flux_rates=numeric()
            ,numberOfPools=numberOfPools
          )
        )
      }
)
setMethod(
    f="getFunctionDefinition",
    signature="ConstLinDecompOp",
    definition=function 
    (object){
      return(function(t){object@mat})
    }
)
setMethod(
    f="getTimeRange",
    signature="ConstLinDecompOp",
    definition=function 
    (object)
    {
        return( c("t_min"=-Inf,"t_max"=Inf))
    }
)
setMethod(
  f= "getMeanTransitTime",
    signature= "ConstLinDecompOp",
    definition=function 
      (object,
      inputDistribution 
      ){
      f=getFunctionDefinition(object)
      g=function(t){spectralNorm(f(t))}
      t_max=function(t_end){
          t_step=t_end/10
          t=seq(0,t_end,t_step)
          norms=sapply(t,g)
          tm=100*max(norms)
	  return(tm)
      } 
      t_end=20
      t_end_new=t_max(t_end)
      while(t_end_new>t_end){
	  t_end=t_end_new
	  t_end_new=t_max(t_end)
      }
      longTailEstimate=t_end
      subd=10000
      t_step=t_end/subd
      t=seq(0,t_end,t_step)
      shortTailEstimate=min(sapply(t,g))
      ttdd=getTransitTimeDistributionDensity(object,inputDistribution,t)
      int2=splinefun(t,ttdd*t)
      meanTimeIntegrate=integrate(int2,0,t_end,subdivisions=subd)[["value"]] 
      return(meanTimeIntegrate)
   }
)
setMethod(
   f= "getTransitTimeDistributionDensity",
      signature= "ConstLinDecompOp",
      definition=function 
      (object,
      inputDistribution, 
      times 
      ){
      sVmat=inputDistribution
      n=length(inputDistribution)
      inputFluxes=BoundInFluxes(
        map=function(t0){matrix(nrow=n,ncol=1,0)},
        starttime= -Inf, 
        endtime=+Inf 
      ) 
      mod=GeneralModel(times,object,sVmat,inputFluxes)
      R=getReleaseFlux(mod)
      TTD=rowSums(R)
      return(TTD)
   }
)

setMethod(
   f= "getCompartmentalMatrixFunc",
      signature(object="ConstLinDecompOp"),
      definition=function(object){
          getFunctionDefinition(object)
   }
)
