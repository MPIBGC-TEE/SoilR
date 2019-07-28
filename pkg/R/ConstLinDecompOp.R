
#' helper function 
#'
#' @keywords internal
mat_from_integer_flux_lists=function(
  internal_flux_rates
  ,out_flux_rates
  ,numberOfPools
){
  np=PoolIndex(numberOfPools)
  N=matrix(nrow=np,ncol=np,0)
  for (ofr in out_flux_rates){
      src=ofr@sourceIndex
      if (src> np){stop("The index of the source pool must be smaller than the number of pools")}
      N[src,src]=ofr@rate_constant
  }

  for (ifr in internal_flux_rates){
    dest<-ifr@destinationIndex
    if (dest> np){stop("The index of the destination pool of all internal fluxes must be smaller than the number of pools")}
    src<-ifr@sourceIndex
    if (src> np){stop("The index of the source pool of all internal fluxes must be smaller than the number of pools")}
    N[src,src]=N[src,src]+ifr@rate_constant
  }
  To=diag(nrow=np,-1)
  for (ifr in internal_flux_rates){
    To[dest,src]=ifr@rate_constant/N[src,src]
  }
  B<-To%*%N
  return(B)
}

#' A class to represent a constant (=nonautonomuous,linear) compartmental matrix 
#' or equivalently a combination of ordered constant internal flux rates and 
#' constant out flux rates.
setClass(
    Class="ConstLinDecompOp",
    contains=c("DecompOp"),
    slots=list( mat="matrix")
)

setMethod(
    f="initialize",
    signature="ConstLinDecompOp",
    definition=function (.Object,mat=matrix())
    {
       .Object@mat=mat
        return(.Object)
    }
)

#' Constructor 
#'
#' @param mat A quadratic compartmental matrix. 
#' If present nome of \code{internal_flux_rates}, 
#' code{out_flux_rates}, code{numberOfPools} 
#' may be present.
#' @param internal_flux_rates A list that can be supplied 
#' in one of the following ways:
#' \enumerate{
#' \item A \code{list} of numbers with names of the form "2->4"  
#' \item A \code{numeric} vector of numbers with names of the form "2->4"  
#' \item A \code{list} of objects of class 
#' \linkS4class{ConstantInternalFluxRate_by_PoolIndex}
#' \item An object of class  
#" \linkS4class{ConstantInternalFluxRateList_by_PoolIndex}
#' }
#' @param out_flux_rates A list that can be supplied in one of the 
#' following ways:
#' \enumerate{
#' \item A \code{list} of numbers with names that can be converted 
#' to integers ("2" for pool 2  
#' \item A \code{numeric} vector of numbers with names that can be converted 
#' to integers.
#' \item A \code{list} of objects of class 
#' \linkS4class{ConstantOutFluxRate_by_PoolIndex}
#' \item An object of class
#' \linkS4class{ConstantOutFluxRateList_by_PoolIndex}
#' }
#' @param numberOfPools  The number of pools, should not be present if the 
#' \code{mat} argument is supplied but must be present otherwise.

ConstLinDecompOp=function(
    mat
    ,internal_flux_rates
    ,out_flux_rates
    ,numberOfPools
    ){
        if (missing(mat)){
            assertthat::assert_that(
                hasArg(numberOfPools)
                ,msg="If mat is not given numberOfPools must be present."
            )
            if(hasArg(internal_flux_rates)){
                   internal_flux_rates=ConstantInternalFluxRateList_by_PoolIndex(internal_flux_rates)
            }else{
                   internal_flux_rates=list()
            }
            if(hasArg(out_flux_rates)){ 
                   out_flux_rates=ConstantOutFluxRateList_by_PoolIndex(out_flux_rates)
            }else{
                   out_flux_rates=list()
            }
            mat= mat_from_integer_flux_lists(
                internal_flux_rates=internal_flux_rates
                ,out_flux_rates = out_flux_rates
                ,numberOfPools
            )
        }else{
            assertthat::assert_that(
                all(
                    missing(internal_flux_rates)
                    ,missing(out_flux_rates)
                    ,missing(numberOfPools)
                )
                ,msg="if mat is given, non of internal_flux_rates, 
                      out_flux_rates and numberOfPools, can be given"
            )
            r <- nrow(mat)
            c <- ncol(mat)
            assertthat::are_equal(
                r
                ,c
                ,msg=sprintf('The matrix has to be quadratic!. Your matrix has %s rows and %s columns',r,c)
            )
        }
    return(new("ConstLinDecompOp",mat=mat))
}
#)


#' helper function 
#' @keywords internal
no_outflux_warning=function(){
            warning('Compartmental system without out fluxes. For non zero inputs the 
                    material will accumulate in the system.')
}



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
setMethod(
   f= "getConstantCompartmentalMatrix",
      signature(object="ConstLinDecompOp"),
      definition=function(object){
          object@mat
   }
)
non_zero_rates=function(all_rates){
        all_rates[
          as.logical(
            lapply(
                all_rates
                ,function(cofr){cofr@rate_constant!=0}
            )
          )
        ]
}
setMethod(
    f= "getConstantOutFluxRateList_by_PoolIndex",
        signature(object="ConstLinDecompOp"),
        definition=function(object){
            B=object@mat
            np=nrow(B)
            # calculate outputs
            all_rates=lapply(
              1:np
              ,function(pool){
                  ConstantOutFluxRate_by_PoolIndex(
                    sourceIndex=pool
                    ,rate_constant= -sum(B[, pool])
                  )
              }
            )
            ConstantOutFluxRateList_by_PoolIndex(
                non_zero_rates(
                    all_rates
                )
            )
        
          
   }
)
setMethod(
    f= "getConstantInternalFluxRateList_by_PoolIndex",
        signature(object="ConstLinDecompOp"),
        definition=function(object){
            B=object@mat
            np=nrow(B)
            ## calculate internal fluxes
            #internal_fluxes = dict()
            #pipes = [(i,j) for i in range(state_vector.rows) 
            #                for j in range(state_vector.rows) if i != j]
            pipes=sets::cset_cartesian(1:np,1:np) - as.set(lapply((1:np),function(i){tuple(i,i)}))
            
            #pp('pipes')
            all_rates=lapply(
                pipes
                ,function(tup){
                    pool_to  <-tup[[1]]
                    pool_from<-tup[[2]]
                    flux_rate = ConstantInternalFluxRate_by_PoolIndex(
                        sourceIndex         =pool_from
                        ,destinationIndex   =pool_to
                        ,rate_constant=     B[pool_to, pool_from] 
                    )
                }
            )
            ConstantInternalFluxRateList_by_PoolIndex(
                non_zero_rates(
                    all_rates
                )
            )
        }
)
