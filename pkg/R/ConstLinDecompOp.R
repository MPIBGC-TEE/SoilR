
#' helper function 
#'
#' @keywords internal
from_integer_flux_lists_with_defaults=function(
  internal_flux_rates=list()
  ,out_flux_rates=list()
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
  return(new('ConstLinDecompOp',mat=B))
}

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
#setMethod(
#    "ConstLinDecompOp"
#    ,signature=signature(
#        mat='matrix'
#        ,internal_flux_rates='missing'
#        ,out_flux_rates='missing'
#        ,numberOfPools='numeric'
#        ,poolNames='missing'
#    )
#    ,definition=function( mat){
#        r <- nrow(mat)
#        c <- ncol(mat)
#        assertthat::are_equal(
#            r
#            ,c
#            ,msg=sprintf('The matrix has to be quadratic!. Your matrix has %s rows and %s columns',r,c)
#        )
#        return(new("ConstLinDecompOp",mat=mat)) 
#    }
#)
#
#' Constructor 
setMethod(
    "ConstLinDecompOp"
    ,signature=signature(
        mat='missing'
        ,internal_flux_rates='ConstantInternalFluxRateList_by_PoolIndex'
        ,out_flux_rates='ConstantOutFluxRateList_by_PoolIndex'
        ,numberOfPools='numeric'
        ,poolNames='missing'
    )
    ,definition=function(
         internal_flux_rates
        ,out_flux_rates
        ,numberOfPools
    ){
         from_integer_flux_lists_with_defaults(
                internal_flux_rates=internal_flux_rates
                ,out_flux_rates = out_flux_rates
                ,numberOfPools = numberOfPools
         )
    }
)
#' Constructor 
setMethod(
    "ConstLinDecompOp"
    ,signature=signature(
        mat='missing'
        ,internal_flux_rates='missing'
        ,out_flux_rates='ConstantOutFluxRateList_by_PoolIndex'
        ,numberOfPools='numeric'
        ,poolNames='missing'
    )
    ,definition=function(
         out_flux_rates
        ,numberOfPools
    ){
         from_integer_flux_lists_with_defaults(
                out_flux_rates = out_flux_rates
                ,numberOfPools = numberOfPools
         )
    }
)
#' Constructor 
setMethod(
    "ConstLinDecompOp"
    ,signature=signature(
        mat='missing'
        ,internal_flux_rates='ConstantInternalFluxRateList_by_PoolIndex'
        ,out_flux_rates='missing'
        ,numberOfPools='numeric'
        ,poolNames='missing'
    )
    ,definition=function(
         internal_flux_rates
        ,numberOfPools
    ){
         from_integer_flux_lists_with_defaults(
                internal_flux_rates=internal_flux_rates
                ,numberOfPools = numberOfPools
         )
    }
)

#' alternative Constructor with pool names 
setMethod(
    'ConstLinDecompOp'
    ,signature=signature(
        mat='missing'
        ,internal_flux_rates='ConstantInternalFluxRateList_by_PoolName'
        #,out_flux_rates='ConstantOutFluxRateList_by_PoolName'
        ,poolNames='character'
     )
    ,definition=function(
         internal_flux_rates
        ,out_flux_rates
        ,poolNames
    ){
        ConstLinDecompOp(
            internal_flux_rates=by_PoolIndex(internal_flux_rates,poolNames)
           , out_flux_rates=by_PoolIndex(out_flux_rates,poolNames)
           ,numberOfPools=length(poolNames)
        )

    }
)

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
