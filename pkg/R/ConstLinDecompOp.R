convert_to_vector_of_ConstantOutFluxRates<-function(out_flux_rates){
  if(inherits(out_flux_rates[[1]],'ConstantOutFluxRate')){
      return(out_flux_rates)
  }else{
    if (is.null(names(out_flux_rates)) ) {
        stop('If out_flux_rates is a vector it must be either a vector of instances of class ConstantOutFluxRate or a numeric vector with names of the from "i" representing pool i')
    } 
    if (inherits(out_flux_rates,'numeric')){
      keys=names(out_flux_rates)
      rates=vector()
      for (key in names(out_flux_rates)){
        rates=append(rates,ConstantOutFluxRate(source=as.integer(key),rate_constant=out_flux_rates[[key]]))
      }
      return(rates)
    } 
  }
}




#' convert names of  vectors or lists to class PoolConnection 
convert_to_vector_of_ConstantInternalFluxRates_by_PooIndex<-function(internal_flux_rates){
    if (
        length(internal_flux_rates)==0 
        || allElementsAreOfClass(internal_flux_rates,"ConstantOutFluxRateList_by_PoolIndex")
    ){
        # nothing to do
        return(internal_flux_rates)
    } 
  #if ( is.null(names(internal_flux_rates))){
  #      stop('internal_flux_rates must be either a numeric vector with names of the from "i_to_j" or a vector of instances of class ConstantInternalFluxRate')
  #}
  if (inherits(internal_flux_rates,'numeric')){
    keys=names(internal_flux_rates)
    rates=vector()
    for (key in names(internal_flux_rates)){
      rates=append(
                  rates 
                  ,ConstantInternalFluxRate_by_PoolIndex(
                     src_to_dest=key,
                    ,rate_constant=internal_flux_rates[[key]]
                  )
      )
    }
    return(rates)
  }
}

#' A class to represent a constant (=nonautonomuous,linear) compartmental matrix 
#'
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
      ),
      definition=function (mat,numberOfPools=nrow(mat)){
        r <- nrow(mat)
        c <- ncol(mat)
        if (r!=c){
           stop(sprintf('The matrix has to be quadratic!. Your matrix has %s rows and %s columns',r,c))
        }
      return(new("ConstLinDecompOp",mat=mat))
     }
)

#' helper function 
mat_from_integer_flux_lists=function(
  internal_flux_rates=NULL
  ,out_flux_rates=NULL
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
    src<-ifr@sourceIndex
    N[src,src]=N[src,src]+ifr@rate_constant
  }
  To=diag(nrow=np,-1)
  for (ifr in internal_flux_rates){
    To[dest,src]=ifr@rate_constant/N[src,src]
  }
  B<-To%*%N
  return(B)
}

no_outflux_warning=function(){
            warning('Compartmental system without out fluxes. For non zero inputs the 
                    material will accumulate in the system.')
}
setMethod(
      f="ConstLinDecompOp",
      signature=c(
         mat="missing"
        ,internal_flux_rates='list'
        ,out_flux_rates='missing'
        ,numberOfPools='numeric'
      ),
      definition=function(
        internal_flux_rates
        ,numberOfPools
      ){
            no_outflux_warning()
            np=PoolIndex(numberOfPools)

            internal_flux_rates<-convert_to_vector_of_ConstantInternalFluxRates_by_PooIndex(internal_flux_rates)
            print('#######################################################################3')
            print(internal_flux_rates)


            B<- mat_from_integer_flux_lists(
                internal_flux_rates=internal_flux_rates
                ,numberOfPools= numberOfPools
            )
            new('ConstLinDecompOp',mat=B)
      }
)

#setMethod(
#      f="ConstLinDecompOp_by_PoolName",
#      signature=c(
#      #  ,internal_flux_rates='vector'
#      #  ,out_flux_rates='vector'
#         ,poolNames='character'
#      ),
#      definition=function(
#        internal_flux_rates=list()
#        ,out_flux_rates=list()
#        ,poolNames
#      ){
#        numberOfPools=PoolIndex(length(poolNames))
#        if (length(out_flux_rates)==0){
#            # nothing to do
#            warning('Compartmental system without outfluxes')
#        } else {
#            out_flux_rates<-convert_to_vector_of_ConstantOutFluxRates(out_flux_rates)
#        }
#        internal_flux_rates<-convert_to_vector_of_ConstantInternalFluxRates(internal_flux_rates)
#        # we already have a vector of flux rates
#        # but now we make sure that the rates are indexed by integers (not names))
#        internal_flux_rates_by_index<-as.vector(lapply(
#            internal_flux_rates
#            ,function(ifr){by_PoolIndex(ifr,poolNames) }
#        ))
#        out_flux_rates_by_index<-as.vector(lapply(
#            internal_flux_rates
#            ,function(ifr){by_PoolIndex(ifr,poolNames) }
#        ))
#
#        B<- mat_from_integer_flux_lists(
#          internal_flux_rates_by_index
#          ,out_flux_rates_by_index
#          ,numberOfPools
#        )
#        return(new('ConstLinDecompOp',mat=B))
#      }
#)
#setMethod(
#      f="ConstLinDecompOp",
#      signature=c(
#         mat="missing"
#        ,internal_flux_rates='missing'
#        ,out_flux_rates='vector'
#        ,numberOfPools='numeric'
#      ),
#      definition=function 
#      (out_flux_rates,numberOfPools){
#        return(
#          ConstLinDecompOp(
#            internal_flux_rates=numeric()
#            ,out_flux_rates=out_flux_rates
#            ,numberOfPools=numberOfPools
#          )
#        )
#      }
#)
#setMethod(
#      f="ConstLinDecompOp",
#      signature=c(
#         mat="missing"
#        ,internal_flux_rates='vector'
#        ,out_flux_rates='missing'
#        ,numberOfPools='numeric'
#      ),
#      definition=function 
#      (internal_flux_rates,numberOfPools){
#        return(
#          ConstLinDecompOp(
#            internal_flux_rates=internal_flux_rates
#            ,out_flux_rates=list()
#            ,numberOfPools=numberOfPools
#          )
#        )
#      }
#)
#setMethod(
#      f="ConstLinDecompOp",
#      signature=c(
#         mat="missing"
#        ,internal_flux_rates='missing'
#        ,out_flux_rates='missing'
#        ,numberOfPools='numeric'
#      ),
#      definition=function 
#      (numberOfPools){
#        return(
#          ConstLinDecompOp(
#            internal_flux_rates=numeric()
#            ,out_flux_rates=numeric()
#            ,numberOfPools=numberOfPools
#          )
#        )
#      }
#)


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
setMethod(
    f= "ConstantOutFluxRateList_by_PoolIndex",
        signature(object="ConstLinDecompOp"),
        definition=function(object){

        B=object@mat
        np=nrow(B)
        # calculate outputs
        all_rates=lapply(
          1:np
          ,function(pool){
              ConstantOutFluxRate(
                sourceId=pool
                ,rate_constant= -sum(B[, pool])
              )
          }
        )
        non_zero_rates(all_rates)
        
          
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
    f= "ConstantInternalFluxRateList_by_PoolIndex",
        signature(object="ConstLinDecompOp"),
        definition=function(object){
            B=object@mat
            np=nrow(B)
            ## calculate internal fluxes
            #internal_fluxes = dict()
            #pipes = [(i,j) for i in range(state_vector.rows) 
            #                for j in range(state_vector.rows) if i != j]
            pipes=sets::cset_cartesian(1:np,1:np)
            all_rates=lapply(
                pipes
                ,function(tup){
                    pool_to  <-tup[[1]]
                    pool_from<-tup[[2]]
                    flux_rate = ConstantInternalFluxRate_by_PoolIndex(
                        sourceId=pool_from
                        ,destinationId=pool_to
                        ,rate_constant= B[pool_to, pool_from] 
                    )
                }
            )
            non_zero_rates(all_rates)
        }
)
