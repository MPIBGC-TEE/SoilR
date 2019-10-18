
##' convert names of  vectors or lists to class ConstantOutFluxRate 
#convert_to_vector_of_ConstantOutFluxRates<-function(out_flux_rates){
#  if(inherits(out_flux_rates[[1]],'ConstantOutFluxRate')){
#      return(out_flux_rates)
#  }else{
#    if (is.null(names(out_flux_rates)) ) {
#        stop('If out_flux_rates is a vector it must be either a vector of instances of class ConstantOutFluxRate or a numeric vector with names of the from "i" representing pool i')
#    } 
#    if (inherits(out_flux_rates,'numeric')){
#      keys=names(out_flux_rates)
#      rates=vector()
#      for (key in names(out_flux_rates)){
#        rates=append(rates,ConstantOutFluxRate(source=as.integer(key),rate_constant=out_flux_rates[[key]]))
#      }
#      return(rates)
#    } 
#  }
#}
#
#
#
#
##' convert names of  vectors or lists to class ConstantInternalFluxRate 
#convert_to_vector_of_ConstantInternalFluxRates<-function(internal_flux_rates){
#  if (length(internal_flux_rates)==0 | elements_are_PoolConnections(internal_flux_rates)
#  ){
#    return(internal_flux_rates)
#  } 
#  if ( is.null(names(internal_flux_rates))){
#        stop('internal_flux_rates must be either a numeric vector with names of the from "i_to_j" or a vector of instances of class ConstantInternalFluxRate')
#  }
#  if (inherits(internal_flux_rates,'numeric')){
#    keys=names(internal_flux_rates)
#    rates=vector()
#    for (key in names(internal_flux_rates)){
#      rates=append(
#                  rates 
#                  ,ConstantInternalFluxRate(
#                     src_to_dest=key,
#                    ,rate_constant=internal_flux_rates[[key]]
#                  )
#      )
#    }
#    return(rates)
#  }
#}
#
##' A class to represent a constant (=nonautonomuous,linear) compartmental matrix 
##'
setClass(
    Class="ConstLinDecompOpWithLinearScalarFactor"
    ,contains=c("DecompOp")
    ,slots=list(clo="ConstLinDecompOp",xi='TimeMap')
)
#setMethod(
#     f="initialize",
#     signature="ConstLinDecompOp",
#     definition=function 
#     (.Object,mat=matrix())
#     {
#        .Object@mat=mat
#     return(.Object)
#     }
#)
#

#' @auto

#' @auto

#' @auto
setMethod(
      f="ConstLinDecompOpWithLinearScalarFactor",
      signature=c(
         mat="matrix"
        ,internal_flux_rates='missing'
        ,out_flux_rates='missing'
        ,numberOfPools='missing'
        ,xi='ScalarTimeMap'
      ),
      definition=function 
      (mat,xi){
        r <- nrow(mat)
        c <- ncol(mat)
        if (r!=c){
           stop(sprintf('The matrix has to be quadratic!. Your matrix has %s rows and %s columns',r,c))
        }
        clo<-ConstLinDecompOp(mat=mat)
        new("ConstLinDecompOpWithLinearScalarFactor",clo=clo,xi=xi)
     }
)
#
##' helper function 
#mat_from_integer_flux_lists=function(
#  internal_flux_rates
#  ,out_flux_rates
#  ,numberOfPools
#){
#  np=PoolIndex(numberOfPools)
#  N=matrix(nrow=np,ncol=np,0)
#  for (ofr in out_flux_rates){
#      src=PoolIndex(ofr@sourceId)
#      if (src> np){stop("The index of the source pool must be smaller than the number of pools")}
#      N[src,src]=ofr@rate_constant
#  }
#
#  for (ifr in internal_flux_rates){
#    dest<-PoolIndex(ifr@destinationId)
#    src<-PoolIndex(ifr@sourceId)
#    N[src,src]=N[src,src]+ifr@rate_constant
#  }
#  To=diag(nrow=np,-1)
#  for (ifr in internal_flux_rates){
#    To[dest,src]=ifr@rate_constant/N[src,src]
#  }
#  B<-To%*%N
#  return(B)
#}
#
#
#setMethod(
#      f="ConstLinDecompOp",
#      signature=c(
#         mat="missing"
#        ,internal_flux_rates='vector'
#        ,out_flux_rates='vector'
#        ,numberOfPools='numeric'
#        ,poolNames='missing'
#      ),
#      definition=function(
#        internal_flux_rates
#        ,out_flux_rates
#        ,numberOfPools
#      ){
#        np=PoolIndex(numberOfPools)
#        #np=PoolIndex(length(poolNames))
#        if (length(out_flux_rates)==0){
#            # nothing to do convert
#            warning('Compartmental system without out fluxes')
#        } else {
#            out_flux_rates<-convert_to_vector_of_ConstantOutFluxRates(out_flux_rates)
#
#        }
#        internal_flux_rates<-convert_to_vector_of_ConstantInternalFluxRates(internal_flux_rates)
#
#        if( ! elements_are_Indexed_by_PoolIndex(internal_flux_rates)){
#            stop('Without poolNames available PoolIds must be numeric, otherwise no matrix can be computed')
#        }
#
#        B<- mat_from_integer_flux_lists(
#          internal_flux_rates
#          ,out_flux_rates
#          ,numberOfPools
#        )
#        return(new('ConstLinDecompOp',mat=B))
#      }
#)
#
#setMethod(
#      f="ConstLinDecompOp",
#      signature=c(
#         mat="missing"
#        ,internal_flux_rates='vector'
#        ,out_flux_rates='vector'
#        ,numberOfPools='missing'
#        ,poolNames='character'
#      ),
#      definition=function(
#        internal_flux_rates
#        ,out_flux_rates
#        ,poolNames
#      ){
#        #np=PoolIndex(numberOfPools)
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
#            ,out_flux_rates=numeric()
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
#
#
#
#
#
#
#
#
#
#
#

setMethod(
    f="getFunctionDefinition",
    signature="ConstLinDecompOpWithLinearScalarFactor",
    definition=function 
    (object){
      f_clo <-getFunctionDefinition(object@clo) # should be a constant matrix but we delegate 
      f_xi  <-getFunctionDefinition(object@xi)
      function(t){f_clo(t)*f_xi(t)}
    }
)

#' @auto

#' @auto

#' @auto
setMethod(
    f="getTimeRange"
    ,signature="ConstLinDecompOpWithLinearScalarFactor"
    ,definition=function (object) {
      TimeRangeIntersection(object@clo,object@xi)
    }
)

#' @auto

#' @auto

#' @auto
setMethod(
   f= "getConstantCompartmentalMatrix",
     ,signature="ConstLinDecompOpWithLinearScalarFactor"
     ,definition=function(object){
        getConstantCompartmentalMatrix(object@clo)
   }
)

#' @auto

#' @auto

#' @auto
setMethod(
   f= "getLinearScaleFactor",
     ,signature="ConstLinDecompOpWithLinearScalarFactor"
     ,definition=function(object){
        object@xi
   }
)

#' @auto

#' @auto

#' @auto
setMethod(
   f= "getConstLinDecompOp",
     ,signature="ConstLinDecompOpWithLinearScalarFactor"
     ,definition=function(object){
        object@clo
   }
)
#setMethod(
#  f= "getMeanTransitTime",
#    signature= "ConstLinDecompOp",
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
#      signature= "ConstLinDecompOp",
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
##' synonym
#setMethod(
#   f= "getCompartmentalMatrixFunc",
#      signature(object="ConstLinDecompOp"),
#      definition=function(object){
#          getFunctionDefinition(object)
#   }
#)
