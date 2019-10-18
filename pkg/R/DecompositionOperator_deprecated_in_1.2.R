
#' @auto

#' @auto

#' @auto
setMethod(
    f="initialize",
    signature="DecompositionOperator",
    definition=function 
    (.Object,
    starttime=numeric(),
    endtime=numeric(),
    map=function(t){t},
    lag=0
    ){
    warning("The class DecompositionOperator is deprecated. The fuctionallity is now implemented by class BoundLinDecompOp. To get rid of this warning change your code from:\n
    new(DecompositionOperator,other,args,...) to \n
    BoundLinDecompOp(other,args,...) ")
    .Object@starttime=starttime
    .Object@endtime=endtime
    .Object@map=map
    return(.Object)
    }
)

#' @auto

#' @auto

#' @auto
setMethod(
    f="getTimeRange",
    signature="DecompositionOperator",
    definition=function 
    (object 
    ){
        return( c("t_min"=object@starttime,"t_max"=object@endtime))
    }
)

#' @auto

#' @auto

#' @auto
setMethod(
    f="getFunctionDefinition",
    signature="DecompositionOperator",
    definition=function(object){
        return(object@map)
    }
)
