setClass(
    Class="UnBoundLinDecompOp",
    contains=c("DecompOp"),
    slots=list( matFunc="function")
)

#' @auto

#' @auto

#' @auto
setMethod(
     f="initialize",
     signature="UnBoundLinDecompOp",
     definition=function 
     (.Object,matFunc=function(){})
     {
        .Object@matFunc=matFunc
     return(.Object)
     }
)

#' @auto

#' @auto

#' @auto
setMethod(
      f="UnBoundLinDecompOp",
      signature=c(matFunc="function"),
      definition=function 
      (matFunc){
        mat <- matFunc(0)
        r <- nrow(mat)
        c <- ncol(mat)
        if (r!=c){
           stop(sprintf('The matrix valued function has to return a quadratic matrix!. Your matrix has %s rows and %s columns',r,c))
        }
      return(new("UnBoundLinDecompOp",matFunc=matFunc))
     }
)

#' @auto

#' @auto

#' @auto
setMethod(
    f="getFunctionDefinition",
    signature="UnBoundLinDecompOp",
    definition=function 
    (object){
      return(object@matFunc)
    }
)

#' @auto

#' @auto

#' @auto
setMethod(
    f="getTimeRange",
    signature="UnBoundLinDecompOp",
    definition=function 
    (object)
    {
        return( c("t_min"=-Inf,"t_max"=Inf))
    }
)
