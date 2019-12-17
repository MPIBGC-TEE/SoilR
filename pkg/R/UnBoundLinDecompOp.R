#' automatic title
#' 
#' @autocomment 
setClass(
    Class="UnBoundLinDecompOp",
    contains=c("DecompOp"),
    slots=list( matFunc="function")
)



#' automatic title
#' 
#' @param .Object no manual documentation
#' @param matFunc no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
setMethod(
     f="initialize",
     signature="UnBoundLinDecompOp",
     definition=function (.Object,matFunc=function(){}) {
        .Object@matFunc=matFunc
     return(.Object)
     }
)



#' automatic title
#' 
#' @param matFunc no manual documentation
#' @autocomment 
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



#' automatic title
#' 
#' @param object no manual documentation
#' @autocomment 
setMethod(
    f="getFunctionDefinition",
    signature="UnBoundLinDecompOp",
    definition=function 
    (object){
      return(object@matFunc)
    }
)



#' automatic title
#' 
#' @param object no manual documentation
#' @autocomment 
setMethod(
    f="getTimeRange",
    signature="UnBoundLinDecompOp",
    definition=function 
    (object)
    {
        return( c("t_min"=-Inf,"t_max"=Inf))
    }
)
