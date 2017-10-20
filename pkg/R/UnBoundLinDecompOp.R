#
# vim:set ff=unix expandtab ts=2 sw=2:
#correctnessOfDecompOp=function(object){ A=object@mat}
  
setClass(# constant decomposition operator 
    Class="UnBoundLinDecompOp",
    contains=c("DecompOp"),
    slots=list( matFunc="function")
)
setMethod(
     f="initialize",
     signature="UnBoundLinDecompOp",
     definition=function #internal constructor 
     ### This mehtod is intendet to be used internally. It may change in the future.
     ### For user code it is recommended to use one of the generic constructor \code{UnBoundLinDecompOp} instead.
     (.Object,matFunc=function(){})
     {
        .Object@matFunc=matFunc
     return(.Object)
     }
)
############################constructors####################
setMethod(
      f="UnBoundLinDecompOp",
      ### 
      signature=c(matFunc="function"),
      definition=function # construct from matrix valued function
      ### This method creates a UnBoundLinDecompOp from a matrix
      ### The operator is assumed to act on the vector of carbon stocks
      ### by multiplication of the (time dependent) matrix from the left.
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

############################methods####################
setMethod(
    f="getFunctionDefinition",
    signature="UnBoundLinDecompOp",
    definition=function # creates a constant timedependent function and returns it
      ### The method creates a timedependent function from the existing matrix describing the operator 
    (object){
      return(object@matFunc)
    }
)
setMethod(
    f="getTimeRange",
    signature="UnBoundLinDecompOp",
    definition=function # return an (infinite) time range since the function is assumed to be valid for all times
    ### some functions dealing with DecompOps in general rely on this
    ### so we have to implement it even though the timerange is always the same: (-inf,inf)
    (object)
    {
        return( c("t_min"=-Inf,"t_max"=Inf))
    }
)
setMethod(
      f="BoundLinDecompOp",
      signature=c(map="UnBoundLinDecompOp",starttime="missing",endtime="missing"),
      definition=function # convert a UnBoundLinDecompOp to a BoundLinDecompOp
      ### The method creates a BoundLinDecompOp consisting of a constant time dependent function 
      ### and the limits of its domain (starttime and endtime) set to -Inf and Inf respectively
      (map){
      f=getFunctionDefinition(map)
      return(BoundLinDecompOp(starttime=-Inf,endtime=Inf,map=f))
     }
)
