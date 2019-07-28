setClass(
   Class="ScalarTimeMap",
   contains=c('TimeMap')
)

#' constructor 
#'
#' The arguments are passed through to create a function of time (an object of class \code{\link{TimeMap}}) which is susequently checked to yield a scalar numeric value
ScalarTimeMap<-function(...){
    # create a time map Object
    # fixme mm :
    # Instead of using all the constructors of TimeMap we could be 
    # transfer some of them here wich would give us more flexibility.
    # since we know more specifically what we want here.

    tm=TimeMap(...)
    # check if it defines a scalar valued functiont
    f=getFunctionDefinition(tm)
    ts=getTimeRange(tm)[1]
    val<-f(ts)
    if ( !inherits(val,'numeric')){
        stop("The created TimeMap must return numeric values.")
    }
    if (length(val)!=1) {
        print(val)
        stop("The created TimeMap must return a scalar.")
    }
    as(tm,'ScalarTimeMap')
}


