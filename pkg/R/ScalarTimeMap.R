#' constructor 
#'
#setMethod(
#  f= ScalarTimeMap<-function(...){
#    signature(
#            map="numeric'
#
#    tm=TimeMap(...)
#    # check if it defines a scalar valued functiont
#    f=getFunctionDefinition(tm)
#    ts=getTimeRange(tm)[1]
#    val<-f(ts)
#    if ( !inherits(val,'numeric')){
#        stop("The created TimeMap must return numeric values.")
#    }
#    if (length(val)!=1) {
#        print(val)
#        stop("The created TimeMap must return a scalar.")
#    }
#    as(tm,'ScalarTimeMap')
#}

#' @auto
#'
#' special case for a time map from a constant 
setMethod(
  f="ScalarTimeMap",
  signature=signature(
    map="missing" ,
    starttime="missing",
    endtime="missing",
    times="missing",
    data="numeric"
  ),
  def=function 
  (
   starttime=-Inf,
   endtime=+Inf,
   data,
   lag=0 
  ){
    if(length(data)>1) {stop("if the data argument is a numeric vector it has to be of length 1. If you want an interpolating function there are methods that accept an additional times argument, or a list or dataframe for the data argument")
    }else{
      new(
        "ScalarTimeMap",
        map=function(t){data},
        starttime=-Inf,
        endtime=Inf
      )
    }
  }
)


