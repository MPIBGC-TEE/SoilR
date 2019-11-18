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

#' constructor for data and times given as vectors
#' 
#' @param times (the times for the values in data)
#' @param data the timeline
#' @param lag a (scalar) delay
#' @param interpolation the interpolation, usually splinefun or approxfun
#' @autocomment 
setMethod(
  f="TimeMap",
  signature=signature(
    map="missing" ,
    starttime="missing",
    endtime="missing",
    times="numeric",
    data="numeric"
  ),
  def=function 
  (
   times,
   data,
   lag=0, 
   interpolation=splinefun 
  )
  {
		lt <- length(times)
    ll <- length(lag)
    targetClass <- class(lag)
    if (ll>1){
      stop('For ScalarTimeMap only scalar lags are allowed')
    }
    if(!is.null(dim(data))){
      stop('For ScalarTimeMap only a vector of data is allowed are allowed')
    }
    scalarFuncMaker(
      times,
      scalar_lag=lag,
      y_vector=data,
      interpolation
    ) 
  }
)

#' manual constructor for a function and an interval
#'
setMethod(
    f="ScalarTimeMap",
    signature=signature(
      map="function",
      starttime="numeric",
      endtime="numeric",
      times="missing",
      data="missing"
    ),
    definition=function(
        map, 
        starttime,  
        endtime,
        lag=0
    ){
        new(
            "ScalarTimeMap"
            ,map=function(t){
                map(t-lag)
             }
            ,starttime=starttime
            ,endtime=endtime
        )
  }
)

#' manual constructor for just a function 
#'
#' The interval will be set to [-Inf,Inf]
setMethod(
    f="ScalarTimeMap",
    signature=signature(
      map="function",
      starttime="missing",
      endtime="missing",
      times="missing",
      data="missing"
    ),
    definition=function (map,lag=0){
        new(
            "ScalarTimeMap"
            ,map=function(t){
                map(t-lag)
            }
            ,starttime=-Inf
            ,endtime=Inf
        )
    }
)

#' special case for a time map from a constant 
#'
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


