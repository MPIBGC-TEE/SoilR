#' constructor for data given as 2 column data.frame 
#' 
#' @param map In this case a data.frame. Only the first two columns will be
#' used
#' @param lag a (scalar) delay
#' @param interpolation the interpolation, usually splinefun or approxfun
#' @autocomment 
setMethod(
  f="ScalarTimeMap",
  signature=signature(
    map="data.frame",
    starttime="missing",
    endtime="missing",
    times="missing",
    data="missing" 
  ),
  def=function 
  (
   map,
   lag=0, 
   interpolation=splinefun 
  )
  {
    # interpret the first column as times and the second as values
    # and the sister method for signature(times="numeric",data="numeric")  
    ScalarTimeMap(times=map[,1], data=map[,2],lag=lag,interpolation=interpolation)
     
  }
)

#' constructor for data and times given as vectors
#' 
#' @param times (the times for the values in data)
#' @param data the values at times
#' @param lag a (scalar) delay
#' @param interpolation the interpolation, usually splinefun or approxfun
#' @autocomment 
setMethod(
  f="ScalarTimeMap",
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
    f<-scalarFuncMaker(
      times,
      scalar_lag=lag,
      y_vector=data,
      interpolation
    ) 
    # call the sister method for a given function recursively
    ScalarTimeMap(map=f,starttime=min(times),endtime=max(times))
  }
)

#' manual constructor for a function and an interval
#'
#' @param map a function
#' @param starttime initial time of simulation
#' @param endtime end time of simulation
#' @param lag a time lag
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
#' @param map a function
#' @param lag a time lag
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
#' @param starttime initial time of simulation
#' @param endtime end time of simulation
#' @param data data
#' @param lag a time lag
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


