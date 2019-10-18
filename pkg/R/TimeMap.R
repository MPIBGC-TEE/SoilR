arrFuncMaker <- function(times,arr,srcDim,targetClass,interpolation,lag=0){
  flatDim <- dim(arr)[[1]]
  if(length(lag)==1){
    lag <- rep(lag,flatDim)
  }
  funcMaker <- function(i){
    interpolation(x=times+lag[i],y=arr[i,])
  }
	funcs <- lapply(seq(flatDim),funcMaker)
	arrFunc <- function(t){
		as(
      array(dim=srcDim,data=unlist(lapply(funcs,function(f){f(t)}))),
      targetClass)
	}
  return(arrFunc)
}
flat_arr_TimeMap <- function(times,arr,srcDim,targetClass,interpolation,lag=0){
		return(
      TimeMap(
        map=arrFuncMaker(times,arr,srcDim,targetClass,interpolation,lag=lag),
        min(times)+max(lag),
        max(times)+min(lag)
      )
    )
}
plotAll<- function(tr,valMin,valMax,times,values,srcDim){
  plotCoordSystem(tr,valMin,valMax)
  plotTrajectories(valMin,valMax,times,values,srcDim)
}
plotCoordSystem <- function(tr,valMin,valMax)
      plot(0,0,
      type='n',
      xlab='time',
      ylab='values',
      xlim=tr,
      ylim=c(valMin,valMax+valMax-valMin)
    )
plotTrajectories <- function(valMin,valMax,times,values,srcDim){
    flatDim <- dim(values)[[1]]
    colors <- rainbow(flatDim)
    plotFun <- function(i){
      y <- values[i,]
      lines(x=times,y=y,col=colors[[i]])
    }
    flatInds <- 1:flatDim
    lapply(flatInds, plotFun)
    legend(x=min(times),y=2*valMax-valMin,lapply(flatInds,function(i){as.character(arrayInd(.dim=srcDim,i))}),colors)
}
CallWithPlotVars<- function(
  obj,
  workerFunc,
  varNamesFromPackageEnv,
  ...
){
    lt <- 40
    f <- getFunctionDefinition(obj)
    tr <- getTimeRange(obj)
    times <- seq(min(tr),max(tr),length.out=lt)
    f_1 <- f(times[[1]]) 
    flatDim <- length(f_1)
    srcDim <- dim(f_1)
    if(is.null(srcDim)){ 
      srcDim <- flatDim
    }
    values <- array(
      dim=c(flatDim,lt),
      unlist(
        lapply(
          times,
          function(t){f(t)}
        )
      )
    )
    valMin <- min(values)
    valMax <- max(values)
	  e <- environment()
	  valuesProvidedByThisFuntions <- as.list(e)[varNamesFromPackageEnv]
    valuesProvidedByCaller <- list(...)
	  values <- c( valuesProvidedByThisFuntions, valuesProvidedByCaller) 
  	funcCall <- as.call(append(list(workerFunc),values))
  	res <- eval(funcCall)
  res
}


#' @auto

#' @auto

#' @auto
setMethod(
    f="initialize",
    signature="TimeMap",
    definition=function 
    (.Object,
    starttime=numeric(),
    endtime=numeric(),
    map=function(t){t}
    ){
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
  f="TimeMap",
  signature=signature(
    map="missing" ,
    starttime="missing",
    endtime="missing",
    times="numeric",
    data="list"
  ),
  def=function 
  (
   times, 
   data,  
   lag=0, 
   interpolation=splinefun 
  ){
		lt <- length(times)
    ll <- length(lag)
    dl <- dim(lag)
    fe <- data[[1]]
		targetClass <- class(fe)
    if(inherits(fe,'numeric')){
      if(length(fe)==1){ 
        return(
          TimeMap(
            times=times,
            data=unlist(data),
            lag=lag,
            interpolation=interpolation
          )
        )
      }else{ 
        srcDim <- c(length(fe))
		    flatDim <- prod(srcDim)
        if(ll>1){ 
          if(is.null(dl)){ 
            if(ll!=flatDim){
              stop(
                sprintf('If data is a list of vectors, 
                  the lag element has to be a scalar or a vector of the same legth as
                  the elemenst of the data list. 
                  You gave a lag argument that was a vector of length %s ,
                  while the elements of the data list have length %s',
                  ll,
                  toString(flatDim)
                )
              )
            }
          }else{ 
            stop(
              sprintf('If data is a list of vectors 
                the lag element has to be a scalar or a vector too.
                You gave a lag argument that was an array or matrix of dimension %s ,
                while the elements of the data list where vectors (dim=NULL)' ,
                toString(srcDim)
              )
            )
          }
        }
      }
		  arr <- array(dim=c(flatDim,lt),data=unlist(lapply(data,as.vector)))
    }else{
      if(inherits(fe,'array')|inherits(fe,'matrix')){
		    srcDim <- dim(fe)
		    flatDim=prod(srcDim)
        if(ll>1){ 
          if(is.null(dl)){ 
            stop(
              sprintf('If data is a list of matrices or arrays, 
                the lag element has to either be a scalar or  
                a matrix or array of the same shape as the elemenst of the data list. 
                You gave a lag argument that was a vector of length %s,
                while the elements of the data list have dimension %s.',
                ll,
                toString(srcDim)
              )
            )
          }else{
            if(!identical(dl,srcDim)){
              stop(
                sprintf('If data is a list of matrices or arrays, 
                  the lag element has to either be a scalar or  
                  a matrix or an array of the same shape as the elemenst of the data list. 
                  You gave a lag argument of dimension %s,
                  while the elements of the data list have dimension %s.',
                  toString(dl),
                  toString(srcDim)
                )
              )
            }
          }
        }
		    arr <- array(dim=c(flatDim,lt),data=unlist(lapply(data,as.vector)))
      }else{
        stop(
          sprintf(
            'The elements of the data list must be arrays, matrices or vectors but you provided an object of class %s.',
             class(fe)
          )
        )
      }
    }
		return(
      flat_arr_TimeMap(times,arr,srcDim,targetClass,interpolation,lag=lag)
    )
  }
)

#' @auto

#' @auto

#' @auto
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
      ld <- dim(lag)
      if(!is.null(ld)){
        srcDim <- ld
      }else{
        srcDim <- ll
      }
		  flatDim=prod(srcDim)
		  arr <- array(
        dim=c(flatDim,lt),
        as.vector(unlist( lapply( data, function(de){rep(de,flatDim)})))
      )
    }else{
      srcDim <- 1
		  flatDim=prod(srcDim)
		  arr <- array(dim=c(flatDim,lt),data=data)
    }
		return(
      flat_arr_TimeMap(times,arr,srcDim,targetClass,interpolation,lag=lag)
    )
  }
)

#' @auto

#' @auto

#' @auto
setMethod(
  f="TimeMap",
  signature=signature(
    map="missing" ,
    starttime="missing",
    endtime="missing",
    times="numeric",
    data="matrix"
  ),
  def=function 
  (
   times,
   data, 
   lag=0,
   interpolation=splinefun 
  ){
    srcDim <-c(dim(data)[[1]])
    flatDim=prod(srcDim)
    arr <- data
    targetClass <-'numeric'
    dl <- dim(lag)
    ll <- length(lag)
    if (ll>1){ 
      if (is.null(dl)){ 
          if (ll!=flatDim){
            stop(
              sprintf(
                'The lenght of a column of the matrix (refering to one timestep) was %s while
                the lag parameter had length %s.',
                flatDim,
                ll
              )
            )
          }
       }else{ 
         if(!identical(dl,c(flatDim,1))){
          stop(
            sprintf(
              'If data is a matrix then the slices for every timestep are vectors.
               In this case lag has to be either scalar or a vector of the same size as the columns
               of data. dim(lag)=%s, but should have been %s,1',
               toString(dl),
               flatDim
            )
          )
        }
      }
    }
    return(
      flat_arr_TimeMap(times,arr,srcDim,targetClass,interpolation,lag=lag)
    )
  }
)

#' @auto

#' @auto

#' @auto
setMethod(
  f="TimeMap",
  signature=signature(
    map="missing" ,
    starttime="missing",
    endtime="missing",
    times="numeric",
    data="array"
  ),
  def=function 
  (
   times,
   data,
   lag=0, 
   interpolation=splinefun 
  ){
    dd <- dim(data)
    dl <- dim(lag)
    ll <- length(lag)
    srcDim <-dd[1:(length(dd)-1)] 
	  flatDim=prod(srcDim)
    if(ll>1){ 
      if(is.null(dl)){
        stop(
          sprintf('The lag element has to either be a scalar or  
            have the same dimension as the array slices per time step.
            You gave a lag argument that was a vector of length %s ,while  dim(data)[1:(length(dim(data))-1)]=%s',
            ll,
            toString(srcDim))
          )
      }else{
        if(ll!=flatDim){
        stop(
          sprintf('The lag element has to either be a scalar or  
            have the same dimension as the array slices per time step.
            dim(lag)=%s, dim(data)[1:(length(dim(data))-1)]=%s',
            dl,
            toString(srcDim))
        )
      }
    }
  }
  lt <- length(times)
	arr <- array(dim=c(flatDim,lt),data=as.vector(data))
	targetClass <-'array'
	return(
    flat_arr_TimeMap(times,arr,srcDim,targetClass,interpolation,lag=lag)
  )
 }
)

#' @auto

#' @auto

#' @auto
setMethod(
  f="TimeMap",
  signature=signature(
    map="list" ,
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
  ){
	  if (length(map)<2){
	  	stop('Your list has to have at least 2 elements: a vector usually labeled "times" and a list of arrays or matrices.')
	  }
    targetNames <- c('times','data')
    if(identical(intersect(targetNames,names(map)),targetNames)){
	    times <- map[['times']]
	    data  <- map[['data']]
    }else{
	   times <- map[[1]]
	   data  <- map[[2]]
    }
    return(TimeMap(times=times,data=data,lag=lag,interpolation=interpolation))
  }
)

#' @auto

#' @auto

#' @auto
setMethod(
    f="TimeMap",
    signature=signature(
      map="data.frame",
      starttime="missing",
      endtime="missing",
      times="missing",
      data="missing"
    ),
    definition=function 
    (
      map,
      lag=0, 
      interpolation=splinefun 
    ){
      obj <- TimeMap(times=as.vector(map[,1]),data=as.vector(map[,2]),lag=lag,interpolation=interpolation)
     return(obj)
}
)

#' manual constructor for a function and an interval
setMethod(
    f="TimeMap",
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
            "TimeMap"
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
    f="TimeMap",
    signature=signature(
      map="function",
      starttime="missing",
      endtime="missing",
      times="missing",
      data="missing"
    ),
    definition=function (map,lag=0){
        new(
            "TimeMap"
            ,map=function(t){
                map(t-lag)
            }
            ,starttime=-Inf
            ,endtime=Inf
        )
    }
)

#' @auto

#' @auto

#' @auto
setMethod(
    f="TimeMap",
    signature=signature(
      map="TimeMap"
    ),
    definition=function 
    (map 
    ){
   map 
  }
)

#' @auto

#' @auto

#' @auto
setMethod(
    f="as.character",
    signature=c(x="TimeMap"),
    definition=function 
    (x, 
     ... 
     ){
        return(
            paste( class(x),
                  "(\n starttime=",
                  x@starttime,
                  "\n endtime=",
                  x@endtime,
                  ")",
                  sep=""
            )
        )
    }
)    

#' The time interval where the function is defined
setMethod(
    f="getTimeRange",
    signature="TimeMap",
    definition=function 
    (object 
    ){
        return( c("t_min"=object@starttime,"t_max"=object@endtime))
    }
)

#setMethod(
#    f="getLaggingTimeRange",
#    signature="TimeMap",
#    definition=function 
#    (object 
#    ){
#        lag=object@lag
#        return( c("t_min"=object@starttime+lag,"t_max"=object@endtime+lag))
#    }
#)

#' @auto

#' @auto

#' @auto
setMethod(
    f="getFunctionDefinition",
    signature="TimeMap",
    definition=function(object){
        return(object@map)
    }
)

#' deprecated constructor of the class TimeMap.
#' 
#' deprecated functions #################### use the generic TimeMap(...)
#' instead
#' 
#' 
#' @param t_start A number marking the begin of the time domain where the
#' function is valid
#' @param t_end A number the end of the time domain where the function is valid
#' @param f The time dependent function definition (a function in R's sense)
#' @return An object of class TimeMap that can be used to describe models.
TimeMap.new=function
(t_start, 
 t_end,   
 f        
 ){
   warning("This function is going deprecated because:\n
      Constructors for SoilR classes have been renamed consistently to the name of the class (NameOfClass( ) instead of NameOfClass.new() )
   ")
   obj=TimeMap(map=f,t_start,t_end) 
return(obj)
}






#' TimeMap.from.Dataframe
#' 
#' This function is a deprecated constructor of the class TimeMap.
#' 
#' 
#' @param dframe A data frame containing exactly two columns: the first one is
#' interpreted as time
#' @param lag a scalar describing the time lag. Positive Values shift the
#' argument of the interpolation function forward in time. (retard its effect)
#' @param interpolation A function that returns a function the default is
#' splinefun. Other possible values are the linear interpolation approxfun or
#' any self made function with the same interface.
#' @return An object of class TimeMap that contains the interpolation function
#' and the limits of the time range where the function is valid. Note that the
#' limits change according to the time lag this serves as a saveguard for Model
#' which thus can check that all involved functions of time are actually
#' defined for the times of interest
TimeMap.from.Dataframe=function
(dframe, 
lag=0, 
interpolation=splinefun 
 ){
   warning(
   "This function will be deprecated because constructors are now called like the classes they produce objects of.(In this case TimeMap(...)
   These constructors are frequently generic functions and implement 
   (usually many) methods with different signatures (types of arguments) 
   to  assemble the object from different components.
   You can find all methods by typing 
   'getMethod('TimeMap')'")
   obj=TimeMap(dframe,interpolation=interpolation) 
return(obj)
}

#' @auto

#' @auto

#' @auto
setMethod(
  f= "plot",
  signature=c(x="TimeMap"),
  def=function(
    x,
    ...
    ){
    CallWithPlotVars(
      x,
      workerFunc=plotAll,
      varNamesFromPackageEnv=c('tr','valMin','valMax','times','values','srcDim')
    )
  }
)

#' @auto

#' @auto

#' @auto
setMethod(
  f= "add_plot",
  signature=c(x="TimeMap"),
  def=function 
  (
    x,  
    ... 
    ){
    CallWithPlotVars(
      x,
      workerFunc=plotTrajectories,
      varNamesFromPackageEnv=c('valMin','valMax','times','values','srcDim')
    )
  }
)
