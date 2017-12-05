#
# vim:set ff=unix expandtab ts=2 sw=2:
arrFuncMaker <- function(times,arr,srcDim,targetClass,interpolation,lag=0){
  flatDim <- dim(arr)[[1]]
  if(length(lag)==1){
    lag <- rep(lag,flatDim)
  }
  funcMaker <- function(i){
    interpolation(x=times+lag[i],y=arr[i,])
  }
	# cut out a time line for every index in the flattene vector
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
  
### This class enhances a time dependent function by information about its domain.
### The information about the delay is especially usefull for functions that interpolate data. 
### Assume that you are given time series data in two vectors \code{times}, \code{values}.
### You can create an interpolating function with  \code{\link{splinefun}} or \code{\link{approxfun}}
### \code{f  <- splinefun(x=times,y=values) } 
### \code{f(t)} will yield sensible values for \eqn{$\min_{t \in times}\le t \le max_{t \in times}$.}{min(times)<t<max(times).} 
### but will produce unreasonable values for any t outside these limits.
### Unfortunately the interpolating functions produced by 
### \code{\link{splinefun}} or \code{\link{approxfun}} do not retain any information 
### about their domain which makes it possible to accidentatly 
### apply them to times not at all supported by the original data. 
### This would not even cause errors in the code but silently corrupt the results.
### To help you to keep track of the domains of the many time dependent functions used in SoilR's 
### Models this class \code{\linkS4class{TimeMap}} stores the \code{starttime} 
### and \code{endtime} values
### along with the function represented by \code{map}.
### SoilR functions that accept time series data will normally convert it to 
### subclasses  \code{TimeMap-class} automatically but you can do it explicitly.
setClass(
   Class="TimeMap",
   slots=list(
      map="function"
      ,
      lag="numeric"
      ,
      starttime="numeric"
      ,
      endtime="numeric"
   )
)
##########################################################################
# constructors
setMethod(
    f="initialize",
    signature="TimeMap",
    definition=function #initialize called by (new)
    ### This is the standard constructor for objects of this class
    ### It is called by statements of the form 
    ### \code{new("TimeMap",start,end,f,lag)}
    ### but should be avoided by user code since it is much more 
    ### likely to change.
    ### Rather use the generic constructor TimeMap(...)
    ### which implements a lot of methods 
    (.Object,
    starttime=numeric(),
    endtime=numeric(),
    map=function(t){t}
    #,
    #lag=0
    ){
    .Object@starttime=starttime
    .Object@endtime=endtime
    .Object@map=map
    #.Object@lag=lag
    return(.Object)
    }
)
#-----------------------------------------------------------
setMethod(
  f="TimeMap",
  signature=signature(
    map="missing" ,
    starttime="missing",
    endtime="missing",
    times="numeric",
    data="list"
  ),
  ### The method creates an instance of \code{\link{TimeMap-class}}
  ### from a vector of times and a list of the same length, 
  ### containing vectors matrices or arrays 
  def=function # Create a TimeMap from a nested list 
  (
   times,
   data,
   lag=0,
   interpolation=splinefun
  ){
		lt <- length(times)
    fe <- data[[1]]
		#remember the class of the data elements
		targetClass <- class(fe)
    if(inherits(fe,'numeric')){
      # we have a list of vectors
      srcDim <- c(length(fe))
		  flatDim <- prod(srcDim)
		  arr <- array(dim=c(flatDim,lt),data=unlist(lapply(data,as.vector)))
    }else{
      if(inherits(fe,'array')|inherits(fe,'matrix')){
		    #remember the shape of the data elements
		    srcDim <- dim(fe)
		    flatDim=prod(srcDim)
		    # create a 2D array 
		    # with the elements of data flattened to vectors 
		    # and  time as second dimension
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
#-----------------------------------------------------------
setMethod(
  f="TimeMap",
  signature=signature(
    map="missing" ,
    starttime="missing",
    endtime="missing",
    times="numeric",
    data="numeric"
  ),
  ### The method creates an instance of \code{\link{TimeMap-class}}
  ### from a  vector of times and an array referring to it.
  def=function # Create a TimeMap from a nested list 
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
      # Although we have only one datapoint per time step the
      # a vector (matrix or array) lag makes the result a 
      # vector(matrix/array) valued function that yields values
      # of the same dimension as lag.
      # This is the same result as if the the data argument
      # had been a matrix/array with identical entries.
      # So we first create this matrix and then delegate to
      # the method that takes a data argument of type matrix/array
      ld <- dim(lag)
      if(!is.null(ld)){
        srcDim <- ld
      }else{
        #lag was a vector
        srcDim <- ll
      }
		  flatDim=prod(srcDim)
		  arr <- array(
        dim=c(flatDim,lt),
        as.vector(unlist( lapply( data, function(de){rep(de,flatDim)})))
      )
    }else{
      # lag was a scalar 
      srcDim <- 1
		  flatDim=prod(srcDim)
		  arr <- array(dim=c(flatDim,lt),data=data)
    }
		return(
      flat_arr_TimeMap(times,arr,srcDim,targetClass,interpolation,lag=lag)
    )
  }
)
#-----------------------------------------------------------
setMethod(
  f="TimeMap",
  signature=signature(
    map="missing" ,
    starttime="missing",
    endtime="missing",
    times="numeric",
    data="matrix"
  ),
  ### The method creates an instance of \code{\link{TimeMap-class}}
  ### from a  vector of times and an array referring to it.
  def=function # Create a TimeMap from a nested list 
  (
   times,
   data,
   lag=0,
   interpolation=splinefun
  ){
    # R insists that a 2D array is a matrix and NOT an array which is extremely weierd
    # checkout: inherits(array(dim=c(2,2),'array')) which yields FALSE since
    #[MaR class(array(dim=c(2,2)) yields 'matrix' while 
    # class(array(dim=c(2)) and class(array(dim=c(2,2,2)) both yield 'array'
    # se we have to allow the class 'matrix' here for the array in case 
    # some poor unsuspecting fellow tries to create 2D array 
    # but ends up with a matrix ...
    srcDim <-c(dim(data)[[1]])
		flatDim=prod(srcDim)
		arr <- data
		targetClass <-'numeric'

		return(
      flat_arr_TimeMap(times,arr,srcDim,targetClass,interpolation,lag=lag)
    )
  }
)
#-----------------------------------------------------------
setMethod(
  f="TimeMap",
  signature=signature(
    map="missing" ,
    starttime="missing",
    endtime="missing",
    times="numeric",
    data="array"
  ),
  ### The method creates an instance of \code{\link{TimeMap-class}}
  ### from a  vector of times and an array whose last dimension   ### is referring to it.
  def=function # Create a TimeMap from times and array 
  (
   times,
   data,
   lag=0,
   interpolation=splinefun
  ){
    lt <- length(times)
    dd <- dim(data)
    srcDim <-dd[1:(length(dd)-1)] 
		flatDim=prod(srcDim)
		arr <- array(dim=c(flatDim,lt),data=as.vector(data))
		targetClass <-'array'
		return(
      flat_arr_TimeMap(times,arr,srcDim,targetClass,interpolation,lag=lag)
    )
  }
)
#-----------------------------------------------------------
setMethod(
  f="TimeMap",
  signature=signature(
    map="list" ,
    starttime="missing",
    endtime="missing",
    times="missing",
    data="missing"
  ),
  ### The method creates an instance of \code{\link{TimeMap-class}}
  ### from a list that contains data and a vector of times referring to it.
  def=function # Create a TimeMap from a nested list 
  (
   map,
   lag=0,
   interpolation=splinefun
  ){
    print('#####################################list')
    print(lag)
    ##details<< The list must have two entries
    ##  If the entries are not named, the first one is supposed to be a numeric vector
    ##  of \code{times} and the second to contain the data referring to those times.
    ##  The \code{data} entry of the list can itself be a list with the same length as
    ##  the \code{times} entry or an array whose last dimension is equal to the length of 
    ##  the \code{times} entry.
    ##  If the \code{data} entry is a list the elements must 
    ##  be \code{vectors},\code{matrices} or \code{arrays}.
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
    # delegate to other mehtods that deal with the components
    return(TimeMap(times=times,data=data,lag=lag,interpolation=interpolation))
  }
)
##########################################################################
setMethod(
    f="TimeMap",
    signature=signature(
      map="data.frame",
      starttime="missing",
      endtime="missing",
      times="missing",
      data="missing"
    ),
    definition=function # constructor
    ### create a  TimeMap object by interpolating 
    ### the data.frame 
    (
      map,##<< a data frame containing two columns
      lag=0, ##<< a time delay
      interpolation=splinefun ##<< the interpolating function
    ){
      # delegate to the vector vector mehtod
      obj <- TimeMap(times=as.vector(map[,1]),data=as.vector(map[,2]),lag=lag,interpolation=interpolation)
     return(obj)
### An object of class TimeMap that contains the interpolation function and the limits of the time range where the function is valid. Note that the limits change according to the time lag
### this serves as a saveguard for Model which thus can check that all involved functions of time are actually defined for the times of interest  
}
)
###########################################################################
setMethod(
    f="TimeMap",
    signature=signature(
      map="function",
      starttime="numeric",
      endtime="numeric",
      times="missing",
      data="missing"
    ),
    definition=function # constructor
    ### create a  TimeMap object from the function definition and the time interval  
    (map, ## the R function 
    starttime,  
    endtime
    ){
    new("TimeMap",map=map,starttime=starttime,endtime=endtime)
  }
)
###########################################################################
setMethod(
    f="TimeMap",
    signature=signature(
      map="TimeMap"
    ),
    definition=function # pass through constructor
    ### The function just returns its argument.
    ### So any function that has to convert one of its argument can just call TimeMap on it 
    ### even if the argument is allready one.
    (map ##<< the object that will be returned unchanged
    ){
   map 
  }
)
##########################################################################
setMethod(
    f="as.character",
    signature=c(x="TimeMap"),
    definition=function #convert TimeMap Objects to something printable.
    ### This method is needed to print a TimeMap object.
    (x, ##<<An Object of class time map
     ... ##<< will be ignored 
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
##########################################################################
setMethod(
    f="getTimeRange",
    signature="TimeMap",
    definition=function # ask for the boundaries of the underlying time interval
    ### The method returns the time range of the given object 
    ### It is probably mostly used internally to make sure that 
    ### time dependent functions retrieved from data are not
    ### used outside the interval where they are valid. 
    
    (object ##<< An object of class TimeMap or one that inherits from TimeMap
    ){
        return( c("t_min"=object@starttime,"t_max"=object@endtime))
        ### a vector of length two \code{ c(t_min,t_max) }
        ### containing start and end time of the time interval 
        ### for which the TimeMap object is well defined,
        ### considering the initial timerange and the lag. 
        ### To see how a (possibly vectorvalued lag) changes the definition
        ### use plot on the object. 
    }
)
###########################################################################
setMethod(
    f="getFunctionDefinition",
    signature="TimeMap",
    definition=function(object){
    ### extract the function definition (the R-function) 
        return(object@map)
    }
)

#######################################################################
####################    deprecated functions        ####################
########################################################################
TimeMap.new=function# deprecated constructor of the class TimeMap.
### use the generic TimeMap(...) instead
(t_start, ##<<A number marking the begin of the time domain where the function is valid
 t_end,   ##<<A number the end of the time domain where the function is valid
 f        ##<<The time dependent function definition (a function in R's sense)
 ){
   warning("This function is going deprecated because:\n
      Constructors for SoilR classes have been renamed consistently to the name of the class (NameOfClass( ) instead of NameOfClass.new() )
          
   ")
   # delegate to new constructor
   obj=TimeMap(map=f,t_start,t_end) 
return(obj)
### An object of class TimeMap that can be used to describe models.
}
##########################################################################
TimeMap.from.Dataframe=function
### This function is a deprecated constructor of the class TimeMap.
(dframe, ##<<A data frame containing exactly two columns:
## the first one is interpreted as time
lag=0, ##<< a scalar describing the time lag. Positive Values shift the argument of the interpolation function forward in time. (retard its effect)
interpolation=splinefun ##<<A function that  returns a function  the default is splinefun. Other possible values are the linear interpolation approxfun or any self made function with the same interface.
 ){
   warning(
   "This function will be deprecated because constructors are now called like the classes they produce objects of.(In this case TimeMap(...)
   These constructors are frequently generic functions and implement 
   (usually many) methods with different signatures (types of arguments) 
   to  assemble the object from different components.
   You can find all methods by typing 
   'getMethod('TimeMap')'")
   #delegate to new constructor
   obj=TimeMap(dframe,interpolation=interpolation) 
return(obj)
### An object of class TimeMap that contains the interpolation function and the limits of the time range where the function is valid. Note that the limits change according to the time lag
### this serves as a saveguard for Model which thus can check that all involved functions of time are actually defined for the times of interest  
}
#---------------------------------------------------------------------------------------------------------
setMethod(
  f= "plot",
      signature=c(x="TimeMap"),
  def=function(x,...){
    lt <- 40
    f <- getFunctionDefinition(x)
    tr <- getTimeRange(x)
    #tr <- c(tmin=1,tmax=4)
    times <- seq(min(tr),max(tr),length.out=lt)
    f_1 <- f(times[[1]]) #could be a scalar vector matrix or array
    flatDim <- length(f_1)
    srcDim <- dim(f_1)
    if(is.null(srcDim)){ 
      srcDim <- flatDim
    }
    resultArr<- array(dim=c(flatDim,lt),unlist(lapply(times,function(t){f(t)})))
    resMin <- min(resultArr)
    resMax <- max(resultArr)
    pp('f_1')
    colors <- rainbow(flatDim)

    #plot.new()
    plot(0,0,
      type='n',
      xlab='time',
      ylab='values'
    #)
    #plot.window(
      ,
      xlim=tr,
      ylim=c(resMin,resMax+resMax-resMin)
    )
    plotFun <- function(i){
      y <- resultArr[i,]
      pe(quote(length(times)))
      pe(quote(length(y)))
      lines(x=times,y=y,col=colors[[i]])
    }
    flatInds <- 1:flatDim
    lapply(flatInds, plotFun)
    legend(x=min(tr),y=2*resMax-resMin,lapply(flatInds,function(i){as.character(arrayInd(.dim=srcDim,i))}),colors)
  }
)
