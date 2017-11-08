#
# vim:set ff=unix expandtab ts=2 sw=2:

### defines a (time dependent) mapping including the function definition and the ### domain where the function is well defined.  This can be used to avoid interpolations out of range when mixing different time dependent data sets
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
    map=function(t){t},
    lag=0
    ){
    .Object@starttime=starttime
    .Object@endtime=endtime
    .Object@map=map
    .Object@lag=lag
    return(.Object)
    }
)
#-----------------------------------------------------------
# fixme mm:
# the method is doing a lot.
# we could add two other constructors for TimeMap that have the elements of the list
# as separated arguments where the first one is always a numeric vectors
# and the second one either an array or a list.
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
   map
  ){
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
		lt <- length(times)
    if(inherits(data,'list')){
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
              'The elements of the data list must be a arrays, matrices or vectors but you provided an object of class %s.',
               class(fe)
            )
          )
       }
     }
    }else{
      if(inherits(data,'array')){ 
        dd <- dim(data)
        srcDim <-dd[1:(length(dd)-1)] 
		    flatDim=prod(srcDim)
		    arr <- array(dim=c(flatDim,lt),data=as.vector(data))
		    targetClass <-'array'
      }else{
        if(inherits(data,'matrix')){
          # R insists that a 2D array is a matrix and NOT an array which is extremely weierd
          # checkout: inherits(array(dim=c(2,2),'array')) which yields FALSE since
          # class(array(dim=c(2,2)) yields 'matrix' while 
          # class(array(dim=c(2)) and class(array(dim=c(2,2,2)) both yield 'array'
          # se we have to allow the class 'matrix' here for the array in case somebody tried to create 2D array 
          # ending up with a matrix ...
          srcDim <-c(dim(data)[[1]])
		      flatDim=prod(srcDim)
		      arr <- data
		      targetClass <-'numeric'
        }else{
          if(inherits(data,'numeric')){
          srcDim <- 1
		      flatDim=prod(srcDim)
		      arr <- array(dim=c(flatDim,lt),data=data)
		      targetClass <-'numeric'
          }else{
            stop(
              sprintf(
                'The data element of the list must be a list an array, matrix or a vector but
                 you provided an object of class %s.',
                class(data)))
          }
        }
      }
    }
    funcMaker <- function(i){splinefun(x=times,y=arr[i,])}
		# cut out a time line for every index in the flattene vector
		funcs <- lapply(seq(flatDim),funcMaker)
		arrFunc <- function(t){
			as(
        array(dim=srcDim,data=unlist(lapply(funcs,function(f){f(t)}))),
        targetClass)
		}
		return(TimeMap(map=arrFunc,min(times),max(times)))
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
    # build dummy object
     t=map[,1]  
     y=map[,2]  
     o=order(t)
     tyo=cbind(t[o],y[o])
     to=tyo[,1]
     yo=tyo[,2]
     t_start=min(to)
     t_end=max(t)
     func_map <- interpolation(to,yo)
     obj <- new("TimeMap",map=func_map,starttime=t_start,endtime=t_end,lag=lag)
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
    endtime,
    lag=0 ##<< delay
    ){
    new("TimeMap",map=map,starttime=starttime,endtime=endtime,lag=lag)
  }
)
###########################################################################
setMethod(
    f="TimeMap",
    signature=signature(
      map="TimeMap"
    ),
    definition=function # pass through constructor
    ### 
    (map ##<< The function just returns its argument and helps to avoid a lot of checks if a conversion is necessary.
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
                  #fixme:lag is missing
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
        ### for which the TimeMap object has been defined.
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

#########################################################
#converters
#########################################################
# helpers (to be used also by child classes)
#########################################################
#setMethod(
#    f="fromDataFrame",
#    signature=c(  
#      # the first Argument is just here to make the method inheritable
#      # by subclasses just like initilize
#      .Object="TimeMap",
#      map="data.frame"
#    ),
#    definition=function # assemble the objects components
#    ### The method is used internally to convert TimeMap objects to BoundInFlux objects.
#    (
#      .Object,
#      map,
#      lag=0,
#      interpolation=splinefun
#    ){
#     t=map[,1]  
#     y=map[,2]  
#     o=order(t)
#     tyo=cbind(t[o],y[o])
#     to=tyo[,1]
#     yo=tyo[,2]
#     t_start=min(to)
#     t_end=max(t)
#     func_map=interpolation(to,yo)
#
#    .Object@starttime=t_start
#    .Object@endtime=t_end
#    .Object@map=func_map
#    .Object@lag=lag
#    return(.Object)
#     }
#)
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
