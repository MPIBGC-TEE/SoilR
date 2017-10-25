#
# vim:set ff=unix expandtab ts=2 sw=2:

### defines a (time dependent) mapping including the function definition and the ### domain where the function is well defined.  This can be used to avoid interpolations out of range when mixing different time dependent data sets
setClass(
   Class="TimeMap",
   #contains="UnlimitedTimeMap",
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
##########################################################################
setMethod(
    f="TimeMap",
    signature=c(
      map="data.frame",
      starttime="missing",
      endtime="missing"
      #lag="numeric",
      #interpolation="function"
    ),
    definition=function # constructor
    ### create a  TimeMap object by interpolating the data.frame 
    (
      map,##<< a data frame containing two columns
      lag=0, ##<< a time delay
      interpolation=splinefun ##<< the interpolating function
    ){
    # build dummy object
    obj=new(Class="TimeMap")
    # use the method inherited from TimeMap
    obj=fromDataFrame(obj,map,lag=0,interpolation=splinefun)
    return(obj)
### An object of class TimeMap that contains the interpolation function and the limits of the time range where the function is valid. Note that the limits change according to the time lag
### this serves as a saveguard for Model which thus can check that all involved functions of time are actually defined for the times of interest  
}
)
###########################################################################
setMethod(
    f="TimeMap",
    signature=c(
    map="function",
    starttime="numeric",
    endtime="numeric"
    ),
    definition=function # constructor
    ### create a  TimeMap object from the function definition and the time interval  
    (map, ## the R function 
    starttime,  
    endtime,
    lag=0 ##<< delay 
    ){
    new("TimeMap",map=map,starttime=starttime,endtime=endtime)
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
setMethod(
      f="BoundLinDecompOp",
      signature=c(map="TimeMap",starttime="missing",endtime="missing"),
      definition=function # create a BoundLinDecompOp from a TimeMap
      ### The method is used internally to convert TimeMap objects to BoundLinDecompOp where the use of TimeMap is now deprecated.
      (map){
      starttime=map@starttime
      endtime=map@endtime
      map=map@map
      return(BoundLinDecompOp(map,starttime,endtime))
     }
     )
#########################################################
setMethod(
      f="BoundInFlux",
      signature=c("TimeMap","missing","missing","missing","missing"),
      definition=function # convert to BoundInFlux
      ### The method is used internally to convert TimeMap objects to BoundInFlux objects, since the use of TimeMap objects is now deprecated.
      (map){
      starttime=map@starttime
      endtime=map@endtime
      map=map@map
      return(BoundInFlux(map,starttime,endtime))
     }
     )
#########################################################
# helpers (to be used also by child classes)
#########################################################
setMethod(
    f="fromDataFrame",
    signature=c(  
      # the first Argument is just here to make the method inheritable
      # by subclasses just like initilize
      .Object="TimeMap",
      map="data.frame"
    ),
    definition=function # assemble the objects components
    ### The method is used internally to convert TimeMap objects to BoundInFlux objects, since the use of TimeMap objects is now deprecated.
    (
      .Object,
      map,
      lag=0,
      interpolation=splinefun
    ){
     t=map[,1]  
     y=map[,2]  
     o=order(t)
     tyo=cbind(t[o],y[o])
     to=tyo[,1]
     yo=tyo[,2]
     t_start=min(to)
     t_end=max(t)
     func_map=interpolation(to,yo)

    .Object@starttime=t_start
    .Object@endtime=t_end
    .Object@map=func_map
    .Object@lag=lag
    return(.Object)
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
   warning("This function is going deprecated for 2 reasons:\n
      1.) There are new more specialized classes to replace it.( BoundedInFlux,BoundedLinDecompOp)\n
          Please consider one of those if this is what you need.
      2.) Constructors for SoilR classes have been renamed consistently to the name of the class (NameOfClass( ) instead of NameOfClass.new() )
          
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
