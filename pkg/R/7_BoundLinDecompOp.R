#
# vim:set ff=unix expandtab ts=2 sw=2:

setClass(# a decomposition operator described by a matrix valued function of time
    Class="BoundLinDecompOp",
    contains=c("DecompOp","TimeMap"),   
    slots=list(
    map="function"
    ,
    starttime="numeric"
    ,
    endtime="numeric"
    ) 
    
   )
#---------------------------------------------------------------------
setMethod(
      f="BoundLinDecompOp",
      signature=c(map="function",starttime="numeric",endtime="numeric"),
      definition=function # a constructor 
      ### This method creates a BoundLinDecompOp from a timedependent function and its domain 
      (
        map, ##<<a function
        starttime,##<< the begin of the time domain
        endtime ##<< the end of the time domain
      ){
      return(new("BoundLinDecompOp",starttime=starttime,endtime=endtime,map=map))
    }
)
#---------------------------------------------------------------------
setMethod(
      f="BoundLinDecompOp",
      signature=c(
        map="data.frame",
        starttime="missing",
        endtime="missing"
        ),
      definition=function # a constructor 
      ### This method creates a BoundLinDecompOp from a dataframe
      (
        map
        #starttime,
        #endtime
      ){
      # build dummy object
      obj=new(Class="BoundLinDecompOp")
      # use the method inherited from TimeMap
      obj=fromDataFrame(obj,map,lag=0,interpolation=splinefun)
      return(obj)
    }
)

