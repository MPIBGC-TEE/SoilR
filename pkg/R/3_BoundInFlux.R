#
# vim:set ff=unix expandtab ts=2 sw=2:
BoundInFlux.new=function#deprecated constructor of the class BoundInFlux use BoundInFluxNow.
### A BoundInFlux is nothing more than an usual R-function of one argument augmented by the lower and upper boundary of the interval where it is defined.
(t_start, ##<<A number marking the begin of the time domain where the function is valid
 t_end,   ##<<A number the end of the time domain where the function is valid
 f        ##<<The time dependent function definition (a function in R's sense)
 ){
   warning("The function is deprecated, constructors are now called as the class. To get rid of this warning please use BoundInFlux() in the future.")
   obj=BoundInFlux(f,t_start,t_end) 
return(obj)
### An object of class BoundInFlux that can be used to describe models.
}
#-----------------------------------------------------------------------------------

### defines a time dependent inputrate as function of time and including the domain where the function is well defined. This can be used to avoid interpolations out of range when mixing different time dependent data sets
setClass(
   Class="BoundInFlux",
   contains=c("InFlux","TimeMap"),
   slots=list(
   )
)
#---------------------------------------------------------------------
setMethod(
      f="BoundInFlux",
      signature=c(map="ANY",starttime='missing',endtime='missing'),
      definition=function # constructor for BoundInFlux
      ### The method internally calls \code{\link{TimeMap}} and expects
      ### the same kind of map argument
      (map, ##<< see the same argument in  \code{\link{TimeMap}} 
       lag=0, ##<< see the same argument in  \code{\link{TimeMap}} 
       interpolation ##<< see the same argument in  \code{\link{TimeMap}} 
       ){
        
        if (inherits(map,'TimeMap')){
         tm <-map
        }else{
          tm <- TimeMap(map,lag=lag,interpolation=interpolation)
        }
      return(as(tm,'BoundInFlux'))
     }
)
##------------------------constructors------------------------------------
#---------------------------------------------------------------------
setMethod(
      f="BoundInFlux",
      signature=c("ANY"),
      definition=function # convert to BoundInFlux
      ### The method is used internally to convert TimeMap objects to BoundInFlux objects, since the use of TimeMap objects is now deprecated.
      (map,  ##<< see the same argument in  \code{\link{TimeMap}} 
       starttime,##<< see the same argument in  \code{\link{TimeMap}} 
       endtime, ##<< see the same argument in  \code{\link{TimeMap}} 
       lag=0, ##<< see the same argument in  \code{\link{TimeMap}} 
       interpolation ##<< see the same argument in  \code{\link{TimeMap}} 
       ){
        
        obj <- as(TimeMap(map,starttime,endtime,lag=lag,interpolation=interpolation),"BoundInFlux")
        return(obj)
     }
)
