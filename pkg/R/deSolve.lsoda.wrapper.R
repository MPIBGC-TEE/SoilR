#' deSolve.lsoda.wrapper
#' 
#' The function serves as a wrapper for lsoda using a much simpler interface
#' which allows the use of matrices in the definition of the derivative.  To
#' use lsoda we have to convert our vectors to lists, define tolerances and so
#' on. This function does this for us , so we don't need to bother about it.
#' 
#' 
#' @param t A row vector containing the points in time where the solution is
#' sought.
#' @param ydot The function of y and t that computes the derivative for a given
#' point in time and a column vector y.
#' @param startValues A column vector with the starting values.
#' @return A matrix. Every column represents a pool and every row a point in
#' time
deSolve.lsoda.wrapper=function(
	       t,	
	       ydot,    
	       startValues 
	       ){
   parms=NULL
   lsexamp <- function(t, y,parms)
     {
	yv=cbind(y)
	YD=ydot(y,t)
	yd=as.vector(YD)
       list(yd)
     }
   out <- lsoda(startValues,t,lsexamp)
   n=length(startValues)
   if (n==1) { Yt=matrix(ncol=n,out[,-1])}
   else {Yt=out[,-1]}
   tn=length(t) 
   Y=matrix(ncol=n,nrow=length(t))
   for (i in 1:n){
      Y[,i]=Yt[,i]
   }
   return(Y)
}
