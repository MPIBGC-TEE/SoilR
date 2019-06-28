#' euler
#' 
#' This function can solve arbitrary first order ode systems with the euler
#' forward method and an adaptive time-step size control given a tolerance for
#' the deviation of a coarse and fine estimate of the change in y for the next
#' time step. It is an alternative to \code{\link{deSolve.lsoda.wrapper}} and
#' has the same interface. It is much slower than ode and should probably be
#' considered less capable in solving stiff ode systems. However it has one
#' main advantage, which consists in its simplicity. It is quite easy to see
#' what is going on inside it. Whenever you don't trust your implementation of
#' another (more efficient but probably also more complex) ode solver, just
#' compare the result to what this method computes.
#' 
#' 
#' @param times A row vector containing the points in time where the solution
#' is sought.
#' @param ydot The function of y and t that computes the derivative for a given
#' point in time and a column vector y.
#' @param startValues A column vector with the initial values.
euler=function
(times,		
 ydot,		
 startValues		
 ){
   inc=1.5
   tol=10**(-10) 
   ttol=10**(-10) 
   minstep=10**(-5) 
   n=nrow(startValues)
   tn=length(times) 
   Y=matrix(nrow=n,ncol=tn)
   y=startValues
   Y[,1]=y
      for (j in 2:tn){
	 targettime=times[j]
	 t=times[j-1]
	 stepsize=(targettime-t)/10
	 y=Y[,j-1] 
	 while (t< targettime-ttol){
	    y0=y
	    dy=stepsize*ydot(y,t)
	    yp=y+dy
	    k=4
	    smallstep=stepsize/k
	    for (i in 1:k){
	       t=t+smallstep
	       dy=smallstep*ydot(y,t)
	       y=y+dy
	    }
	    ydiff=sum((yp-y)*(yp-y))
	    if (ydiff>tol){
	       t=t-stepsize
	       y=y0
	       stepsize=stepsize/2
	    }
	    else{
	       rest=targettime-t
	       planedstep=inc*stepsize
	       if (rest>planedstep)
		  if (rest-planedstep<minstep)
		     {stepsize=rest/2}
		  else
		     {stepsize=stepsize*inc}
	       else{stepsize=rest}
	    }
	 }
	 Y[,j]=y
      }
   Yt=t(Y)
   return(Yt)
}
