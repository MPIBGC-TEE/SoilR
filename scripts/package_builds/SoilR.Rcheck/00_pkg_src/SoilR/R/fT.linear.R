#' Effects of temperature on decomposition rates according to a linear model
#' 
#' Calculates the effects of temperature on decomposition rates according to a
#' linear model.
#' 
#' 
#' @param Temp A scalar or vector containing values of temperature for which
#' the effects on decomposition rates are calculated.
#' @param a A scalar defining the intercept of the linear function.
#' @param b A scalar defining the slope of the linear function.
#' @return A scalar or a vector containing the effects of temperature on
#' decomposition rates (unitless).
#' @references Adair, E. C., W. J. Parton, S. J. D. Grosso, W. L. Silver, M. E.
#' Harmon, S. A. Hall, I. C. Burke, and S. C. Hart. 2008.  Simple three-pool
#' model accurately describes patterns of long-term litter decomposition in
#' diverse climates. Global Change Biology 14:2636-2660.
fT.linear<-structure(
  function 
    (Temp,     
     a=0.198306,   
     b=0.036337   
     )
   {
      a+b*Temp
    }
    ,
    ex=function(){
      Temperature=0:50
      plot(Temperature,fT.linear(Temperature),type="l",ylab="f(T) (unitless)", 
           main="Effects of temperature on decomposition rates according to a linear function")
    }
)
