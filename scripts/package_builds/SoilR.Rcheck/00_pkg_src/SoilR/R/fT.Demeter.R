#' Effects of temperature on decomposition rates according to the DEMETER model
#' 
#' Calculates the effects of temperature on decomposition rates according to
#' the DEMETER model.
#' 
#' 
#' @param Temp A scalar or vector containing values of temperature for which
#' the effects on decomposition rates are calculated
#' @param Q10 A scalar. Temperature coefficient Q10
#' @return A scalar or a vector containing the effects of temperature on
#' decomposition rates (unitless).
#' @references Foley, J. A. (1995), An equilibrium model of the terrestrial
#' carbon budget, Tellus B, 47(3), 310-319.
fT.Demeter<-structure(
  function 
    (Temp,     
     Q10=2     
     )
   {
     exp((log(Q10)/10)*(Temp-20))
    }
    ,
    ex=function(){
      Temperature=0:50
      plot(Temperature,fT.Demeter(Temperature),type="l",ylab="f(T) (unitless)", 
           main="Effects of temperature on decomposition rates according to the DEMETER model")
     }
)
