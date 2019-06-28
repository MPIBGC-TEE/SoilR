#' Effects of temperature on decomposition rates according to a function
#' proposed by Lloyd and Taylor (1994)
#' 
#' Calculates the effects of temperature on decomposition rates according to a
#' function proposed by Lloyd and Taylor (1994).
#' 
#' 
#' @param Temp A scalar or vector containing values of soil temperature for
#' which the effects on decomposition rates are calculated
#' @return A scalar or a vector containing the effects of temperature on
#' decomposition rates (unitless).
#' @references Lloyd, J., and J. A. Taylor (1994), On the Temperature
#' Dependence of Soil Respiration, Functional Ecology, 8(3), 315-323.
fT.LandT<-structure(
  function 
    (Temp     
     )
   {
      exp(308.56*((1/56.02)-(1/((Temp+273)-227.13))))
    }
    ,
    ex=function(){
      Temperature=0:50
      plot(Temperature,fT.LandT(Temperature),type="l",
           ylab="f(T) (unitless)", 
           main="Effects of temperature on decomposition 
           rates according to the Lloyd and Taylor function")
    }
)
