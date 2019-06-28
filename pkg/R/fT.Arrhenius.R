#' Effects of temperature on decomposition rates according the Arrhenius
#' equation
#' 
#' Calculates the effects of temperature on decomposition rates according to
#' the Arrhenius equation.
#' 
#' 
#' @param Temp A scalar or vector containing values of temperature (in degrees
#' Kelvin) for which the effects on decomposition rates are calculated.
#' @param A A scalar defining the pre-exponential factor.
#' @param Ea A scalar defining the activation energy in units of J mol^-1.
#' @param Re A scalar defining the universal gas contant in units of J K^-1
#' mol^-1.
#' @return A scalar or a vector containing the effects of temperature on
#' decomposition rates (unitless).
fT.Arrhenius<-structure(
  function 
    (Temp,     
     A=1000,  
     Ea=75000,   
     Re=8.3144621   
     )
   {
      A*exp(-Ea/(Re*Temp))
    }
    ,
    ex=function(){
      Temperature=273:300
      plot(Temperature,fT.Arrhenius(Temperature),type="l",ylab="f(T) (unitless)", xlab="Temperature (K)",
           main="Effects of temperature on decomposition rates according to the Arrhenius equation")
    }
)
