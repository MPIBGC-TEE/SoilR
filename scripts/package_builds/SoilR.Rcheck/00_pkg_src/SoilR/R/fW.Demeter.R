#' Effects of moisture on decomposition rates according to the DEMETER model
#' 
#' Calculates the effects of soil moisture on decomposition rates according to
#' the DEMETER model.
#' 
#' 
#' @param M A scalar or vector containing values of soil moisture for which the
#' effects on decomposition rates are calculated.
#' @param Msat A scalar representing saturated soil moisture.
#' @return A scalar or a vector containing the effects of moisture on
#' decomposition rates (unitless).
#' @references Foley, J. A. (1995), An equilibrium model of the terrestrial
#' carbon budget, Tellus B, 47(3), 310-319.
fW.Demeter<-structure(
  function 
    (M,         
     Msat=100   
     )
   {
     0.25+0.75*(M/Msat)
    }
    ,
    ex=function(){
      Moisture=0:100
      plot(Moisture,fW.Demeter(Moisture),type="l",ylab="f(W) (unitless)", 
           main="Effects of soil moisture on decomposition rates according to the DEMETER model")
     }
)
