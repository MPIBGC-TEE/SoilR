#' Effects of temperature on decomposition rates according to the StandCarb
#' model
#' 
#' Calculates the effects of temperature on decomposition rates according to
#' the StandCarb model.
#' 
#' 
#' @param Temp A scalar or vector containing values of temperature for which
#' the effects on decomposition rates are calculated.
#' @param Topt A scalar representing the optimum temperature for decomposition.
#' @param Tlag A scalar that determines the lag of the response curve.
#' @param Tshape A scalar that determines the shape of the response curve.
#' @param Q10 A scalar. Temperature coefficient Q10.
#' @return A scalar or a vector containing the effects of temperature on
#' decomposition rates (unitless).
#' @references Harmon, M. E., and J. B. Domingo (2001), A users guide to
#' STANDCARB version 2.0: A model to simulate carbon stores in forest stands.
#' Oregon State University, Corvallis.
fT.Standcarb<-structure(
  function 
    (Temp,      
     Topt=45,   
     Tlag=4,    
     Tshape=15, 
     Q10=2      
     )
   {
     exp(-1*(Temp/(Topt+Tlag))^Tshape)*Q10^((Temp-10)/10)
    }
    ,
    ex=function(){
      Temperature=0:50
      plot(Temperature,fT.Standcarb(Temperature),type="l",ylab="f(T) (unitless)", 
           main="Effects of temperature on decomposition rates according to the StandCarb model")
    }
)
