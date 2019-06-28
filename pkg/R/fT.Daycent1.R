#' Effects of temperature on decomposition rates according to the DAYCENT model
#' 
#' Calculates the effects of temperature on decomposition rates according to
#' the DAYCENT model.
#' 
#' 
#' @param Temp A scalar or vector containing values of soil temperature for
#' which the effects on decomposition rates are calculated
#' @return A scalar or a vector containing the effects of temperature on
#' decomposition rates (unitless).
#' @references Kelly, R. H., W. J. Parton, M. D. Hartman, L. K. Stretch, D. S.
#' Ojima, and D. S. Schimel (2000), Intra-annual and interannual variability of
#' ecosystem processes in shortgrass steppe, J. Geophys. Res., 105.
fT.Daycent1<-structure(
  function 
    (Temp     
     )
   {
      0.08*exp(0.095*Temp)
    }
    ,
    ex=function(){
      Temperature=0:50
      plot(Temperature,fT.Daycent1(Temperature),type="l",ylab="f(T) (unitless)", 
           main="Effects of temperature on decomposition rates according to the DAYCENT model")
    }
)
