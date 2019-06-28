#' Effects of moisture on decomposition rates according to the function
#' proposed by Moyano et al. (2013)
#' 
#' Calculates the effects of water content on decomposition rates.
#' 
#' 
#' @param theta A scalar or vector containing values of volumetric soil water
#' content.
#' @param a Empirical parameter
#' @param b Empirical parameter
#' @references F. E. Moyano, S. Manzoni, C. Chenu. 2013 Responses of soil
#' heterotrophic respiration to moisture availability: An exploration of
#' processes and models.  Soil Biology and Biochemistry, Volume 59, April 2013,
#' Pages 72-85
fW.Moyano<- structure(
  function 
  (theta,     
   a=3.11, 
   b=2.42 
  )
  {
    a*theta-b*theta^2
  }
  ,
  ex=function(){
    th=seq(0,1,0.01)
    xi=fW.Moyano(theta=th)
    plot(th,xi,type="l",main="Effects of soil water content on decomposition rates",
         xlab="Volumetric soil water content (cm3 cm-3)",ylab=expression(xi))
  }
)
