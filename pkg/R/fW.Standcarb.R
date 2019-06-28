#' Effects of moisture on decomposition rates according to the StandCarb model
#' 
#' Calculates the effects of moisture on decomposition rates according to the
#' StandCarb model.
#' 
#' 
#' @param Moist A scalar or vector containing values of moisture content of a
#' litter or soil pool (\%).
#' @param MatricShape A scalar that determines when matric limit is reduced to
#' the point that decay can begin to occur.
#' @param MatricLag A scalar used to offset the curve to the left or right.
#' @param MoistMin A scalar determining the minimum moisture content.
#' @param MoistMax A scalar determining the maximum moisture content without
#' diffusion limitations.
#' @param DiffuseShape A scalar that determines the range of moisture contents
#' where diffusion is not limiting.
#' @param DiffuseLag A scalar used to shift the point when moisture begins to
#' limit diffusion.
#' @return A data frame with limitation due to water potential (MatricLimit),
#' limitation due to oxygen diffusion (DiffuseLimit), and the overall
#' limitation of moisture on decomposition rates (MoistDecayIndex).
#' @references Harmon, M. E., and J. B. Domingo (2001), A users guide to
#' STANDCARB version 2.0: A model to simulate carbon stores in forest stands.
#' Oregon State University, Corvallis.
fW.Standcarb<-structure(
  function 
    (Moist,      
     MatricShape=5,  
     MatricLag=0,    
     MoistMin=30,  
     MoistMax=350 ,      
     DiffuseShape=15,   
     DiffuseLag=4  
     )
   {
      IncreaseRate=3/MoistMin  
      MatricLimit=(1-exp(-IncreaseRate*(Moist+MatricLag)))^MatricShape
      DiffuseLimit=exp(-1*(Moist/(MoistMax+DiffuseLag))^DiffuseShape)
      MoistDecayIndex=MatricLimit*DiffuseLimit
      return(data.frame(MatricLimit,DiffuseLimit,MoistDecayIndex))
    }
    ,
    ex=function(){
      MC=0:500
      DeadFoliage=fW.Standcarb(MC)
      DeadBranch=fW.Standcarb(MC,MoistMax=200)
      DeadWood=fW.Standcarb(MC,MoistMax=150)
      StableSoil=fW.Standcarb(MC,MoistMin=15,MoistMax=100)
      plot(MC,DeadFoliage$MoistDecayIndex,type="l",xlab="Moisture Content (%)",
           ylab="f(W) (unitless)",
           main="Effects of moisture on decomposition rates according to the StandCarb model")
      lines(MC,DeadBranch$MoistDecayIndex,col=4)
      lines(MC,DeadWood$MoistDecayIndex,col=3)
      lines(MC,StableSoil$MoistDecayIndex,col=2)
      legend("topright",c("Dead Foliage","Dead Branch","Dead Wood","Stable Soil"),
             lty=c(1,1,1),col=c(1,4,3,2),bty="n")
    }
)
