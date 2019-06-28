#' Transit times for compartment models
#' 
#' Computes the density distribution and mean for the transit time of a
#' compartmental model
#' 
#' 
#' @param A A compartmental linear square matrix with cycling rates in the
#' diagonal and transfer rates in the off-diagonal.
#' @param u A one-column matrix defining the amount of inputs per compartment.
#' @param a A sequence of ages to calculate density functions
#' @param q Vector of probabilities to calculate quantiles of the transit time
#' distribution
#' @return A list with 3 objects: mean transit time, transit time density
#' distribution, and quantiles.
#' @seealso \code{\link{systemAge}}
transitTime<-structure(
  function 
  (A,  
   u,   
   a=seq(0,100), 
   q=c(0.05,0.5,0.95) 
  )
  {
    d=dim(A)[1]
    one=matrix(1,nrow=d,ncol=1)
    beta=u/sum(u)
    zT=-1*t(one)%*%A
    meanTransitTime=norm(solve(A)%*%beta)
    transitTimeDensity=NULL
    for(i in 1:length(a)){
      transitTimeDensity[i]=zT%*%expm(A*a[i])%*%beta
    }
    FTt=function(t){
      1-(t(one)%*%expm(A*t)%*%beta)
    }
    generalized_inverse_CDF <- function(CDF, x, start_dist = 1e-4){
      f <- function(y) x - CDF(y)
      x1 <- start_dist
      y1 <- f(x1)
      while (y1 >= 0){
        x1 <- x1 * 2
        y1 <- f(x1)
      }
      return(uniroot(f, lower = 0, upper = x1)$root)
    }
    quantile <- function(CDF, qs) generalized_inverse_CDF(CDF, qs)
    Q=sapply(q,quantile,CDF=FTt)
    return(list(meanTransitTime=meanTransitTime,transitTimeDensity=transitTimeDensity, quantiles=Q)) 
  }
  ,
  ex=function(){
  }
)
