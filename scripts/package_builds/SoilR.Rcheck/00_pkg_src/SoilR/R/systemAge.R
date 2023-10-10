#' System and pool age for constant compartment models
#' 
#' Computes the density distribution and mean for the system and pool ages of a
#' constant compartmental model in matrix representation 
#' 
#' @param A A constant compartmental square matrix with cycling rates in the
#' diagonal and transfer rates in the off-diagonal.
#' @param u A one-column matrix defining the amount of inputs per compartment.
#' @param a A sequence of ages to calculate density functions
#' @param q A vector of probabilities to calculate quantiles of the system age
#' distribution
#' @return A list with 5 objects: mean system age, system age distribution,
#' quantiles of system age distribution, mean pool-age, and pool-age
#' distribution.
#' @seealso \code{\link{transitTime}}
systemAge<-structure(
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
    xss= (-1*solve(A))%*%u
    X=diag(as.numeric(xss)) 
    eta=xss/sum(xss)
    meanSystemAge=(-1*t(one))%*%solve(A)%*%eta
    systemAgeDensity=NULL
    for(i in 1:length(a)){
      systemAgeDensity[i]=zT%*%expm(A*a[i])%*%eta
    }
    poolAgeDensity=NULL
    for(i in 1:length(a)){
      tmp=t((solve(X))%*%expm(A*a[i])%*%u)
      poolAgeDensity=rbind(poolAgeDensity,tmp)
    }
    meanPoolAge=-1*(solve(X))%*%solve(A)%*%xss
    FAt=function(t){
      1-(t(one)%*%expm(A*t)%*%eta)
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
    Q=sapply(q,quantile,CDF=FAt)
  return(list(meanSystemAge=meanSystemAge,systemAgeDensity=systemAgeDensity, quantilesSystemAge=Q,
              poolAgeDensity=poolAgeDensity, meanPoolAge=meanPoolAge))    
  }
  ,
  ex=function(){
  }
)
