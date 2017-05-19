#
# vim:set ff=unix expandtab ts=2 sw=2:
systemAge<-structure(
  function #System and pool age for compartment models
  ### Computes the density distribution and mean for the system and pool ages of a SoilR model or a matrix representation of a compartmental model
  (A,  ##<< A compartmental linear square matrix with cycling rates in the diagonal and transfer rates in the off-diagonal.
   u,   ##<< A one-column matrix defining the amount of inputs per compartment.
   a=seq(0,100), ##<< A sequence of ages to calculate density functions   
   q=c(0.05,0.5,0.95) ##<< A vector of probabilities to calculate quantiles of the system age distribution
  )
  {
    
    #Mean system age
    d=dim(A)[1]
    one=matrix(1,nrow=d,ncol=1)
    beta=u/sum(u)
    zT=-1*t(one)%*%A
    xss= (-1*solve(A))%*%u
    X=diag(as.numeric(xss)) # Diagonal matrix with steady-state solutions
    eta=xss/sum(xss)
    meanSystemAge=(-1*t(one))%*%solve(A)%*%eta
    
    # System age density
    systemAgeDensity=NULL
    for(i in 1:length(a)){
      systemAgeDensity[i]=zT%*%expm(A*a[i])%*%eta
    }
    
    # Pool age density
    poolAgeDensity=NULL
    for(i in 1:length(a)){
      tmp=t((solve(X))%*%expm(A*a[i])%*%u)
      poolAgeDensity=rbind(poolAgeDensity,tmp)
    }
    
    # Mean pool-age
    meanPoolAge=-1*(solve(X))%*%solve(A)%*%xss

    # Cummulative probability function
    FAt=function(t){
      1-(t(one)%*%expm(A*t)%*%eta)
    }
    
    # compute inverse of CDF at x for quantiles or generation of random variables
    generalized_inverse_CDF <- function(CDF, x, start_dist = 1e-4){
      f <- function(y) x - CDF(y)
      x1 <- start_dist
      
      # go so far to the right such that CDF(x1) > x, the bisect in interval [0, x1]
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
        ### A list with 5 objects: mean system age, system age distribution, quantiles of system age distribution, mean pool-age, and pool-age distribution.
        ##seealso<< \code{\link{transitTime}}
  }
  ,
  ex=function(){
# Uncomment to run:
#    # Gaudinski model
#    ks = c(kr = 1/1.5, koi = 1/1.5, koeal = 1/4, koeah = 1/80, 
#           kA1 = 1/3, kA2 = 1/75, kM = 1/110)
#    A = -abs(diag(ks))
#    A[3, 2] = ks[2] * (98/(3 + 98 + 51))
#    A[4, 3] = ks[3] * (4/(94 + 4))
#    A[6, 5] = ks[5] * (24/(6 + 24))
#    A[7, 6] = ks[6] * (3/(22 + 3))
#    A[7, 2] = ks[2] * (3/(3 + 98 + 51))
#    A[4, 1] = ks[1] * (35/(35 + 190 + 30))
#    A[5, 1] = ks[1] * (30/(35 + 190 + 30))
#    
#    LI = 150 #Litter inputs
#    RI = 255 #Root inputs
#    In=matrix(nrow = 7, ncol = 1, c(RI, LI, 0, 0, 0, 0, 0))
#    
#    ages=seq(0,200)
#      
   # ga=systemAge(A=A, u=In, a=ages)
   # 
   # plot(ages, ga$systemAgeDensity, type="l")
   # abline(v=ga$meanSystemAge, lty=2)
   # abline(v=ga$quantilesSystemAge[2],lty=2,col=2)
   # legend("topright",c("System Age density",
   #         paste("Mean system age = ",round(ga$meanSystemAge, 2)),
   #         paste("Median system age = ",round(ga$quantilesSystemAge[2],2))),
   #         lty=c(1,2,2), col=c(1,1,2), bty="n")
   # 
   # pools=c("Roots", "Oi", "Oe/a L", "Oe/a H", "A, LF (>80 um)",
   #          "A, LF (< 80 um)", "Min. Ass.")
   # par(mfrow=c(3,2))
   # for(i in 2:7){
   #   plot(ages,ga$poolAgeDensity[,i],type="l", main=pools[i],
   #        ylab="Probability density", xlab="Age", bty="n")
   #  }
   #  par(mfrow=c(1,1))
    
    
  }
)
