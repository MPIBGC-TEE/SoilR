#' A Linear Diffusion Model for representing depth profiles at equilibrium
#' 
#' This function implements a linear second order model with constant diffussion 
#' for representing vertical profiles.
#' 
#' @param x A vector of length m representing spatial (vertical) points for which the solution is requested
#' @param h A scalar representing the mesh width 
#' @param D A scalar represention the Diffusion coefficient 
#' @param psi A vector or a function of x representing the inputs at each discretized location
#' @param boundary A vector of length 2 with the boundary conditions at the extreme points of the mesh

linearDiffusionModel<-function(x, h, D, psi, boundary){
  
  m<-length(x)
  alpha<-boundary[1]
  beta<-boundary[2]
  
  A<-diag(-2,nrow=m)
  A[row(A)-1 == col(A)]<- 1
  A[row(A) == col(A)-1]<- 1
  A<-(h^(-2))*A
  
  f<- -psi/D
  F<-matrix(f,nrow=m, ncol=1)
  F[1]<-f[1]-(alpha/(h^2))
  F[m]<-f[m]-(beta/(h^2))

  U<-solve(A)%*%F
  Out<-list(U, A, F)
  names(Out)<-c("U", "A", "F")
  return(Out)
}

#' @example 
h<-0.1
depth<-seq(0,10, by=h)
D<- 0.01
boundary<-c(1,0.5)
rootIn<-h*exp(-0.5*depth)

eqSol<-linearDiffusionModel(x=depth, h=h, D=5, psi=rootIn, boundary=boundary)
str(eqSol)
plot(eqSol$U, -depth, type="l", bty="n")

Ds=seq(0.5,5, by=0.5)
xs<-sapply(Ds, FUN=function(y){ linearDiffusionModel(D=y, x=depth, h=h,psi=rootIn, boundary=boundary)$U})

matplot(depth, xs, type="l", lty=1, col=rainbow(10), bty="n")
legend("topright", legend=as.character(Ds), lty=1, col=rainbow(10), bty="n")

tau=seq(0,20, by=0.1)

library(SoilR)
TTs<-sapply(Ds, FUN=function(y){
  ldm<-linearDiffusionModel(D=y, x=depth, h=h,psi=rootIn, boundary=boundary)
  Ts<-transitTime(A=ldm$A, u=ldm$F, a=tau)$transitTimeDensity
  return(Ts)
  })

matplot(tau, log(TTs), type="l", lty=1, xlim=c(0,20), ylim=c(-10,5), col=rainbow(10), bty="n")
