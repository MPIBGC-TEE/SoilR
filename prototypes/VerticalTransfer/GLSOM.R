#' A General Linear Second Order Model for representing depth profiles at equilibrium
#' 
#' This function implements a general linear second order model for representing vertical transport 
#' at steady-state. It uses a second order differencing method to solve for the equilibrium state.
#' given some boundary conditions. 
#' @param x A vector with the spatial coordinates of the vertical profile
#' @param h The size of the intervals in x. A scalar value
#' @param D Diffusion coefficient. A scalar value
#' @param a Advection rate. A scalar value
#' @param c Reaction rate. A scalar value
#' @param f A vector with the vertical input rate
#' @param boundary A vector of length 2 with the upper and lower boundary conditions. These are Neumann boundary conditions with the input rate on the top layer as first element, and the bottom output rate as second element

GLSOM<-function(x, h, D, a, c, f, boundary){
  
  m<-length(x)
  alpha<-boundary[1]
  beta<-boundary[2]
  
  # Original formula from Leveque assuming Dirichlet boundary conditions at both ends
  # F<-matrix(f,nrow=m, ncol=1)
  # F[1]<- F[1]-((D/h^2) - (a/(2*h)))*alpha
  # F[m]<- F[m]-((D/h^2) + (a/(2*h)))*beta
  
  # Assuming Neumann boundary condition at the top and bottom
  F<-matrix(c(0,f),nrow=m+1, ncol=1)
  F[1]<- alpha
  F[m+2]<- -beta
  
  A<-diag(((h^2)*c - 2*D),nrow=m, ncol=m)
  A[row(A)-1 == col(A)]<- D + h*a/2
  A[row(A) == col(A)-1]<- D - h*a/2
  
  Ac1<-matrix(0, ncol=1, nrow=m)
  Ar1<-matrix(c(3*h/2, -2*h, h/2, rep(0, m-2)), nrow=1, ncol=m+1) # Derived from Leveque, page 31, third approach. Matrix 2.57 in following page is wrong, but this implementation should be correct.
  Arm<-matrix(0,nrow=1, ncol=m+1)
  Acm<-matrix(c(rep(0, m), h^2, h^2), ncol=1, nrow=m+2)
  
  A<-cbind(Ac1,A)
  A<-rbind(Ar1,A)
  A<-rbind(A,Arm)
  A<-cbind(A,Acm)

  A<-(h^(-2))*A
  
  U<- -1*solve(A)%*%F
  Out<-list(U[c(-1, -(m+2))], A[2:(m+1),2:(m+1)], F[c(-1, -(m+2))]) # Remove boundaries from solution and pack as list
  names(Out)<-c("U", "A", "F")
  return(Out)
}

# h<-0.01 # 1 cm steps
# depth<-seq(0,10, by=h) # 0 to 10 m
# D<- 1.48 # cm2 yr-1
# A<- -0.42*0.1 # mm yr-1 * 0.1 cm/mm * 1 cm -> cm2 yr-1
# fs<-0.17
# fd0=0.48; fd1=3.1 
# rootInputs<-fd0*exp(-fd1*depth)*h
# boundary<-c(fs,0)
# 
# sol<-GLSOM(x=depth,h,D,A,c=-5,f=rootInputs, boundary)
# 
# m=length(depth)
# plot(depth, sol$U,type="l", bty="n")
# 
# peclet<- -D/A

