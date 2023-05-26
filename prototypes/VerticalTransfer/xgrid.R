library(pracma)
# % Specify grid points in space for solving a 2-point boundary value problem
# % or time-dependent PDE in one space dimension.
# %
# % Grid has m interior points on the interval [ax, bx].  
# % gridchoice specifies the type of grid (see below).
# % m+2 grid points (including boundaries) are returned in x.
# %
# % From  http://www.amath.washington.edu/~rjl/fdmbook/  (2007)

xgrid<-function(ax,bx,m,gridchoice){
  z = seq(0, 1, length.out=m+2) #';   % uniform grid in [0,1]

  if(gridchoice=='uniform') x = ax + (bx-ax)*z

  if(gridchoice=='rtlayer') x =  ax + (bx-ax) * (1 - (1-z)^2) # Clustered near right boundary
  if(gridchoice=='random') {
     x = ax + (bx-ax)*sort(runif(m+2))  
     x[1] = ax  
     x[m+2] = bx
     }

  if(gridchoice=='chebyshev') x = ax + (bx-ax) * 0.5*(1 + cos(pi*(1-z))) #Chebyshev extreme points
     
  if(gridchoice=='legendre'){ #zeros of Legendre polynomial plus endpoints
     Toff = 0.5/sqrt(1-(2*(1:(m-1)))^(-2))
      T = diag(Toff)
      T[row(T)-1==col(T)]<-Toff[-length(Toff)]
      xi = sort(eig(T))
      x = ax + (bx-ax) * 0.5*(1 + c(-1, xi, 1))
      }
 return(x)
}

xgrid(ax=0,bx=1,m=10, gridchoice = "uniform")
seq(0,1,length.out=12)
