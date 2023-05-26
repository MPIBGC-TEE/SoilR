# % Compute coefficients for finite difference approximation for the
# % derivative of order k at xbar based on grid values at points in x.
# %
# % This function returns a row vector c of dimension 1 by n, where n=length(x),
# % containing coefficients to approximate u^{(k)}(xbar), 
# % the k'th derivative of u evaluated at xbar,  based on n values
# % of u at x(1), x(2), ... x(n).  
# %
# % If U is a column vector containing u(x) at these n points, then 
# % c*U will give the approximation to u^{(k)}(xbar).
# %
# % Note for k=0 this can be used to evaluate the interpolating polynomial 
# % itself.
# %
# % Requires length(x) > k.  
# % Usually the elements x(i) are monotonically increasing
# % and x(1) <= xbar <= x(n), but neither condition is required.
# % The x values need not be equally spaced but must be distinct.  
# %
# % This program should give the same results as fdcoeffV.m, but for large
# % values of n is much more stable numerically.
# %
# % Based on the program "weights" in 
# %   B. Fornberg, "Calculation of weights in finite difference formulas",
# %   SIAM Review 40 (1998), pp. 685-691.
# %
# % Note: Forberg's algorithm can be used to simultaneously compute the
# % coefficients for derivatives of order 0, 1, ..., m where m <= n-1.
# % This gives a coefficient matrix C(1:n,1:m) whose k'th column gives
# % the coefficients for the k'th derivative.
# %
# % In this version we set m=k and only compute the coefficients for
# % derivatives of order up to order k, and then return only the k'th column
# % of the resulting C matrix (converted to a row vector).  
# % This routine is then compatible with fdcoeffV.   
# % It can be easily modified to return the whole array if desired.
# %
# % From  http://www.amath.washington.edu/~rjl/fdmbook/  (2007)
#


fdcoeffF<-function(k,xbar,x){

n = length(x)
if(k >= n) stop('*** length(x) must be larger than k')

m = k   # change to m=n-1 if you want to compute coefficients for all
        # possible derivatives.  Then modify to output all of C.
c1 = 1
c4 = x[1] - xbar
C = matrix(0,nrow=n-1, ncol=m+1) #zeros(n-1,m+1);
C[1,1] = 1
for(i in 1:(n-1)){
  i1 = i+1
  mn = min(i,m)
  c2 = 1
  c5 = c4
  c4 = x[i1] - xbar
  for(j in 0:(i-1)){
    j1 = j+1
    c3 = x[i1] - x[j1]
    c2 = c2*c3
    if(j==(i-1)){
      for(s in mn:1){
        s1 = s+1
        C[i1,s1] = c1*(s*C[(i1-1),(s1-1)] - c5*C[(i1-1),s1])/c2
        }
      C[i1,1] = -c1*c5*C[(i1-1),1]/c2
    }
    for(s in mn:1){
      s1 = s+1
      C[j1,s1] = (c4*C[j1,s1] - s*C[j1,(s1-1)])/c3
      }
    C[j1,1] = c4*C[j1,1]/c3
  }
  c1 = c2
  }
c = C[,ncol(C)]            #% last column of c gives desired row vector
return(c)
}

x<-xgrid(1,3,10,"uniform")
fdcoeffF(k=1, xbar=x[1], x[1:3])

fornberg(x=x[1:3],y=rep(1,3),xs=x[1],k=1)
