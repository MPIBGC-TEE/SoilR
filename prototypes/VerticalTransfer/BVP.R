##
##  BVP  Boundary Value Problems. Modified form function pracma::bvp
##


BVP <- function(f, g, h, x, y, n = 50) {
  stopifnot(is.numeric(x), is.numeric(y), is.numeric(n))
  if (length(x) != 2 || length(y) != 2)
    stop("Arguments 'x' and 'y' must have length 2.")
  if (length(n) != 1 || floor(n) != ceiling(n) || n < 2)
    stop("Argument 'n' must be an integer greater or equal 2.")
  
  if (is.numeric(f)) ffun <- function(x) rep(f[1], length(x))
  else               ffun <- match.fun(f)
  if (is.numeric(g)) gfun <- function(x) rep(g[1], length(x))
  else               gfun <- match.fun(g)
  if (is.numeric(h)) hfun <- function(x) rep(h[1], length(x))
  else               hfun <- match.fun(h)
  
  xa <- x[1]; xb <- x[2]
  ya <- y[1]; yb <- y[2]
  xs <- linspace(xa, xb, n+2)[2:(n+1)]
  dt <- (xb - xa) / (n+1)
  
  a <- -2 - dt^2 * gfun(xs)           # main diagonal
  b <-  1 - dt/2 * ffun(xs[1:(n-1)])  # superdiagonal
  d <-  1 + dt/2 * ffun(xs[2:n])      # subdiagonal
  
  rhs <- dt^2 * hfun(xs)              # right hand side
  rhs[1] <- rhs[1] - ya * (1 + (dt/2) * ffun(xs[1]))
  rhs[n] <- rhs[n] - yb * (1 - (dt/2) * ffun(xs[n]))
  
  ys <- trisolve(a, b, d, rhs)
  
  A<-diag(a)
  A[row(A)-1 == col(A)]<- d
  A[row(A) == col(A)-1]<- b
  
  #return(list(xs = c(xa, xs, xb), ys = c(ya, ys, yb)))
  return(list(U = c(ya, ys, yb), A=A, F=rhs))
}


