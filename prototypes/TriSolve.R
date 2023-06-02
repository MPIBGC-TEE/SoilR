TriSolve<-function(B, u){
  if(ncol(B) != nrow(B)) stop("B must be a square matrix")
  
  dg<-diag(B)
  ud<-B[row(B) == col(B)-1]
  ld<-B[row(B)-1 == col(B)]
  
  trisolve(a=dg, b=ud, d=ld, rhs=u)
}

B<-matrix(c(-2,1,0,1,-1,1, 0, 2, -3), 3,3)
TriSolve(B, u=c(1,0,0))
