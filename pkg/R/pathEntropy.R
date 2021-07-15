#' Path Entropy
#' 
#' Computes the entropy of particles passing through the whole network of
#' compartments for a model at equilibrium
#' 
#' @param A A constant compartmental square matrix with cycling rates in the
#' diagonal and transfer rates in the off-diagonal.
#' @param u A one-column matrix defining the amount of inputs per compartment.
#' @return A scalar value with the path entropy
#' @references 
#' Metzler, H. (2020). Compartmental systems as Markov chains : age, 
#' transit time, and entropy (T. Oertel-Jaeger, I. Pavlyukevich, and C. Sierra, 
#' Eds.) 
#' [PhD thesis](https://suche.thulb.uni-jena.de/Record/1726091651)
#' @examples
#' B6=matrix(c(-1,1,0,0,-1,1,0,0,-1),3,3); u6=matrix(c(1,0,0))
#' pathEntropy(A=B6, u=u6)
pathEntropy=function(A,u){
  if(dim(A)[1]==dim(A)[2]) d=dim(A)[1] else(stop("A must be a square matrix"))
  beta=u/sum(u)
  z=-colSums(A)
  x=-1*solve(A)%*%u
  A0=A; diag(A0)<-0
  H1=sum(beta*log(beta), na.rm = TRUE)
  H2_1=colSums(A0*(1-log(A0)), na.rm = TRUE)
  H2_2=z*(1-log(z))
  H2=sum((x/sum(u))*(rowSums(cbind(H2_1,H2_2),na.rm=TRUE)), na.rm = TRUE)
  H=-H1+H2
  return(H)
}

