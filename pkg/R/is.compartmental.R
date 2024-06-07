#' Test whether a square matrix is compartmental
#' 
#' @description
#' `is.compartmental` test three conditions on a square matrix. Condition 1: all diagonal elements are negative.
#' Condition 2: all off-diagonal elements are non-negative. Condition 3: Column sums are non-positive.
#' Notice that this is a test on strictly compartmental matrices, i.e. diagonal elements cannot be equal to zero.
#' 
#' @param B a square matrix
#' @return Logical. TRUE if B is a strictly compartmental matrix
#' @examples
#' B<-matrix(c(-3,2,1,1,-2,1,0,0,-1),3,3)
#' is.compartmental(B)
#' @export

is.compartmental<-function(B){
  if(ncol(B) != nrow(B)) stop("B must be a square matrix")
  
  condition1<- prod(diag(B)<0)
  
  bij<-B; diag(bij)<-0
  condition2<- prod(bij>=0)
  
  condition3<- prod(colSums(B)<=0)
  
  if(condition1*condition2*condition3==1)
    return(TRUE)
  else
    return(FALSE)
}
