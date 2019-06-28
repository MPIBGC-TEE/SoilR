#' Cycling analysis of compartmental matrices
#' 
#' Computes the fundamental matrix N, and the expected number of steps from a
#' compartmental matrix A
#' 
#' 
#' @param A A compartmental linear square matrix with cycling rates in the
#' diagonal and transfer rates in the off-diagonal.
#' @return A list with 2 objects: the fundamental matrix N, and the expected
#' number of steps Et.
#' @seealso \code{\link{systemAge}}
cycling<-structure(
  function 
  (A  
  )
  {
    Id = diag(1,nrow=nrow(A),ncol=ncol(A)) 
    ones = matrix(1,nrow=nrow(A),ncol=1) 
    D = diag(abs(diag(A)))
    P = A%*%solve(D) + Id
    N = solve((Id-P))
    Et = t(ones)%*%N
    return(list(N=N,Et=Et)) 
  }
  ,
  ex=function(){
    Fl=matrix(c(-2.1, 1.1, 1.0,
                2.1, -1.1-1.1, 0,
                0, 1.1, -1.0-0.2), byrow = TRUE, 3,3)
    x0=matrix(c(50,10,10*3),3,3,byrow = TRUE) 
    A=Fl/x0 
    cycling(A)
  }
)
