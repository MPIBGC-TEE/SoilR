#
# vim:set ff=unix expandtab ts=2 sw=2:
cycling<-structure(
  function #Cycling analysis of compartmental matrices
  ### Computes the fundamental matrix N, and the expected number of steps from a compartmental matrix A
  (A  ##<< A compartmental linear square matrix with cycling rates in the diagonal and transfer rates in the off-diagonal.
  )
  {

    Id = diag(1,nrow=nrow(A),ncol=ncol(A)) #Identity matrix
    ones = matrix(1,nrow=nrow(A),ncol=1) # Vector of ones
      
    #Diagonal matrix D and jump chain matrix P
    D = diag(abs(diag(A)))
    P = A%*%solve(D) + Id
    
    # The fundamental matrix N and expected number of steps particles stay in pool j before leaving
    N = solve((Id-P))
    Et = t(ones)%*%N
    
    return(list(N=N,Et=Et)) 
    ### A list with 2 objects: the fundamental matrix N, and the expected number of steps Et.
    ##seealso<< \code{\link{systemAge}}
  }
  ,
  ex=function(){
    # Matrix with fluxes of phosphorus in an Eucalyptus-dominated ecosystem
    Fl=matrix(c(-2.1, 1.1, 1.0,
                2.1, -1.1-1.1, 0,
                0, 1.1, -1.0-0.2), byrow = TRUE, 3,3)
    x0=matrix(c(50,10,10*3),3,3,byrow = TRUE) # States: Vegetation, litter, mineral soil
    
    A=Fl/x0 # Compartmental matrix
    
    cycling(A)
  }
)
