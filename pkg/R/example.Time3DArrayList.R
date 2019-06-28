#' create an example TimeFrame from 3D array
#' 
#' An example used in tests and other examples.
#' 
#' 
example.Time3DArrayList<- function 
(){
	times <- seq(1,10,by=.1)
	a <- array(dim=c(2,2,length(times)))
	a[1,1,] <- -0.1 *(sin(times)+1.1)
	a[2,1,] <- -0.3 *(sin(times)+1.1)
	a[1,2,] <- -0.15*(sin(times)+1.1)
	a[2,2,] <- -0.2 *(sin(times)+1.2)
  return(list(times=times,data=a))
}
