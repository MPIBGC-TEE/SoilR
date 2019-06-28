#' create an example nested list that can be
#' 
#' An example used in tests and other examples.
#' 
#' 
example.nestedTime2DMatrixList <- function 
(){
	times <- seq(0,10,by=1)
	matFunc <- function(t){matrix(nrow=2,byrow=TRUE,c(-2*(sin(t*pi/2)+3),1,0,-3*(sin(t*pi/2)+3)))}
	matList <- lapply(times,matFunc)
	nestedList <-list(times=times,data=matList) 
  return(nestedList)
}
