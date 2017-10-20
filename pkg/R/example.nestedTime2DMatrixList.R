
# vim:set ff=unix ts=2 sw=2:
example.nestedTime2DMatrixList <- function # create an example nested list that can be 
(){
# We could imagine time series data stored in an array consisting of
	# many stacked matrices, one for each ti`me step.
  # we synthesize such data
	times <- seq(1,10,by=0.1)
	matFunc <- function(t){matrix(nrow=2,byrow=TRUE,c(-0.1,0,0,-0.2*(sin(t)+2)))}
	matList <- lapply(times,matFunc)
	nestedList <-list(times=times,data=matList) 
  return(nestedList)
}
