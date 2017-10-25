
# vim:set ff=unix ts=2 sw=2:
example.nestedTime3DArrayList <- function # create an example TimeFrame from 3D array
(){
# We could also imagine time series data stored in an array consisting of
	# many stacked matrices, one for each time step.
	# Let us sythesize such a dataset:

	times <- seq(1,10,by=0.1)
	a <- array(dim=c(2,2,length(times)))
	a[1,1,] <- -0.1*(sin(times)+1.1)
	a[2,1,] <-  0
	a[1,2,] <-  0
	a[2,2,] <- -0.2*(sin(times)+1.2)

  return(list(data=a,times=times))
}
