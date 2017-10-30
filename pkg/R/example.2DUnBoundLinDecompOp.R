
# vim:set ff=unix ts=2 sw=2:
example.2DUnBoundLinDecompOpFromFunction <- function (){
### An example used in tests and other examples.
	# The constructor BoundLinDecompOp has several Methods.
	# A single Matrix can describe a constant linear decomposition operator.
	# If the matrix varies with time it becomes a matrix valued function of time
	matFunc<- function(t){
		matrix(
			nrow=2,
			byrow=TRUE,
			c(
					-0.1*(sin(t)+1.1)		,	 0,
					 0									,	-0.2*(sin(t)+1.2)
			)
		)
	}
	# which is valid on a certain interval of time.
	res <- UnBoundLinDecompOp(matFunc)

	return(res)
}
