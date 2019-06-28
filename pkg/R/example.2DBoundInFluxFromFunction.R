#' example.2DBoundInFluxFromFunction
#' 
#' Create a 2-dimensionsonal example of a BoundInFlux object
#' 
#' 
#' @return The returned object represents a time dependent Influx into a two
#' pool model.
example.2DBoundInFluxFromFunction <- function
(){
	BoundInFlux(
		function(t){
			c(
				1+sin(t),
				2+sin(2*t)
			)
		},
		0,
		10
	)
}
