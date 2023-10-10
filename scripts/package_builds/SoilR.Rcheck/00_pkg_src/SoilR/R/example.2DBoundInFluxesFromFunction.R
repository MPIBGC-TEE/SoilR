#' example.2DBoundInFluxesFromFunction
#' 
#' Create a 2-dimensional example of a BoundInFluxes object
#' 
#' 
#' @return The returned object represents a time dependent Influx into a two
#' pool model.
example.2DBoundInFluxesFromFunction <- function
(){
	BoundInFluxes(
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
