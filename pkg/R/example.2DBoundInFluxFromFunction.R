
# vim:set ff=unix ts=2 sw=2:
### Create a 2-dimensionsonal example of a BoundInFlux object
example.2DBoundInFluxFromFunction <- function
# 2D finite time linear influx examplex
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
	### The returned object represents a time dependent Influx into 
	### a two pool model.
}

