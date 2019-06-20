
# vim:set ff=unix ts=2 sw=2:
test.allpossible.InFlux.arguments<- function(){
	possibleArgs <-example.2DInFlux.Args() 
	InFluxes <- lapply(possibleArgs, InFlux)
	# We check which subclass is used internally for which argument type 
	print(lapply(InFluxes,class))
}
