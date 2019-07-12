
# vim:set ff=unix ts=2 sw=2:
test.allpossible.InFluxes.arguments<- function(){
	possibleArgs <-example.2DInFlux.Args() 
	InFluxes <- lapply(possibleArgs, InFluxes)
	# We check which subclass is used internally for which argument type 
	print(lapply(InFluxes,class))
}
