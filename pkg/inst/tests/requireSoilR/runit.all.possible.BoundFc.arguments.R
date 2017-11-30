
      p <- function# plot  
      (x,...){
        f <- getFunctionDefinition(x)   
        tr <- getTimeRange(x)
				times <- seq(min(tr),max(tr),length.out=400)
				print('#####################################################')
				print(quote(f(times[[1]])))
				print(f(times[[1]]))
				print('#####################################################')
				vals <- lapply(times,f)
        plot(x=times,y=vals)
      }
# vim:set ff=unix ts=2 sw=2:
test.allpossible.BoundFc.arguments<- function(){
	BoundFcs <- list(
    scalar_fraction = BoundFc(
    	list(
      	times=0:99,
      	data=C14Atm_NH[1:100,2]
     	),
      format="Delta14C"
    )
		,
    saclar_fraction_scalar_delay = BoundFc(
    	list(
      	times=0:99,
      	data=C14Atm_NH[1:100,2],
				lag=1.1
     	),
      format="Delta14C"
    )
		,
    saclar_fraction_vector_delay = BoundFc(
    	list(
      	times=0:99,
      	data=C14Atm_NH[1:100,2],
				lag=c(1.1,2.1)
     	),
      format="Delta14C"
    )
)
	# We check which subclass is used internally for which argument type 
  p(BoundFcs[['saclar_fraction_vector_delay']])
	#print(lapply(BoundFcs,plot))
}
