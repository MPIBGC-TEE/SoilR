tupels<- function(dims){
		n <- length(dims)
		lastDimIndeces <-  1:dims[[n]]
		if(n==1){
				tupels <- lastDimIndeces
		}else{
				others <- tupels(dims[1:(n-1)])
				tupels <- list()
		    for (i in lastDimIndeces){
					tupels <-append(tupels,lapply(others,function(tup){
						newtup <- c(tup,i)
						return(newtup)
				 }))
				}
		}	
		return(tupels)
}
test <- tupels(c(2,3,4))
