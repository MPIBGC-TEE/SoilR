example.BoundLinDecompOpFromFunction <- function (){
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
	return(BoundLinDecompOp(matFunc,0,10))
}
