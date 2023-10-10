#' example.2DUnBoundLinDecompOpFromFunction
#' 
#' An example used in tests and other examples.
#' 
#' 
example.2DUnBoundLinDecompOpFromFunction <- function (){
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
	res <- UnBoundLinDecompOp(matFunc)
	return(res)
}
