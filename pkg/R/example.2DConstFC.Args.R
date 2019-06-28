#' example.2DConstFc.Args
#' 
#' Create a 2-dimensionsonal examples of a Influx objects from different
#' arguments
#' 
#' 
example.2DConstFc.Args <- function
(){
	Fc.num_Delta14C_d 										<- ConstFc(5)
	Fc.num_Delta14C 											<- ConstFc(5,'Delta14C')
	Fc.num_AbsoluteFractionModern 				<- ConstFc(5,'AbsoluteFractionModern')
	Fc.vec_Delta14C_d 										<- ConstFc(c(5,6))
	Fc.vec_Delta14C 											<- ConstFc(c(5,6),'Delta14C')
	Fc.vec_AbsoluteFractionModern 				<- ConstFc(c(5,6),'AbsoluteFractionModern')
	 return(
		list(
			Fc.num_Delta14C_d 										= Fc.num_Delta14C ,
			Fc.num_Delta14C 											= Fc.num_Delta14C ,
			Fc.num_AbsoluteFractionModern 				= Fc.num_AbsoluteFractionModern 
			,
			Fc.vec_Delta14C_d 										= Fc.vec_Delta14C_d , 
			Fc.vec_Delta14C 											= Fc.vec_Delta14C , 
			Fc.vec_AbsoluteFractionModern 				= Fc.vec_AbsoluteFractionModern 
		)
	)
}
