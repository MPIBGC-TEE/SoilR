
# vim:set ff=unix ts=2 sw=2:
### Create a 2-dimensionsonal examples  of a Influx objects from different arguments
example.2DConstFc.Args <- function
# 2D finite time 14C/C  fraction examples
(){
	# The 14C/C fraction of the intial values 
	# can be represented  by the combination of 
	# a number and an optional format string 
	# which is either "Delta14C' or 'AbsoluteFractionModern'.
	# if non is given 'Delta14C will be assumend.

	Fc.num_Delta14C_d 										<- ConstFc(5)
	Fc.num_Delta14C 											<- ConstFc(5,'Delta14C')
	Fc.num_AbsoluteFractionModern 				<- ConstFc(5,'AbsoluteFractionModern')
	
	# or a positive vector with length equal 
	#to the number of pools and a format string

	Fc.vec_Delta14C_d 										<- ConstFc(c(5,6))
	Fc.vec_Delta14C 											<- ConstFc(c(5,6),'Delta14C')
	Fc.vec_AbsoluteFractionModern 				<- ConstFc(c(5,6),'AbsoluteFractionModern')

  # We return a list to be used in other examples and tests
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

