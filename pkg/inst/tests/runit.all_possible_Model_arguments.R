test.all_possible_Model_arguments <- function(){#
# The function Model will try to tranform its arguments into objects of the classes required by the internal constructor.
# This leads to a great number of possible argument types. We demonstrate
# some of the possibilities here.
# Let us first look at the possibilities of argument \code{A}
# Since "Model" will call "GeneralDecompOp" on its argument "A" we look at the methods of "GeneralDecompOp"
findMethods(GeneralDecompOp)
# We can use their arguments also as argument "A" for "Model". For simplicity we will build only two pool models
A_mat <- matrix(nrow=2,byrow=TRUE,c(-0.1,0,0,-0.2,0))
}


#Since "Model" will call "GeneralInFlux" on its "inputFluxes" argument we can again look athe  
