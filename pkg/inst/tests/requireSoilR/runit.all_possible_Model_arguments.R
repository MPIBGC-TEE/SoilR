
# vim:set ff=unix ts=2 sw=2:
test.all_possible_Model_arguments <- function(){
  # This example shows different kinds of arguments to the function Model.
  # The model objects we will build will share some common features.
  #  - two pools 
  #  - initial values 

       iv<-  c(5,6)

  #  - times 

       times <- seq(1,10,by=0.1)

  # The other parameters A and inputFluxes will be different
  # The function Model will transform these arguments 
  # into objects of the classes required by the internal constructor.
  # This leads to a number of possible argument types. 
  # We demonstrate some of the possibilities here.
  # Let us first look at the choeices for argument 'A'.

  A_matrix             <- matrix(nrow=2,byrow=TRUE,c(-0.1,0,0,-0.2))
  A_matrix_func				 <- function(t){matrix(nrow=2,byrow=TRUE,c(-0.1,0,0,-0.2*(sin(t)+2)))}
  A_TimeMap            <- example.TimeMapFromArray()
  A_ConstlinDecompOp   <- example.ConstlinDecompOpFromMatrix()
  A_BoundLinDecompOp   <- example.2DBoundLinDecompOpFromFunction()
  A_UnBoundLinDecompOp   <- example.2DUnBoundLinDecompOpFromFunction()
  
  
  # Since "Model" will call "GeneralInFlux" on its "inputFluxes" argument there are again different choices
  
  I_vec          =c(5,6) 
  I_BoundInFlux  =SoilR:::example.2DBoundInFluxFromFunction()
  I_ConstInFlux  =SoilR:::example.2DConstInFluxFromVector()
    # We can build a lot of  models from the possible combinations
	   
     Model(t=times,A=A_matrix_func,ivList=iv,inputFluxes=I_vec) 

	models <- list(
      Model(t=times,A=A_matrix,ivList=iv,inputFluxes=I_vec),
      Model(t=times,A=A_matrix,ivList=iv,inputFluxes=I_BoundInFlux),
      Model(t=times,A=A_matrix,ivList=iv,inputFluxes=I_ConstInFlux),

      Model(t=times,A=A_matrix_func,ivList=iv,inputFluxes=I_vec),
      Model(t=times,A=A_matrix_func,ivList=iv,inputFluxes=I_BoundInFlux),
      Model(t=times,A=A_matrix_func,ivList=iv,inputFluxes=I_ConstInFlux),

      Model(t=times,A=A_TimeMap,ivList=iv,inputFluxes=I_vec),
      Model(t=times,A=A_TimeMap,ivList=iv,inputFluxes=I_BoundInFlux),
      Model(t=times,A=A_TimeMap,ivList=iv,inputFluxes=I_ConstInFlux),

      Model(t=times,A=A_ConstlinDecompOp,ivList=iv,inputFluxes=I_vec),
      Model(t=times,A=A_ConstlinDecompOp,ivList=iv,inputFluxes=I_BoundInFlux),
      Model(t=times,A=A_ConstlinDecompOp,ivList=iv,inputFluxes=I_ConstInFlux),

      Model(t=times,A=A_ConstlinDecompOp,ivList=iv,inputFluxes=I_vec),
      Model(t=times,A=A_ConstlinDecompOp,ivList=iv,inputFluxes=I_BoundInFlux),
      Model(t=times,A=A_ConstlinDecompOp,ivList=iv,inputFluxes=I_ConstInFlux)
	)
  # lets check that we can compute something# 
  lapply(models,getC)
  
}
#test.all_possible_Model_arguments()
