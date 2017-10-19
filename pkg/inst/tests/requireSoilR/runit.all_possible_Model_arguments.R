
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
  # Let us first look at the choices for argument 'A'.
  possible_As <- c(
    A_matrix            =matrix(nrow=2,byrow=TRUE,c(-0.1,0,0,-0.2)),
    A_TimeMap           =SoilR:::example.TimeMapFromArray(),
    A_ConstlinDecompOp  =SoilR:::example.ConstlinDecompOpFromMatrix(),
    A_BoundLinDecompOp  =SoilR:::example.BoundLinDecompOpFromFunction()
  )
  lapply(possible_As,class)
  
  
  # Since "Model" will call "GeneralInFlux" on its "inputFluxes" argument there are again different choices
  
  possible_InFluxes <- c(
    I_vec          =c(5,6),
    I_BoundInFlux  =SoilR:::example.2DBoundInFluxFromFunction(),
    I_ConstInFlux  =SoilR:::example.2DConstInFluxFromVecto()
  )
    # We can build a lot of  models from the possible combinations
  models <- list()
  for (A in possible_As){
    for (IF in possible_InFluxes){
      models <- append(models,Model(t=times,A=A,ivList=iv,inputFluxes=IV)) 
    }
  }
  # lets check that we can compute something# 
  lapply(models,getC)
  
}
