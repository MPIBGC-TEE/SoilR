
# vim:set ff=unix expandtab ts=2 sw=2:
test.all.possible.Model.arguments <- function(){
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

  A.matrix              <- matrix(nrow=2,byrow=TRUE,c(-0.1,0,0,-0.2))
  A.matrix.func         <- function(t){
                            matrix(nrow=2,
                              byrow=TRUE,
                              c(-0.1,0,0,-0.2*(sin(t)+2)))}
  A.list.array          <- example.Time3DArrayList()
  A.list.mat.list       <- example.nestedTime2DMatrixList()
  # Model will convert these to the classes it uses internally but 
  # you can also create these object explicitly, which in some situations
  # gives you more control.
  A.TimeMap             <- TimeMap(example.nestedTime2DMatrixList())
  A.ConstlinDecompOp    <- example.ConstlinDecompOpFromMatrix()
  A.BoundLinDecompOp    <- example.2DBoundLinDecompOpFromFunction()
  A.UnBoundLinDecompOp  <- example.2DUnBoundLinDecompOpFromFunction()
  
  
  # Since "Model" will call "GeneralInFlux" on its "inputFluxes" 
  # argument there are again different choices
  
  I.vec          =c(5,6) 
  I.BoundInFlux  =SoilR:::example.2DBoundInFluxFromFunction()
  I.ConstInFlux  =SoilR:::example.2DConstInFluxFromVector()
    # We can build a lot of  models from the possible combinations
     
     Model(t=times,A=A.matrix.func,ivList=iv,inputFluxes=I.vec) 

  models <- list(
    Model(t=times,A=A.matrix,ivList=iv,inputFluxes=I.vec)
    ,
    Model(t=times,A=A.matrix,ivList=iv,inputFluxes=I.BoundInFlux)
    ,
    Model(t=times,A=A.matrix,ivList=iv,inputFluxes=I.ConstInFlux)
    ,

    Model(t=times,A=A.matrix.func,ivList=iv,inputFluxes=I.vec)
    ,
    Model(t=times,A=A.matrix.func,ivList=iv,inputFluxes=I.BoundInFlux)
    ,
    Model(t=times,A=A.matrix.func,ivList=iv,inputFluxes=I.ConstInFlux)
    ,

    Model(t=times,A=A.list.mat.list,ivList=iv,inputFluxes=I.vec)
    ,
    Model(t=times,A=A.list.mat,ivList=iv,inputFluxes=I.BoundInFlux)
    ,
    Model(t=times,A=A.list.mat,ivList=iv,inputFluxes=I.ConstInFlux)
    ,

    Model(t=times,A=A.list.array,ivList=iv,inputFluxes=I.vec)
    ,
    Model(t=times,A=A.list.array,ivList=iv,inputFluxes=I.BoundInFlux)
    ,
    Model(t=times,A=A.list.array,ivList=iv,inputFluxes=I.ConstInFlux)
    ,

    Model(t=times,A=A.TimeMap,ivList=iv,inputFluxes=I.vec)
    ,
    Model(t=times,A=A.TimeMap,ivList=iv,inputFluxes=I.BoundInFlux)
    ,
    Model(t=times,A=A.TimeMap,ivList=iv,inputFluxes=I.ConstInFlux)
    ,

    Model(t=times,A=A.ConstlinDecompOp,ivList=iv,inputFluxes=I.vec)
    ,
    Model(t=times,A=A.ConstlinDecompOp,ivList=iv,inputFluxes=I.BoundInFlux)
    ,
    Model(t=times,A=A.ConstlinDecompOp,ivList=iv,inputFluxes=I.ConstInFlux)
    ,

    Model(t=times,A=A.ConstlinDecompOp,ivList=iv,inputFluxes=I.vec)
    ,
    Model(t=times,A=A.ConstlinDecompOp,ivList=iv,inputFluxes=I.BoundInFlux)
    ,
    Model(t=times,A=A.ConstlinDecompOp,ivList=iv,inputFluxes=I.ConstInFlux)
    )
  # lets check that we can compute something# 
  lapply(models,getC)
  
}
#test.all.possible.Model.arguments()
