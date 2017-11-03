
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
  
  #possibleAs <- list(
  #  A.mat=matrix(nrow=2,byrow=TRUE,c(-0.1,0,0,-0.2)),
  #  A.matfunc=function(t){
  #   matrix(nrow=2,
  #     byrow=TRUE,
  #     c(-0.1,0,0,-0.2*(sin(t)+2)))},
  #  A.list.Times.Array=example.Time3DArrayList(),
  #  A.list.Times.List=example.nestedTime2DMatrixList(),
  #  # Model will convert these to the classes it uses internally but 
  #  # you can also create these object explicitly, which in some situations
  #  # gives you more control.
  #  A.tm=TimeMap(example.nestedTime2DMatrixList()),
  #  A.cdo=example.ConstlinDecompOpFromMatrix(),
  #  A.bldo=example.2DBoundLinDecompOpFromFunction(),
  #  A.ubldo=example.2DUnBoundLinDecompOpFromFunction()
  #) 
  possibleAs  <- example.2DGeneralDecompOpArgs()
  
  # Since "Model" will call "GeneralInFlux" on its "inputFluxes" 
  # argument there are again different choices
  # we have included a function in SoilR that produces 2D examples
  
  possibleInfluxes <- example.2DInFlux.Args()
 print(possibleInfluxes$I.vec)
  # We can build a lot of  models from the possible combinations
  # for instance   
  #m1 <- Model(
  #        t=times,
  #        A=matrix(nrow=2,byrow=TRUE,c(-0.1,0,0,-0.2)),
  #        ivList=iv,
  #        inputFluxes=possibleInfluxes$I.vec) 
  ## We now produce that all combinations of As and InputFluxes
  combinations <- unlist(recursive=FALSE,
                    lapply(possibleAs,function(A){
                      lapply(possibleInfluxes,function(I){
                        list(A=A,I=I)})}))
  print(length(combinations))
  # an a Model for each
  models <- lapply(
              combinations,
              function(combi){
                Model(t=times,A=combi$A,ivList=iv,inputFluxes=combi$I)
              }
            )
  ## lets check that we can compute something# 
  lapply(models,getC)
}
