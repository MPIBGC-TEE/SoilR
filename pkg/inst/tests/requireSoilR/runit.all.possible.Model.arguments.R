
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
  combinations <- listProduct(possibleAs,possibleInfluxes)
  print(length(combinations))
  # an a Model for each
  models <- lapply(
              combinations,
              function(combi){
                #Model(t=times,A=combi$A,ivList=iv,inputFluxes=combi$I)
                Model(t=times,A=combi[[1]],ivList=iv,inputFluxes=combi[[2]])
              }
            )
  ## lets check that we can compute something# 
  lapply(models,getC)
}
