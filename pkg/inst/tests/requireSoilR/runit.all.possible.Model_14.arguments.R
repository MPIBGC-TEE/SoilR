
# vim:set ff=unix expandtab ts=2 sw=2:
test.all.possible.Model_14.arguments <- function(){
  # This example shows different kinds of arguments to the function Model_14.
  # The model objects we will build will share some common features.
  #  - two pools 
  #  - initial values 
       iv<-  c(5,6)

  #  - times 
       times <- seq(1,10,by=0.1)
 
  #  - 14C decayrate
       c14DecayRate <- -0.0001209 #assumed unit is per y

  ##  - 14C ratio of initial carbon 
  #     initialValF  <- ConstFc(c(0,0))

  ##  - 14C ratio of InFlux 
  #     inputFc <- BoundFc(
  #       list(
  #         times=0:99,
  #         data=C14Atm_NH[1:100,2]
  #       ),
  #       format="Delta14C"
  #     )

  # The other parameters A, inputFluxes and inputFc will be different
  # The function Model_14 will transform these arguments 
  # into objects of the classes required by the internal constructor.
  # This leads to a number of possible argument types. 
  # Let us first look at the choices for argument 'A'.
  # We have included a function in SoilR that produces 2D examples of decomposition operators
  possibleAs  <- example.2DGeneralDecompOpArgs()
  
  # Since "Model_14" will call "GeneralInFlux" on its "inputFluxes" 
  # argument there are again different choices
  # We have included a function in SoilR that produces 2D examples
  
  possibleInfluxes <- example.2DInFlux.Args()

  # The initial values fIor the 14C/C fraction can also be given in different form
  # We have included a function in SoilR that produces 2D examples
  possibleInitialValFs <- example.2DConstFc.Args()

  possibleInputFs <- example.2DFc.Args()
  # We can build a lot of  models from the possible combinations
  # for instance   
  #m1 <- Model_14(
  #        t=times,
  #        A=matrix(nrow=2,byrow=TRUE,c(-0.1,0,0,-0.2)),
  #        ivList=iv,
  #        inputFluxes=possibleInfluxes$I.vec) 
  ## We now produce that all combinations of As and InputFluxes
  #addCombinations <- function(vectorOfLists,newIndexSet){
  #tp(list(1,2),list("a",'b'))
  #tp(c(1,2),list("a",'b'))
  
  combinations <- SoilR::listProduct(possibleAs,possibleInfluxes,possibleInitialValFs,possibleInputFs)
  print(length(combinations))
  print(combinations[1])
  # an a Model_14 for each
  models <- lapply(
              combinations,
              function(combi){
                #Model_14(t=times,A=combi$A,ivList=iv,inputFluxes=combi$I,initialValF=possibleInitialValFs[[3]],inputFc=inputFc)
                Model_14(t=times,A=combi[[1]],ivList=iv,inputFluxes=combi[[2]],initialValF=combi[[3]],inputFc=combi[[4]])
              }
            )
  ## lets check that we can compute something# 
  lapply(models,getC)
}
