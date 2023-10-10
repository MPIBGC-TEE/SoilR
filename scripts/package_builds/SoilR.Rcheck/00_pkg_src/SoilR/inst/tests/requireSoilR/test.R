requireNamespace('devtools')
devtools::install('../../../')
requireNamespace('SoilR')
iv<-  c(5,6)
times <- seq(1,10,by=0.1)
possibleAs  <- SoilR::example.2DGeneralDecompOpArgs()

# Since "Model" will call "InFluxes" on its "inputFluxes" 
# argument there are again different choices
# we have included a function in SoilR that produces 2D examples

possibleInfluxes <- SoilR::example.2DInFluxes.Args()
print(possibleInfluxes$I.vec)
# We can build a lot of  models from the possible combinations
# for instance   
#m1 <- Model(
#        t=times,
#        A=matrix(nrow=2,byrow=TRUE,c(-0.1,0,0,-0.2)),
#        ivList=iv,
#        inputFluxes=possibleInfluxes$I.vec) 
# We now produce a list of all combinations of As and InputFluxes
combinations <- SoilR::listProduct(possibleAs,possibleInfluxes)
print(length(combinations))
# an a Model for each
models <- lapply(
            combinations,
            function(combi){
              #Model(t=times,A=combi$A,ivList=iv,inputFluxes=combi$I)
              SoilR::Model(t=times,A=combi[[1]],ivList=iv,inputFluxes=combi[[2]])
            }
          )
## lets check that we can compute something# 
lapply(models,SoilR::getC)
