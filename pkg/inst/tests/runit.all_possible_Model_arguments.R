
# vim:set ff=unix expandtab ts=2 sw=2:
test.all_possible_Model_arguments <- function(){
  # This example shows different kinds of arguments do the function Model.
  # The model objects we will build will  share some common features.
  #  - two pools 
  #  - initial values 
	     iv<-  c(5,6)
  #  - times 
	     times <- seq(1,10,by=0.1)
  # The other parameters A and inputFluxes will be different
	# The function Model will try to transform these arguments 
  # into objects of the classes required by the internal constructor.
	# This leads to a number of possible argument types. 
  # We demonstrate some of the possibilities here.
	# Let us first look at the possibilities of argument 'A'.
	# Since "Model" will call "GeneralDecompOp" on its argument "A" we 
  # look at the signatures of the methods of its methods.
	findMethods(GeneralDecompOp)
  # We find, for instance, a method for the argument type 'matrix'
	# We can therfore use an object of this class for argument 'A' in "Model". 
	M<- matrix(nrow=2,byrow=TRUE,c(-0.1,0,0,-0.2))
	A_mat <- M
  # A single Matrix can describe a constant decomposition operator.
  # We could also image time series data stored in an array consisting of
  # many stacked matrices, one for each time step.
  # Let us sythesize such a dataset:
  a <- array(dim=c(2,2,length(times)))
  a[1,1,] <- -0.1*(sin(times)+1.1)
  a[2,1,] <- 0
  a[1,2,] <- 0
  a[2,2,] <- -0.2*(sin(times)+1.2)
  dims <- dim(a)
  n <- dims[[1]]
  if (n!=dims[[2]]){
    stop(
      sprintf('The first two dimensions of the array must be equal.  Your array has dimensions: %s.',dims))
  }
  funcs <- list()
  for(i in 1:n){
    for(j in 1:n){
      index=sprintf('%s_%s',i,j)
      funcs[[index]] <- splinefun(times,a[i,j,])
    }
  }
  matFunc <- function(t){
    mat <- matrix(nrow=n,ncol=n)
    for(i in 1:n){
      for(j in 1:n){
        index=sprintf('%s_%s',i,j)
        mat[i,j] <- funcs[[index]](t)
      }
    }
  # another method for 'GeneralDecompOp' takes arguments of the class 'DecompOp' 
  # which is interesting when we look at the subclasses of 'DecompOp'
  names(getClass('DecompOp')@subclasses)  
  # The interesting ones are  'ConstLinDecompOp'  and 'BoundLinDecompOp'.
  # We can objects of either class for 'A' in 'Model'.	
  A_cl <- ConstLinDecompOp(M)
	# Since "Model" will call "GeneralInFlux" on its "inputFluxes" argument we can again look at the Signature of its Methods 

	findMethods(GeneralInFlux)
  # For instance we find one for class 'numeric' 
	I_vec<-  c(5,6)
	# We can build a model from it.
  mod <- Model(t=times,A=A_mat,ivList=iv,inputFluxes=I_vec)
  mod <- Model(t=times,A=A_cl,ivList=iv,inputFluxes=I_vec)
  # We can also peek inside the model object and see the internal representation.
  # 
	print(class(mod@mat))
	print(getC(mod))
	
}
test.all_possible_Model_arguments()
