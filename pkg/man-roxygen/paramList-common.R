#' The params argument is an experimental feature that is not implemenet yet.
#' Due to the fact that the functions constituating a model do not have to be
#' evaluated until the solution of the model run (or any other numerical
#' computation)  is attempted, it is possible to inject them 'in the last
#' minute'.
#  E.g. a flux function might be given as
#' \code{ function(C){k_c*C} } . 
#' To numerically evaluate such a function we can transform it into 
#' \code{ function(C,params){with(params,k_c*C)}}  
#' and supply it with an argument \code{params=c(k_c=2)}. 
#' Many such functions can constitute more complex objects (like e.g. a model) and share the same parameter set.
#' The constructed object will pass on the parameter set to its building
#' blocks. 

