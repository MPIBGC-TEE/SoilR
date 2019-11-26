#' The solution of the IVP for the pool contents.
#' Since the first models in SoilR had only Carbon pools 
#' the function name \code{getC} could be interpreted as 
#' refering to the C content. If the model includes other 
#' element cycles e.g. N or P this interpretation is no longer 
#' valid. In this case the C in 'getC' stands for 'content' 
#' since the function will always return the solution for 
#' all pools, regardless of the chemical element the author of 
#' the model associated them with.
