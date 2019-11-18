#' helper for the construction of TimeMap objects 
#' 
scalarFuncMaker <-function(times,sclarLag,y_vector,interpolation){
    interpolation(x=times+scalar_lag,y=y_vector)
}

