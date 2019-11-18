# helper for the construction of TimeMap objects 
# 
scalarFuncMaker <-function(times,scalar_lag,y_vector,interpolation){
    interpolation(x=times+scalar_lag,y=y_vector)
}

