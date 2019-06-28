spectralNorm=function(
m 
){
1/min(abs((eigen(m,only.values=TRUE))$values))
}
