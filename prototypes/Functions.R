
setClass('function_of_X_t',contains=c('function'))

#setMethod(
#    'initialize'
#    ,signature=signature(.Object='function_of_X_t')
#    ,definition=function(.Object,...){
#        .Object
#    }
#)
function_of_X_t=function(body){
    f=function(X,t){}
    body(f)<-body
    return(as(f,'function_of_X_t'))
}
function_of_X_t2=function(body){
    b<-substitute(body)
    f=function(X,t){}
    body(f)<-b
    return(as(f,'function_of_X_t'))
}
function_of_X_t3=function(f){
    return(as(f,'function_of_X_t'))
}
fvec1=function_of_X_t(quote({X**5*t}))
fvec2=function_of_X_t2({
    y=X**5
    return(y*2)
})
fvec3=function_of_X_t3(f=function(a,b){a**5*b})
#fvec3=new('function_of_X_t',func=function(a,b){a**5*b})
print(fvec1(2,3))
print(fvec2(2,3))
print(fvec3(2,3))
