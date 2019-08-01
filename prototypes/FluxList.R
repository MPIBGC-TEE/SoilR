
###' @param generatorFunc A 'classGeneratorFunction' 
##' as returned by 'setClass'
##' It contains information on which class it creates
##' @return A constructor for an new class that is created as a side effect
##' @details The function create a new class representing a 
##' a list of elements of the class created by generatorFunc.
##' So if generatorFunc creates an instance of class 'myClass' 
##' then it returns a constructor for the new class 'myClassList'
#listClassMaker<-function(generatorFunc){
#    elementClassName=generatorFunc@className
#    listClassName=paste(elementClassName,"List",sep='')
#
#}
##check_Flux_Frame<-function(object){TRUE}
##mycall=call(myClassNamesetClass( myClassName ,contains=c('list') ,validity=check_Flux_Frame)
##setMethod(
##    'initialize'
##    ,signature=signature(.Object=myClassName)
##    ,definition=function(.Object,...){
##        evaluated_args=list(...)
##         
##        print(evaluated_args)
##        .Object
##    }
##)
## example code
#setClass('PoolId',contains=c('VIRTUAL'))
setClass('PoolIndex',contains=c('integer'))
setClass('PoolName',contains=c('character'))
#setClass('OutBound',contains=c('VIRTUAL')
setClass('OutBoundByIndex'slots=c(source='PoolIndex')
setClass('OutBoundByName',slots=c(source='PoolName'))

OutFluxRateByIndex<-setClass(OutFluxRateByIndex,slots=c(stateVec='function'))
setMethod(
    'initialize'
    ,signature=signature(.Object='OutFluxRateByIndex')
    ,definition=function(.Object,source,){
        evaluated_args=list(...)
         
        print(evaluated_args)
        .Object
    }
)
OutFluxRateByName<-setClass(OutFluxRateByIndex,slots=c(stateVars='function'))
byIndex
#InFluxByIndex<-setClass(FluxClassName,slots=c(destination='character',stateVarFunc='function'))
#myFlux1=InFluxbyIndex(destination="barrel",stateVarFunc=function(x){x**1})
#myFlux2=InFluxbyIndex(destination="barrel",stateVarFunc=function(x){x**2})

#InFluxbyIndexList<-listClassMaker(InFluxbyIndex)
#myfluxList<-InFluxbyIndexList(myFlux1,myFlux2)
