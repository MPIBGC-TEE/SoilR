
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
setClass('PoolId',contains=c('VIRTUAL'))
setClass(
    'PoolIndex'
    ,contains=c(
        'PoolId'
        ,
        'integer'
    )
)
setClass(
    'PoolName'
    ,
    contains=c(
        'PoolId'
        ,
        'character'
    )
)
#setClass('OutBound',contains=c('VIRTUAL'))
setClass(
    'indexable'
    ,
    contains=c('VIRTUAL')
    ,
    slots=c(
        byIndex='logical'
    )
)
setClass(
    'OutBound'
    ,
    contains=c('indexable')
    ,
    slots=c(
        source='PoolId'
        ,
        Func='function'
    )
)
setClass(
    'StateIndependentOutBound'
    ,
    slots=c(
        source='PoolId'
    )
)
OutFluxRate                 <-setClass('OutFluxRate'                ,contains=c('OutBound'))
OutFlux                     <-setClass('OutFluxRateByIndex'         ,contains=c('OutBound'))
StateIndependentOutFlux     <-setClass('StateIndependentOutFlux'    ,contains=c('StateIndependentOutBound'))
StateIndependentOutFluxRate <-setClass('StateIndependentOutFluxRate',contains=c('StateIndependentOutBound'))
ConstantOutFlux             <-setClass('ConstantOutFlux'            ,contains=c('StateIndependentOutBound'))
ConstantOutFluxRate         <-setClass('ConstantOutFluxRate'        ,contains=c('StateIndependentOutBound'))

setClass(
    'InBound'
    ,
    contains=c('indexable')
    ,
    slots=c(
        destination='PoolId'
        ,
        Func='function'
    )
)
setClass(
    'StateIndependentInBound'
    ,
    contains=c('indexable')
    ,
    slots=c(
        byIndex='logical'
        ,
        destination='PoolId'
    )
)
InFluxRate                  <-setClass('InFluxRate'                 ,contains=c('InBound'))
InFlux                      <-setClass('InFluxRateByIndex'          ,contains=c('InBound'))
StateIndependentInFlux      <-setClass('StateIndependentInFlux'     ,contains=c('StateIndependentInBound'))
StateIndependentInFluxRate  <-setClass('StateIndependentInFluxRate' ,contains=c('StateIndependentInBound'))
ConstantInFlux              <-setClass('ConstantInFlux'             ,contains=c('StateIndependentInBound'))
ConstantInFluxRate          <-setClass('ConstantInFluxRate'         ,contains=c('StateIndependentInBound'))

setClass(
    'Internal'
    ,
    contains=c('indexable')
    ,
    slots=c(
        source='PoolId'
        ,
        destination='PoolId'
        ,
        Func='function'
    )
)
setClass(
    'StateIndependentInternal'
    ,
    contains=c('indexable')
    ,
    slots=c(
        source='PoolId'
        ,
        destination='PoolId'
    )
)
InternalFluxRate                 <-setClass('InternalFluxRate'                  ,contains=c('Internal'))
InternalFlux                     <-setClass('InternalFluxRateByIndex'           ,contains=c('Internal'))
StateIndependentInternalFlux     <-setClass('StateIndependentInternalFlux'      ,contains=c('StateIndependentInternal'))
StateIndependentInternalFluxRate <-setClass('StateIndependentInternalFluxRate'  ,contains=c('StateIndependentInternal'))
ConstantInternalFlux             <-setClass('ConstantInternalFlux'              ,contains=c('StateIndependentInternal'))
ConstantInternalFluxRate         <-setClass('ConstantInternalFluxRate'          ,contains=c('StateIndependentInternal'))


setGeneric(
    name='toIndexedForm'
    ,def=function(object,poolNames){ standardGeneric('toIndexedForm') }
)
setGeneric(
    name='isInIndexedForm'
    ,def=function(object){standardGeneric('isInIndexedForm')}
)

setMethod(
    'isInIndexedForm'
    ,signature('OutBound')
    ,definition=function(object){object@byIndex}
)
setMethod(
    'toIndexedForm'
    ,signature('InBound')
    ,definition=function(object,poolNames){
        if(object@byIndex){
            return(object)
        } else {
            object@destination  <- PoolIndex(  object@destination,poolNames)
            object@Func         <-   vecFunc(     object@Func    ,poolNames)
            return(object)
        }
    }
)
setMethod(
    'toIndexedForm'
    ,signature('StateIndependentOutBound')
    ,definition=function(object,poolNames){
        if(object@byIndex){
            return(object)
        } else {
            object@source<- PoolIndex(  object@source   ,poolNames)
            return(object)
        }
    }
)
setMethod(
    'isInIndexedForm'
    ,signature=signature(object='list')
    ,definition=function(object){
        all(as.logical(lapply(object,isInIndexedForm)))
    }
)
#InFluxByIndex<-setClass(FluxClassName,slots=c(destination='character',stateVarFunc='function'))
#myFlux1=InFluxbyIndex(destination="barrel",stateVarFunc=function(x){x**1})
#myFlux2=InFluxbyIndex(destination="barrel",stateVarFunc=function(x){x**2})

#InFluxbyIndexList<-listClassMaker(InFluxbyIndex)
#myfluxList<-InFluxbyIndexList(myFlux1,myFlux2)
