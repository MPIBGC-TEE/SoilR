


#' automatic title
#' 
#' @param F see method arguments
#' @s4methods
#' @autocomment
setGeneric(
  name="Delta14C",
  def=function( 
  F 
  ){
	   standardGeneric("Delta14C")
  }
)



#' automatic title
#' 
#' @param AbsoluteFractionModern see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="Delta14C_from_AbsoluteFractionModern",
	 def=function( 
	 AbsoluteFractionModern 
	 ){
	     standardGeneric("Delta14C_from_AbsoluteFractionModern")
	 }
)

#' conversion
#' @param delta14C Object to be converted to AbsoluteFractionModern
setGeneric(
	 name="AbsoluteFractionModern_from_Delta14C",
	 def=function(delta14C){
	     standardGeneric("AbsoluteFractionModern_from_Delta14C")
	 }
)



#' automatic title
#' 
#' @param F see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="AbsoluteFractionModern",
	 def=function( 
	 F 
	 ){
	     standardGeneric("AbsoluteFractionModern")
	 }
)



#' automatic title
#' 
#' @param object see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="getFormat",
	 def=function( 
	 object 
	 ){
	     standardGeneric("getFormat")
	 }
)



#' automatic title
#' 
#' @param object see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="getValues",
	 def=function( 
	 object 
	 ){
	     standardGeneric("getValues")
	 }
)



#' automatic title
#' 
#' @param delta14C no manual documentation
#' @autocomment
setMethod(
	f= "AbsoluteFractionModern_from_Delta14C",
	   signature("numeric"),
	   definition=function(
	delta14C 
	){
	fprime=(delta14C/1000)+1
	return(fprime)
	}
)



#' automatic title
#' 
#' @param AbsoluteFractionModern no manual documentation
#' @autocomment
setMethod(
	f= "Delta14C_from_AbsoluteFractionModern",
	   signature("numeric"),
	   definition=function(
	   AbsoluteFractionModern 
	   ){
	     D14C=(AbsoluteFractionModern-1)*1000
	     return(D14C)
	   }
)



#' automatic title
#' 
#' @param delta14C no manual documentation
#' @autocomment
setMethod(
	f= "AbsoluteFractionModern_from_Delta14C",
	   signature("matrix"),
	   definition=function 
	(
	delta14C 
	){
	fprime=matrix(
	    nrow=nrow(delta14C),
	    ncol=ncol(delta14C),
	    sapply(delta14C,AbsoluteFractionModern_from_Delta14C)
	)
	return(fprime)
	}
)



#' automatic title
#' 
#' @param AbsoluteFractionModern no manual documentation
#' @autocomment
setMethod(
	f= "Delta14C_from_AbsoluteFractionModern",
	   signature("matrix"),
	   definition=function(
	AbsoluteFractionModern 
	){
	D14C=matrix(
	    nrow=nrow(AbsoluteFractionModern),
	    ncol=ncol(AbsoluteFractionModern),
	    sapply(AbsoluteFractionModern,Delta14C_from_AbsoluteFractionModern)
	)
	return(D14C)
	}
)



#' automatic title
#' 
#' @param object see method arguments
#' @param inputDistribution see method arguments
#' @s4methods
#' @autocomment
setGeneric ( 
	name= "getMeanTransitTime",
	def=function(
	   object,           
	   inputDistribution 
	){standardGeneric("getMeanTransitTime")}
)



#' automatic title
#' 
#' @param object see method arguments
#' @param inputDistribution see method arguments
#' @param times see method arguments
#' @s4methods
#' @autocomment
setGeneric ( 
	name= "getTransitTimeDistributionDensity",
	def=function(
	             object, 
	             inputDistribution, 
	             times 
	){standardGeneric("getTransitTimeDistributionDensity")}
)



#' automatic title
#' 
#' @param object see method arguments
#' @s4methods
#' @autocomment
setGeneric (
	name= "getTimes",
	def=function(
	object){standardGeneric("getTimes")}
)



#' automatic title
#' 
#' @param object see method arguments
#' @s4methods
#' @autocomment
setGeneric (
	name= "getInitialValues",
	def=function(
	object){standardGeneric("getInitialValues")}
)

#' Generic Function to obtain the fluxes out of of the pools 
#'
setGeneric ( 
	name= "getOutputFluxes",
	def=function
	(
	object 
  ,as.closures=F 
	){standardGeneric("getOutputFluxes")
	}
)

#' Calculates the content of the pools 
#'
#' This function computes the content of the pools as function of time.
#' In the original (and most of the present) Models these are Carbon pools hence the name.
#' Have a look at the methods for details.
#' @template Model-param
#' @return A matrix with m columns representing where m is the number of pools, and n rows where n is the number times 
# as specified by the \code{times} of the model. 



setGeneric ( 
	name= "getC",
	def=function(
	object 
  ,as.closures=F 
	){standardGeneric("getC")
	}
)



#' automatic title
#' 
#' @param object see method arguments
#' @s4methods
#' @autocomment
setGeneric( 
	name= "getParticleMonteCarloSimulator",
	def=function
	(object 
	){standardGeneric("getParticleMonteCarloSimulator")
	 }
)

#' Generic Function to obtain the vector of release fluxes out of the pools for all times.
#'
setGeneric ( 
	name= "getReleaseFlux",
  valueClass='matrix',
	def=function
	(
	object 
	){standardGeneric("getReleaseFlux")
	}
)



#' automatic title
#' 
#' @param object see method arguments
#' @s4methods
#' @autocomment
setGeneric ( 
	name= "getAccumulatedRelease",
	def=function
	(object 
	){standardGeneric("getAccumulatedRelease")
	 }
)

#' Generic that yields the ^{14}C content for all pools and all times
#'
setGeneric ( 
	name= "getC14",
	def=function(
	object
	){standardGeneric("getC14")}
)



#' automatic title
#' 
#' @param object see method arguments
#' @s4methods
#' @autocomment
setGeneric ( 
	name= "getCumulativeC",
	def=function(
	object
	){standardGeneric("getCumulativeC")}
)

#' Generic that yields the ^{14}C fraction for the content all pools and all times
#'
setGeneric ( 
	name= "getF14",
	def=function(
	object
	){standardGeneric("getF14")}
)



#' automatic title
#' 
#' @param object see method arguments
#' @s4methods
#' @autocomment
setGeneric ( 
	name= "getReleaseFlux14",
	def=function(
	object
	){standardGeneric("getReleaseFlux14")}
)

#' Generic that yields the ^{14}C fraction for the release flux of all pools and all times
#'
setGeneric ( 
  name= "getF14R",
  def=function(
	 object
	 ){standardGeneric("getF14R")}
  )

#' Generic that yields the ^{14}C fraction for the cumulative content of all pools and all times
#'
setGeneric ( 
  name= "getF14C",
  def=function(
	 object
	 ){standardGeneric("getF14C")}
  )



#' automatic title
#' 
#' @param object see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="getTimeRange",
	 def=function(object){
	     standardGeneric("getTimeRange")
	 }
)



#' automatic title
#' 
#' @param object see method arguments
#' @param timeSymbol see method arguments
#' @param poolNames see method arguments
#' @param numberOfPools see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="getFunctionDefinition",
	 def=function(object,timeSymbol,poolNames,numberOfPools){
	     standardGeneric("getFunctionDefinition")
	 }
)



#' automatic title
#' 
#' @param object see method arguments
#' @param timeSymbol see method arguments
#' @param poolNames see method arguments
#' @param numberOfPools see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="getRightHandSideOfODE",
	 def=function(object,timeSymbol,poolNames,numberOfPools){
	     standardGeneric("getRightHandSideOfODE")
	 }
)



#' automatic title
#' 
#' @param object see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="getNumberOfPools",
	 def=function(object){
	     standardGeneric("getNumberOfPools")
	 }
)



#' automatic title
#' 
#' @param object see method arguments
#' @param i see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="getOutputReceivers",
	 def=function(object,i){
	     standardGeneric("getOutputReceivers")
	 }
)



#' automatic title
#' 
#' @param object see method arguments
#' @s4methods
#' @autocomment 
setGeneric(
	 name="getDecompOp",
	 def=function(object){
	     standardGeneric("getDecompOp")
	 }
)



#' automatic title
#' 
#' @param object see method arguments
#' @s4methods
#' @autocomment 
setGeneric(
	 name="getConstantInFluxVector",
	 def=function(object){
	     standardGeneric("getConstantInFluxVector")
	 }
)



#' Extract the influxes 
#' 
#' @param object see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="getInFluxes",
	 def=function(object){
	     standardGeneric("getInFluxes")
	 }
)



#' automatic title
#' 
#' @param object see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="availableParticleProperties",
	 def=function(object){
	     standardGeneric("availableParticleProperties")
	 }
)



#' automatic title
#' 
#' @param object see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="availableParticleSets",
	 def=function(object){
	     standardGeneric("availableParticleSets")
	 }
)



#' automatic title
#' 
#' @param object see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="availableResidentSets",
	 def=function(object){
	     standardGeneric("availableResidentSets")
	 }
)



#' automatic title
#' 
#' @param object see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="computeResults",
	 def=function(object){
	     standardGeneric("computeResults")
	 }
)



#' automatic title
#' 
#' @param object see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="getDotOut",
	 def=function(object){
	     standardGeneric("getDotOut")
	 }
)



#' automatic title
#' 
#' @param object see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="getTransferMatrixFunc",
	 def=function(object){
	     standardGeneric("getTransferMatrixFunc")
	 }
)



#' automatic title
#' 
#' @param object see method arguments
#' @param timeSymbol see method arguments
#' @param state_variable_names see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="getCompartmentalMatrixFunc",
	 def=function(
         object
         ,timeSymbol
         ,state_variable_names
    ){
	     standardGeneric("getCompartmentalMatrixFunc")
	 }
)



#' automatic title
#' 
#' @param object see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="getConstantCompartmentalMatrix",
	 def=function(
         object
    ){
	     standardGeneric("getConstantCompartmentalMatrix")
	 }
)



#' automatic title
#' 
#' @param object see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="getConstantInternalFluxRateList_by_PoolIndex",
	 def=function(
         object
    ){
	     standardGeneric("getConstantInternalFluxRateList_by_PoolIndex")
	 }
)



#' automatic title
#' 
#' @param object see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="getConstantOutFluxRateList_by_PoolIndex",
	 def=function(
         object
    ){
	     standardGeneric("getConstantOutFluxRateList_by_PoolIndex")
	 }
)



#' automatic title
#' 
#' @param object see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="getConstLinDecompOp",
	 def=function(
         object
    ){
	     standardGeneric("getConstLinDecompOp")
	 }
)



#' automatic title
#' 
#' @param object see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="getLinearScaleFactor",
	 def=function(
         object
    ){
	     standardGeneric("getLinearScaleFactor")
	 }
)



#' automatic title
#' 
#' @param object see method arguments
#' @param as.closures see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="getTransferCoefficients",
	 def=function(object){
	     standardGeneric("getTransferCoefficients")
	 }
)



#' automatic title
#' 
#' @param object see method arguments
#' @param as.closures see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="getTransferCoefficients",
	 def=function(object,as.closures=F){
	     standardGeneric("getTransferCoefficients")
	 }
)

#' constructor for \code{\link{TimeMap-class}}
#' 
setGeneric(
	 name="TimeMap",
	 def=function 
	 (
	   map,
	   starttime,
	   endtime,
       times,
       data,
	   lag=0,                  
	   interpolation=splinefun,
       ...
     )
	 {
	     standardGeneric("TimeMap")
	 }
)

#' automatic title
#' 
#' @param map see method arguments
#' @param starttime see method arguments
#' @param endtime see method arguments
#' @param times see method arguments
#' @param data see method arguments
#' @param lag see method arguments
#' @param interpolation see method arguments
#' @param ... see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="ScalarTimeMap",
	 def=function 
	 (
	   map,
	   starttime,
	   endtime,
       times,
       data,
	   lag=0,                  
	   interpolation=splinefun,
       ...
     )
	 {
	     standardGeneric("ScalarTimeMap")
	 }
)

#' automatic title
#' 
#' @param format see method arguments
#' @param ... see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="BoundFc",
	 def=function 
	 (
      format,
      ...
   )
	 {
	     standardGeneric("BoundFc")
	 }
)

#' automatic title
#' 
#' @param map see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="UnBoundInFluxes",
	 def=function 
	 (map)
	 {
	     standardGeneric("UnBoundInFluxes")
	 }
)

#' automatic title
#' 
#' @param map see method arguments
#' @param numberOfPools see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="ConstInFluxes",
	 def=function(map,numberOfPools)
	 {
	     standardGeneric("ConstInFluxes")
	 }
)

#' A generic factory for subclasses of GeneralDecompOp
#'
#' The class of the output depends on the provided arguments
setGeneric(
	 name="GeneralDecompOp",
	 def=function 
	 (object)
	 {
	     standardGeneric("GeneralDecompOp")
	 }
)

#' A generic factory for subclasses of \linkS4class{InFluxes}
#'
#' The actual class of the returned object depends on the arguments 
#' provided
setGeneric(
	 name="InFluxes",
	 def=function 
	 (object,numberOfPools)
	 {
	     standardGeneric("InFluxes")
	 }
)

#' Generic constructor for the class with the same name
#'
setGeneric(
	 name="ConstantInternalFluxRate_by_PoolName",
	 def=function(sourceName,destinationName,src_to_dest,rate_constant)
	 {
	     standardGeneric("ConstantInternalFluxRate_by_PoolName")
	 }
)

#' Generic constructor for the class with the same name
#'
setGeneric(
	 name="ConstantInternalFluxRate_by_PoolIndex",
	 def=function(sourceIndex,destinationIndex,src_to_dest,rate_constant)
	 {
	     standardGeneric("ConstantInternalFluxRate_by_PoolIndex")
	 }
)

#' Generic constructor for the class with the same name
#'
setGeneric(
     name='ConstantInFluxList_by_PoolName',
	 def=function(object)
	 {
	     standardGeneric('ConstantInFluxList_by_PoolName')
	 }
)

#' Generic constructor for the class with the same name
#'
setGeneric(
     name='StateIndependentInFluxList_by_PoolIndex',
	 def=function(object)
	 {
	     standardGeneric('StateIndependentInFluxList_by_PoolIndex')
	 }
)

#' Generic constructor for the class with the same name
#'
setGeneric(
     name='StateIndependentInFluxList_by_PoolName',
	 def=function(object)
	 {
	     standardGeneric('StateIndependentInFluxList_by_PoolName')
	 }
)

#' Generic constructor for the class with the same name
#'
setGeneric(
     name='ConstantInFluxList_by_PoolIndex',
	 def=function(object)
	 {
	     standardGeneric('ConstantInFluxList_by_PoolIndex')
	 }
)

#' Generic constructor for the class with the same name
#'
setGeneric(
	 name="InternalFlux_by_PoolIndex",
	 def=function(func,sourceIndex,destinationIndex,src_to_dest)
	 {
	     standardGeneric("InternalFlux_by_PoolIndex")
	 }
)

#' Generic constructor for the class with the same name
#'
setGeneric(
	 name="InternalFlux_by_PoolName",
	 def=function(func,sourceName,destinationName,src_to_dest)
	 {
	     standardGeneric("InternalFlux_by_PoolName")
	 }
)

#' Generic constructor for the class with the same name
#'
setGeneric(
	 name="InternalFluxList_by_PoolName",
	 def=function(object) {
	     standardGeneric("InternalFluxList_by_PoolName")
	 }
)

#' Generic constructor for the class with the same name
#'
setGeneric(
	 name="InternalFluxList_by_PoolIndex",
	 def=function(object)
	 {
	     standardGeneric("InternalFluxList_by_PoolIndex")
	 }
)

#' Generic constructor for the class with the same name
#'
setGeneric(
	 name="InFlux_by_PoolIndex",
	 def=function(func,destinationIndex)
	 {
	     standardGeneric("InFlux_by_PoolIndex")
	 }
)

#' Generic constructor for an influx to a single pool from an ordered pair of PoolName (string like) and function  objects 
#'
setGeneric(
	 name="InFlux_by_PoolName",
	 def=function(func,destinationName)
	 {
	     standardGeneric("InFlux_by_PoolName")
	 }
)

#' Generic constructor for the class with the same name
#'
setGeneric(
	 name="OutFlux_by_PoolIndex",
	 def=function(func,sourceIndex)
	 {
	     standardGeneric("OutFlux_by_PoolIndex")
	 }
)

#' Generic constructor for the class with the same name
#'
setGeneric(
	 name="OutFlux_by_PoolName",
	 def=function(func,sourceName)
	 {
	     standardGeneric("OutFlux_by_PoolName")
	 }
)

#' Generic constructor for the class with the same name
#'
setGeneric(
	 name="InFluxList_by_PoolName",
	 def=function(object)
	 {
	     standardGeneric("InFluxList_by_PoolName")
	 }
)

#' Generic constructor for the class with the same name
#'
setGeneric(
	 name="InFluxList_by_PoolIndex",
	 def=function(object)
	 {
	     standardGeneric("InFluxList_by_PoolIndex")
	 }
)

#' Generic constructor for the class with the same name
#'
setGeneric(
	 name="OutFluxList_by_PoolName",
	 def=function(object)
	 {
	     standardGeneric("OutFluxList_by_PoolName")
	 }
)

#' Generic constructor for the class with the same name
#'
setGeneric(
	 name="ConstantOutFluxRateList_by_PoolName",
	 def=function(object)
	 {
	     standardGeneric("ConstantOutFluxRateList_by_PoolName")
	 }
)

#' Generic constructor for the class with the same name
#'
setGeneric(
	 name="ConstantOutFluxRate_by_PoolIndex",
	 def=function(sourceIndex,rate_constant)
	 {
	     standardGeneric("ConstantOutFluxRate_by_PoolIndex")
	 }
)

#' Generic constructor for the class with the same name
#'
setGeneric(
	 name="ConstantOutFluxRateList_by_PoolIndex",
	 def=function(object)
	 {
	     standardGeneric("ConstantOutFluxRateList_by_PoolIndex")
	 }
)

#' Generic constructor for the class with the same name
#'
setGeneric(
	 name="ConstantInternalFluxRateList_by_PoolName",
	 def=function(object){
	     standardGeneric("ConstantInternalFluxRateList_by_PoolName")
	 }
)

#' Generic constructor for the class with the same name
#'
setGeneric(
	 name="ConstantInternalFluxRateList_by_PoolIndex",
	 def=function(object){
	     standardGeneric("ConstantInternalFluxRateList_by_PoolIndex")
	 }
)

#' Generic constructor for the class with the same name
#'
setGeneric(
	 name="OutFluxList_by_PoolIndex",
	 def=function(object){
	     standardGeneric("OutFluxList_by_PoolIndex")
	 }
)

#' Generic constructor for the class with the same name
#'
setGeneric(
	 name="InFlux",
	 def=function(map,...) {
	     standardGeneric("InFlux")
	 }
)

#' Generic constructor for the class with the same name
#'
setGeneric(
	 name="OutFlux",
	 def=function (map,...) {
	     standardGeneric("OutFlux")
	 }
)


#' Generic constructor for the class with the same name
#'
setGeneric(
	 name="ConstLinDecompOp",
	 def=function(
        mat
        ,internal_flux_rates
        ,out_flux_rates
        ,numberOfPools
        ,poolNames
    ){
	     standardGeneric("ConstLinDecompOp")
	 }
)

#' Generic constructor for the class with the same name
#'
setGeneric(
	 name="ConstLinDecompOp_by_PoolName",
	 def=function(internal_flux_rates,out_flux_rates,poolNames) {
	     standardGeneric("ConstLinDecompOp_by_PoolName")
	 }
)

#' Generic constructor for the class with the same name
#'
setGeneric(
	 name="ConstLinDecompOpWithLinearScalarFactor",
	 def=function(
        mat
        ,internal_flux_rates
        ,out_flux_rates
        ,numberOfPools
        #poolNames
        ,xi
    )
	 {
	     standardGeneric("ConstLinDecompOpWithLinearScalarFactor")
	 }
)

#' Generic constructor for the class with the same name
#'
setGeneric(
	 name="UnBoundLinDecompOp",
	 def=function 
	 (matFunc)
	 {
	     standardGeneric("UnBoundLinDecompOp")
	 }
)

#' Generic constructor for the class with the same name
#'
setGeneric(
	 name="BoundLinDecompOp",
	 def=function 
	 (
    map,
    ...
   )
	 {
	     standardGeneric("BoundLinDecompOp")
	 }
)

#' Generic constructor for the class with the same name
#'
setGeneric(
	 name="UnBoundNonLinDecompOp",
	 def=function(
        matFunc
        ,internal_fluxes
        ,out_fluxes
        ,numberOfPools
        ,state_variable_names
        ,timeSymbol
        ,operator)
	 {
	     standardGeneric("UnBoundNonLinDecompOp")
	 }
)

#' Generic constructor for the class with the same name
#'
setGeneric(
	 name="UnBoundNonLinDecompOp_by_PoolNames",
	 def=function 
	 (internal_fluxes,out_fluxes,timeSymbol)
	 {
	     standardGeneric("UnBoundNonLinDecompOp_by_PoolNames")
	 }
)

#' Generic plotter
#'
#' @param x An argument containing sufficient information about the connections between the pools as well as from and to the exterior.  
setGeneric(
	 name="plotPoolGraph",
	 def=function (x)
	 {
	     standardGeneric("plotPoolGraph")
	 }
)

#' automatic title
#' 
#' @param x see method arguments
#' @param ... see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="add_plot",
	 def=function 
	 (
    x,
    ...
   )
	 {
	     standardGeneric("add_plot")
	 }
)



#' automatic title
#' 
#' @param id see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="GeneralPoolId",
	 def=function  (id) {
	     standardGeneric("GeneralPoolId")
	 }
)



#' automatic title
#' 
#' @param source see method arguments
#' @param destination see method arguments
#' @param src_to_dest see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="PoolConnection_by_PoolIndex",
	 def=function  
	 ( source ,destination,src_to_dest)
	 {
	     standardGeneric("PoolConnection_by_PoolIndex")
	 }
)



#' automatic title
#' 
#' @param source see method arguments
#' @param destination see method arguments
#' @param src_to_dest see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="PoolConnection_by_PoolName",
	 def=function  
	 ( source ,destination,src_to_dest)
	 {
	     standardGeneric("PoolConnection_by_PoolName")
	 }
)



#' automatic title
#' 
#' @param id see method arguments
#' @param ... see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="PoolName",
	 def=function(id,...){
	     standardGeneric("PoolName")
	 }
)



#' automatic title
#' 
#' @param id see method arguments
#' @param ... see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="PoolIndex",
	 def=function(id,...){
	     standardGeneric("PoolIndex")
	 }
)



#' automatic title
#' 
#' @param obj see method arguments
#' @param pools see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="check_pool_ids",
	 def=function(obj,pools){
	     standardGeneric("check_pool_ids")
	 }
)



#' automatic title
#' 
#' @param id see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="GeneralPoolId",
	 def=function(id){
	     standardGeneric("GeneralPoolId")
	 }
)



#' automatic title
#' 
#' @param obj see method arguments
#' @param poolNames see method arguments
#' @param timeSymbol see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="by_PoolIndex",
	 def=function(obj,poolNames,timeSymbol){
	     standardGeneric("by_PoolIndex")
	 }
)



#' automatic title
#' 
#' @param obj see method arguments
#' @param poolNames see method arguments
#' @s4methods
#' @autocomment
setGeneric(
	 name="by_PoolName",
	 def=function(obj,poolNames){
	     standardGeneric("by_PoolName")
	 }
)
