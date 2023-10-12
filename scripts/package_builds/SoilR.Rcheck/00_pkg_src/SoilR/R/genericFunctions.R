#' Conversion of radiocarbon values 
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



#' Conversion of radiocarbon values
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

#' Conversion of radiocarbon values

#' @param delta14C Object to be converted to AbsoluteFractionModern
#' @s4methods
setGeneric(
	 name="AbsoluteFractionModern_from_Delta14C",
	 def=function(delta14C){
	     standardGeneric("AbsoluteFractionModern_from_Delta14C")
	 }
)



#' Conversion of radiocarbon values
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



#' Get format of SoilR object
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



#' Get values of SoilR object
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



#' Conversion of radiocarbon values, from Delta14C to absolute fraction modern
#' 
#' @param delta14C radiocarbon value in Delta14C
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



#' Conversion of radiocarbon values
#' 
#' @param AbsoluteFractionModern radiocarbon value in absolute fraction modern
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



#' Conversion of radiocarbon values
#' 
#' @param delta14C Matrix with radiocarbon values in Delta14C
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



#' Conversion of radiocarbon values
#' 
#' @param AbsoluteFractionModern Matrix of radiocarbon values in absolute fraction modern
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



#' Mean transit time for SoilR objects
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



#' Transit time distribution for SoilR objects
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



#' Time vector of SoilR object
#' 
#' @param object a SoilR object
#' @s4methods
#' @autocomment
setGeneric (
	name= "getTimes",
	def=function(
	object){standardGeneric("getTimes")}
)



#' Initial values of SoilR object
#' 
#' @param object a SoilR object
#' @s4methods
#' @autocomment
setGeneric (
	name= "getInitialValues",
	def=function(
	object){standardGeneric("getInitialValues")}
)

#' Generic Function to obtain the fluxes out of of the pools 
#'
#' @param object a SoilR object
#' @param as.closures a logical variable. Default to FALSE
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
#' @param as.closures a logical variable. Default to FALSE
#' @return A matrix with m columns representing where m is the number of pools, and n rows where n is the number times 
#' as specified by the \code{times} of the model. 
#' @s4methods
setGeneric ( 
	name= "getC",
	def=function(
	object 
  ,as.closures=F 
	){standardGeneric("getC")
	}
)

#' Calculates all stocks all fluxes to ,in and out of  the compartment system and also their integrals over time
#'
#' Have a look at the methods for details.
#' @template Model-param
#' @param as.closures a logical variable. Default to FALSE
#' @param params parameter values
#' @return A matrix with columns representing the name of the statevariable, flux and accumulated flux for every time
#' 
#' as specified by the \code{times} of the model. 
#' @s4methods

setGeneric ( 
	name= "getSolution",
	def=function(
	  object
    ,params 
    ,as.closures=F 
	){standardGeneric("getSolution")
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
#' @param object A SoilR object
#' @s4methods
setGeneric ( 
	name= "getReleaseFlux",
  valueClass='matrix',
	def=function
	(
	object 
	){standardGeneric("getReleaseFlux")
	}
)



#' Accumulated release flux out of the pools
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
#' @param object a SoilR object
#' @s4methods
setGeneric ( 
	name= "getC14",
	def=function(
	object
	){standardGeneric("getC14")}
)



#' Cummulative pool contents
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
#' @param object A SoilR object of class Model14
#' @s4methods
setGeneric ( 
	name= "getF14",
	def=function(
	object
	){standardGeneric("getF14")}
)



#' Generic that yields the ^{14}C fraction in the release flux
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
#' @param object a SoilR object of class Model14
#' @s4methods
setGeneric ( 
  name= "getF14R",
  def=function(
	 object
	 ){standardGeneric("getF14R")}
  )

#' Generic that yields the ^{14}C fraction for the cumulative content of all pools and all times
#'
#' @param object a SoilR object of class Model 14
#' @s4methods
setGeneric ( 
  name= "getF14C",
  def=function(
	 object
	 ){standardGeneric("getF14C")}
  )



#' Time range of a model simulation
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



#' Function definition of SoilR model
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



#' Right hand side of ODE of a SoilR model
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



#' Number of pools in a model
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



#' Pools receiving outputs from other pools
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



#' Decomposition operator of a model
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



#' Input flux vector
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



#' Available particle properties
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



#' Available particle sets
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



#' Available resident sets
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



#' Computes results
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



#' Dot out
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



#' Transfer matrix function
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



#' Compartmental matrix function
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



#' Constant compartmental matrix
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



#' Constant internal flux rate list by pool index
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



#' Constant out flux rate list by pool index
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



#' Constant linear decomposition operator
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



#' Linear scale factor
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



#' Transfer coefficients
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



#' Transfer coefficients
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

#' Constructor for \code{\linkS4class{Model_by_PoolNames}}
#' 
#' @param smod see methods
#' @param times a vector of times
#' @param mat a compartmental matrix
#' @param initialValues a vector of initial values
#' @param inputFluxes a vector of input fluxes
#' @param internal_fluxes flux rate among pools
#' @param out_fluxes flux rates out of pools
#' @param timeSymbol character symbol used to represent time
#' @param solverfunc function used to solve system of ODEs
#' @template  Model_by_PoolNames_Result
#' @s4methods
#' @autocomment 
setGeneric(
	 name="Model_by_PoolNames",
	 def=function 
	 (
	   smod,
     times,
     mat,
     initialValues,
     inputFluxes,
     internal_fluxes,
     out_fluxes ,
     timeSymbol,
     #pass,
     solverfunc
   ) {
	     standardGeneric("Model_by_PoolNames")
	 }
)

#' Constructor for \code{\link{TimeMap-class}}
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

#' Constructor for \code{\link{ScalarTimeMap-class}}
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

#' Bound Fc object
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

#' Unbound input fluxes
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

#' Constant input fluxes
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
#' @param object A SoilR object
#' @s4methods
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
#' @param object a SoilR object
#' @param numberOfPools number of pools in the model
#' @s4methods
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
#' @param sourceName name of the source pool
#' @param destinationName name of the destination pool
#' @param src_to_dest flux rate
#' @param rate_constant rate constant
#' @s4methods
setGeneric(
	 name="ConstantInternalFluxRate_by_PoolName",
	 def=function(sourceName,destinationName,src_to_dest,rate_constant)
	 {
	     standardGeneric("ConstantInternalFluxRate_by_PoolName")
	 }
)

#' Generic constructor for the class with the same name
#' 
#' @param sourceIndex index of source pool
#' @param destinationIndex index of destination pool
#' @param src_to_dest flux rate
#' @param rate_constant rate constant
#' @s4methods
setGeneric(
	 name="ConstantInternalFluxRate_by_PoolIndex",
	 def=function(sourceIndex,destinationIndex,src_to_dest,rate_constant)
	 {
	     standardGeneric("ConstantInternalFluxRate_by_PoolIndex")
	 }
)

#' Generic constructor for the class with the same name
#'
#' @param object see methods
#' @s4methods
setGeneric(
     name='ConstantInFluxList_by_PoolName',
	 def=function(object)
	 {
	     standardGeneric('ConstantInFluxList_by_PoolName')
	 }
)

#' Generic constructor for the class with the same name
#'
#' @param object see methods
#' @s4methods
setGeneric(
     name='StateIndependentInFluxList_by_PoolIndex',
	 def=function(object)
	 {
	     standardGeneric('StateIndependentInFluxList_by_PoolIndex')
	 }
)

#' Generic constructor for the class with the same name
#'
#' @param object see methods
#' @s4methods
setGeneric(
     name='StateIndependentInFluxList_by_PoolName',
	 def=function(object)
	 {
	     standardGeneric('StateIndependentInFluxList_by_PoolName')
	 }
)

#' Generic constructor for the class with the same name
#'
#' @param object see methods
#' @s4methods
setGeneric(
     name='ConstantInFluxList_by_PoolIndex',
	 def=function(object)
	 {
	     standardGeneric('ConstantInFluxList_by_PoolIndex')
	 }
)

#' Generic constructor for the class with the same name
#'
#' @param func function with flux rate
#' @param sourceIndex index of the source pool
#' @param destinationIndex index of the destination pool
#' @param src_to_dest source to destination
#' @s4methods
setGeneric(
	 name="InternalFlux_by_PoolIndex",
	 def=function(func,sourceIndex,destinationIndex,src_to_dest)
	 {
	     standardGeneric("InternalFlux_by_PoolIndex")
	 }
)

#' Generic constructor for the class with the same name
#'
#' @param func function with flux rate
#' @param sourceName name of source pool
#' @param destinationName name of destination pool
#' @param src_to_dest source to destination
#' @s4methods
setGeneric(
	 name="InternalFlux_by_PoolName",
	 def=function(func,sourceName,destinationName,src_to_dest)
	 {
	     standardGeneric("InternalFlux_by_PoolName")
	 }
)

#' Generic constructor for the class with the same name
#'
#' @param object see methods
#' @s4methods
setGeneric(
	 name="InternalFluxList_by_PoolName",
	 def=function(object) {
	     standardGeneric("InternalFluxList_by_PoolName")
	 }
)

#' Generic constructor for the class with the same name
#'
#' @param object see methods
#' @s4methods
setGeneric(
	 name="InternalFluxList_by_PoolIndex",
	 def=function(object)
	 {
	     standardGeneric("InternalFluxList_by_PoolIndex")
	 }
)

#' Generic constructor for the class with the same name
#'
#' @param func function with influx
#' @param destinationIndex index of destination pool
#' @s4methods
setGeneric(
	 name="InFlux_by_PoolIndex",
	 def=function(func,destinationIndex)
	 {
	     standardGeneric("InFlux_by_PoolIndex")
	 }
)

#' Generic constructor for an influx to a single pool from an ordered pair of PoolName (string like) and function  objects 
#'
#' @param func function with input flux
#' @param destinationName name of the destination pool
#' @s4methods
setGeneric(
	 name="InFlux_by_PoolName",
	 def=function(func,destinationName)
	 {
	     standardGeneric("InFlux_by_PoolName")
	 }
)

#' Generic constructor for the class with the same name
#'
#' @param func function with output flux
#' @param sourceIndex index of the source pool
#' @s4methods
setGeneric(
	 name="OutFlux_by_PoolIndex",
	 def=function(func,sourceIndex)
	 {
	     standardGeneric("OutFlux_by_PoolIndex")
	 }
)

#' Generic constructor for the class with the same name
#'
#' @param func function with output flux
#' @param sourceName name of the source pool
#' @s4methods
setGeneric(
	 name="OutFlux_by_PoolName",
	 def=function(func,sourceName)
	 {
	     standardGeneric("OutFlux_by_PoolName")
	 }
)

#' Generic constructor for the class with the same name
#'
#' @param object see methods
#' @s4methods
setGeneric(
	 name="InFluxList_by_PoolName",
	 def=function(object)
	 {
	     standardGeneric("InFluxList_by_PoolName")
	 }
)

#' Generic constructor for the class with the same name
#'
#' @param object see methods
#' @s4methods
setGeneric(
	 name="InFluxList_by_PoolIndex",
	 def=function(object)
	 {
	     standardGeneric("InFluxList_by_PoolIndex")
	 }
)

#' Generic constructor for the class with the same name
#'
#' @param object see methods
#' @s4methods
setGeneric(
	 name="OutFluxList_by_PoolName",
	 def=function(object)
	 {
	     standardGeneric("OutFluxList_by_PoolName")
	 }
)

#' Generic constructor for the class with the same name
#'
#' @param object see methods
#' @s4methods
setGeneric(
	 name="ConstantOutFluxRateList_by_PoolName",
	 def=function(object)
	 {
	     standardGeneric("ConstantOutFluxRateList_by_PoolName")
	 }
)

#' Generic constructor for the class with the same name
#'
#' @param sourceIndex index of the source pool
#' @param rate_constant rate of output flux
#' @s4methods
setGeneric(
	 name="ConstantOutFluxRate_by_PoolIndex",
	 def=function(sourceIndex,rate_constant)
	 {
	     standardGeneric("ConstantOutFluxRate_by_PoolIndex")
	 }
)

#' Generic constructor for the class with the same name
#'
#' @param object see methods
#' @s4methods
setGeneric(
	 name="ConstantOutFluxRateList_by_PoolIndex",
	 def=function(object)
	 {
	     standardGeneric("ConstantOutFluxRateList_by_PoolIndex")
	 }
)

#' Generic constructor for the class with the same name
#'
#' @param object see methods
#' @s4methods
setGeneric(
	 name="ConstantInternalFluxRateList_by_PoolName",
	 def=function(object){
	     standardGeneric("ConstantInternalFluxRateList_by_PoolName")
	 }
)

#' Generic constructor for the class with the same name
#'
#' @param object see methods
#' @s4methods
setGeneric(
	 name="ConstantInternalFluxRateList_by_PoolIndex",
	 def=function(object){
	     standardGeneric("ConstantInternalFluxRateList_by_PoolIndex")
	 }
)

#' Generic constructor for the class with the same name
#'
#' @param object see methods
#' @s4methods
setGeneric(
	 name="OutFluxList_by_PoolIndex",
	 def=function(object){
	     standardGeneric("OutFluxList_by_PoolIndex")
	 }
)

#' Generic constructor for the class with the same name
#'
#' @param map a SoilR map
#' @param ... additional arguments to function
#' @s4methods
setGeneric(
	 name="InFlux",
	 def=function(map,...) {
	     standardGeneric("InFlux")
	 }
)

#' Generic constructor for the class with the same name
#'
#' @param map a SoilR map
#' @param ... additional arguments
#' @s4methods
setGeneric(
	 name="OutFlux",
	 def=function (map,...) {
	     standardGeneric("OutFlux")
	 }
)


#' Generic constructor for the class with the same name
#'
#' @param mat a square compartmental matrix
#' @param internal_flux_rates rates of internal transfers among pools
#' @param out_flux_rates rates of transfer out of poolss
#' @param numberOfPools total number of pools in the system
#' @param poolNames names of all pools
#' @s4methods
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
#' @param internal_flux_rates rates of transfer among pools
#' @param out_flux_rates rates out of pools
#' @param poolNames names of the pools
#' @s4methods
setGeneric(
	 name="ConstLinDecompOp_by_PoolName",
	 def=function(internal_flux_rates,out_flux_rates,poolNames) {
	     standardGeneric("ConstLinDecompOp_by_PoolName")
	 }
)

#' Generic constructor for the class with the same name
#'
#' @param mat a square compartmental matrix
#' @param internal_flux_rates internal transfer rates among pools
#' @param out_flux_rates rates out of pools
#' @param numberOfPools number of pools in the system
#' @param xi rate modifier for the entire matrix
#' @s4methods
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
#' @param matFunc function providing a compartmental matrix
#' @s4methods
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
#' @param map A map
#' @param ... Additional arguments passed to function
#' @s4methods
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
#' @param matFunc function providing a compartmental matrix
#' @param internal_fluxes fluxes among pools
#' @param out_fluxes fluxes out of the pools
#' @param numberOfPools number of pools in the system
#' @param state_variable_names names of the pools
#' @param timeSymbol character used to represent time
#' @param operator a SoilR operator
#' @s4methods
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
#' @param internal_fluxes flux rates among pools
#' @param out_fluxes output flux rates from pools
#' @param timeSymbol character used to represent time
#' @s4methods
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
#' @s4methods
setGeneric(
	 name="plotPoolGraph",
	 def=function (x)
	 {
	     standardGeneric("plotPoolGraph")
	 }
)

#' Add elements to plot
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



#' General pool Id
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



#' Pool connection by pool index
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



#' Pool connection by pool name
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



#' Pool name
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



#' Pool index
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



#' Check pool ids
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



#' General pool Id
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
