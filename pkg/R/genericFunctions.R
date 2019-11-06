


#' S4 generic: Delta14C
#' 
#' @name Delta14C
#' @param F see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
  name="Delta14C",
  def=function( 
  F 
  ){
	   standardGeneric("Delta14C")
  }
)



#' S4 generic: Delta14C_from_AbsoluteFractionModern
#' 
#' @name Delta14C_from_AbsoluteFractionModern
#' @param AbsoluteFractionModern see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="Delta14C_from_AbsoluteFractionModern",
	 def=function( 
	 AbsoluteFractionModern 
	 ){
	     standardGeneric("Delta14C_from_AbsoluteFractionModern")
	 }
)

#' conversion
#' param delta14C Object to be converted to AbsoluteFractionModern
setGeneric(
	 name="AbsoluteFractionModern_from_Delta14C",
	 def=function(delta14C){
	     standardGeneric("AbsoluteFractionModern_from_Delta14C")
	 }
)



#' S4 generic: AbsoluteFractionModern
#' 
#' @name AbsoluteFractionModern
#' @param F see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="AbsoluteFractionModern",
	 def=function( 
	 F 
	 ){
	     standardGeneric("AbsoluteFractionModern")
	 }
)



#' S4 generic: getFormat
#' 
#' @name getFormat
#' @param object see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="getFormat",
	 def=function( 
	 object 
	 ){
	     standardGeneric("getFormat")
	 }
)



#' S4 generic: getValues
#' 
#' @name getValues
#' @param object see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="getValues",
	 def=function( 
	 object 
	 ){
	     standardGeneric("getValues")
	 }
)



#' Automatic description: AbsoluteFractionModern_from_Delta14C,numeric-method
#' 
#' @name AbsoluteFractionModern_from_Delta14C,numeric-method
#' @param delta14C : object of class:\code{numeric}, no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
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



#' Automatic description: Delta14C_from_AbsoluteFractionModern,numeric-method
#' 
#' @name Delta14C_from_AbsoluteFractionModern,numeric-method
#' @param AbsoluteFractionModern : object of class:\code{numeric}, no manual
#' documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
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



#' Automatic description: AbsoluteFractionModern_from_Delta14C,matrix-method
#' 
#' @name AbsoluteFractionModern_from_Delta14C,matrix-method
#' @param delta14C : object of class:\code{matrix}, no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
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



#' Automatic description: Delta14C_from_AbsoluteFractionModern,matrix-method
#' 
#' @name Delta14C_from_AbsoluteFractionModern,matrix-method
#' @param AbsoluteFractionModern : object of class:\code{matrix}, no manual
#' documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
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



#' S4 generic: getMeanTransitTime
#' 
#' @name getMeanTransitTime
#' @param object see method arguments
#' @param inputDistribution see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric ( 
	name= "getMeanTransitTime",
	def=function(
	   object,           
	   inputDistribution 
	){standardGeneric("getMeanTransitTime")}
)



#' S4 generic: getTransitTimeDistributionDensity
#' 
#' @name getTransitTimeDistributionDensity
#' @param object see method arguments
#' @param inputDistribution see method arguments
#' @param times see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric ( 
	name= "getTransitTimeDistributionDensity",
	def=function(
	             object, 
	             inputDistribution, 
	             times 
	){standardGeneric("getTransitTimeDistributionDensity")}
)



#' S4 generic: getTimes
#' 
#' @name getTimes
#' @param object see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric (
	name= "getTimes",
	def=function(
	object){standardGeneric("getTimes")}
)



#' S4 generic: getInitialValues
#' 
#' @name getInitialValues
#' @param object see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
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

#' Generic Function to obtain the contents of the pools for all time steps
#'
setGeneric ( 
	name= "getC",
	def=function(
	object 
  ,as.closures=F 
	){standardGeneric("getC")
	}
)



#' S4 generic: getParticleMonteCarloSimulator
#' 
#' @name getParticleMonteCarloSimulator
#' @param object see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
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



#' S4 generic: getAccumulatedRelease
#' 
#' @name getAccumulatedRelease
#' @param object see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
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



#' S4 generic: getCumulativeC
#' 
#' @name getCumulativeC
#' @param object see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
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



#' S4 generic: getReleaseFlux14
#' 
#' @name getReleaseFlux14
#' @param object see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
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



#' S4 generic: getTimeRange
#' 
#' @name getTimeRange
#' @param object see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="getTimeRange",
	 def=function(object){
	     standardGeneric("getTimeRange")
	 }
)



#' S4 generic: getLaggingTimeRange
#' 
#' @name getLaggingTimeRange
#' @param object see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="getLaggingTimeRange",
	 def=function(object){
	     standardGeneric("getLaggingTimeRange")
	 }
)



#' S4 generic: getFunctionDefinition
#' 
#' @name getFunctionDefinition
#' @param object see method arguments
#' @param timeSymbol see method arguments
#' @param poolNames see method arguments
#' @param numberOfPools see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="getFunctionDefinition",
	 def=function(object,timeSymbol,poolNames,numberOfPools){
	     standardGeneric("getFunctionDefinition")
	 }
)



#' S4 generic: getRightHandSideOfODE
#' 
#' @name getRightHandSideOfODE
#' @param object see method arguments
#' @param timeSymbol see method arguments
#' @param poolNames see method arguments
#' @param numberOfPools see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="getRightHandSideOfODE",
	 def=function(object,timeSymbol,poolNames,numberOfPools){
	     standardGeneric("getRightHandSideOfODE")
	 }
)



#' S4 generic: getNumberOfPools
#' 
#' @name getNumberOfPools
#' @param object see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="getNumberOfPools",
	 def=function(object){
	     standardGeneric("getNumberOfPools")
	 }
)



#' S4 generic: getOutputReceivers
#' 
#' @name getOutputReceivers
#' @param object see method arguments
#' @param i see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="getOutputReceivers",
	 def=function(object,i){
	     standardGeneric("getOutputReceivers")
	 }
)



#' S4 generic: getDecompOp
#' 
#' @name getDecompOp
#' @param object see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="getDecompOp",
	 def=function(object){
	     standardGeneric("getDecompOp")
	 }
)



#' S4 generic: getConstantInFluxVector
#' 
#' @name getConstantInFluxVector
#' @param object see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="getConstantInFluxVector",
	 def=function(object){
	     standardGeneric("getConstantInFluxVector")
	 }
)



#' S4 generic: getInFluxes
#' 
#' @name getInFluxes
#' @param object see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="getInFluxes",
	 def=function(object){
	     standardGeneric("getInFluxes")
	 }
)



#' S4 generic: availableParticleProperties
#' 
#' @name availableParticleProperties
#' @param object see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="availableParticleProperties",
	 def=function(object){
	     standardGeneric("availableParticleProperties")
	 }
)



#' S4 generic: availableParticleSets
#' 
#' @name availableParticleSets
#' @param object see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="availableParticleSets",
	 def=function(object){
	     standardGeneric("availableParticleSets")
	 }
)



#' S4 generic: availableResidentSets
#' 
#' @name availableResidentSets
#' @param object see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="availableResidentSets",
	 def=function(object){
	     standardGeneric("availableResidentSets")
	 }
)



#' S4 generic: computeResults
#' 
#' @name computeResults
#' @param object see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="computeResults",
	 def=function(object){
	     standardGeneric("computeResults")
	 }
)



#' S4 generic: getDotOut
#' 
#' @name getDotOut
#' @param object see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="getDotOut",
	 def=function(object){
	     standardGeneric("getDotOut")
	 }
)



#' S4 generic: getTransferMatrix
#' 
#' @name getTransferMatrix
#' @param object see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="getTransferMatrix",
	 def=function(object){
         warning('The function has been renamed to "getTransferMatrixFunc".
                 Please adapt your code to get rid of this warning.')
	     getTransferMatrixFunc(object)
	 }
)



#' S4 generic: getTransferMatrixFunc
#' 
#' @name getTransferMatrixFunc
#' @param object see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="getTransferMatrixFunc",
	 def=function(object){
	     standardGeneric("getTransferMatrixFunc")
	 }
)



#' S4 generic: getCompartmentalMatrixFunc
#' 
#' @name getCompartmentalMatrixFunc
#' @param object see method arguments
#' @param timeSymbol see method arguments
#' @param state_variable_names see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
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



#' S4 generic: getConstantCompartmentalMatrix
#' 
#' @name getConstantCompartmentalMatrix
#' @param object see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="getConstantCompartmentalMatrix",
	 def=function(
         object
    ){
	     standardGeneric("getConstantCompartmentalMatrix")
	 }
)



#' S4 generic: getConstantInternalFluxRateList_by_PoolIndex
#' 
#' @name getConstantInternalFluxRateList_by_PoolIndex
#' @param object see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="getConstantInternalFluxRateList_by_PoolIndex",
	 def=function(
         object
    ){
	     standardGeneric("getConstantInternalFluxRateList_by_PoolIndex")
	 }
)



#' S4 generic: getConstantOutFluxRateList_by_PoolIndex
#' 
#' @name getConstantOutFluxRateList_by_PoolIndex
#' @param object see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="getConstantOutFluxRateList_by_PoolIndex",
	 def=function(
         object
    ){
	     standardGeneric("getConstantOutFluxRateList_by_PoolIndex")
	 }
)



#' S4 generic: getConstLinDecompOp
#' 
#' @name getConstLinDecompOp
#' @param object see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="getConstLinDecompOp",
	 def=function(
         object
    ){
	     standardGeneric("getConstLinDecompOp")
	 }
)



#' S4 generic: getLinearScaleFactor
#' 
#' @name getLinearScaleFactor
#' @param object see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="getLinearScaleFactor",
	 def=function(
         object
    ){
	     standardGeneric("getLinearScaleFactor")
	 }
)



#' S4 generic: getTransferCoefficients
#' 
#' @name getTransferCoefficients
#' @param object see method arguments
#' @param as.closures see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="getTransferCoefficients",
	 def=function(object){
	     standardGeneric("getTransferCoefficients")
	 }
)



#' S4 generic: getTransferCoefficients
#' 
#' @name getTransferCoefficients
#' @param object see method arguments
#' @param as.closures see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
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

#' S4 generic: ScalarTimeMap
#' 
#' @name ScalarTimeMap
#' @param map see method arguments
#' @param starttime see method arguments
#' @param endtime see method arguments
#' @param times see method arguments
#' @param data see method arguments
#' @param lag see method arguments
#' @param interpolation see method arguments
#' @param ... see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
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

#' S4 generic: BoundFc
#' 
#' @name BoundFc
#' @param format see method arguments
#' @param ... see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
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

#' S4 generic: UnBoundInFluxes
#' 
#' @name UnBoundInFluxes
#' @param map see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="UnBoundInFluxes",
	 def=function 
	 (map)
	 {
	     standardGeneric("UnBoundInFluxes")
	 }
)

#' S4 generic: ConstInFluxes
#' 
#' @name ConstInFluxes
#' @param map see method arguments
#' @param numberOfPools see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
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

#' S4 generic: add_plot
#' 
#' @name add_plot
#' @param x see method arguments
#' @param ... see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
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

#' S4 generic: getSrcDim
#' 
#' @name getSrcDim
#' @param obj see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="getSrcDim",
	 def=function  
	 (
    obj
   )
	 {
	     standardGeneric("getSrcDim")
	 }
)



#' S4 generic: GeneralPoolId
#' 
#' @name GeneralPoolId
#' @param id see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="GeneralPoolId",
	 def=function  (id) {
	     standardGeneric("GeneralPoolId")
	 }
)



## S4 generic: PoolConnection
## 
## @name PoolConnection
## @param source see method arguments
## @param destination see method arguments
## @param src_to_dest see method arguments
## @s4methods
## @autocomment These comments were created by the auto_comment_roclet by
## inspection of the code.  You can use the "update_auto_comment_roclet" to
## automatically adapt them to changes in the source code. This will remove
## `@param` tags for parameters that are no longer present in the source code
## and add `@param` tags with a default description for yet undocumented
## parameters.  If you remove this `@autocomment` tag your comments will no
## longer be touched by the "update_autocomment_roclet".
#setGeneric(
#	 name="PoolConnection",
#	 def=function  
#	 ( source ,destination,src_to_dest)
#	 {
#	     standardGeneric("PoolConnection")
#	 }
#)



#' S4 generic: PoolConnection_by_PoolIndex
#' 
#' @name PoolConnection_by_PoolIndex
#' @param source see method arguments
#' @param destination see method arguments
#' @param src_to_dest see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="PoolConnection_by_PoolIndex",
	 def=function  
	 ( source ,destination,src_to_dest)
	 {
	     standardGeneric("PoolConnection_by_PoolIndex")
	 }
)



#' S4 generic: PoolConnection_by_PoolName
#' 
#' @name PoolConnection_by_PoolName
#' @param source see method arguments
#' @param destination see method arguments
#' @param src_to_dest see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="PoolConnection_by_PoolName",
	 def=function  
	 ( source ,destination,src_to_dest)
	 {
	     standardGeneric("PoolConnection_by_PoolName")
	 }
)



#' S4 generic: PoolName
#' 
#' @name PoolName
#' @param id see method arguments
#' @param ... see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="PoolName",
	 def=function(id,...){
	     standardGeneric("PoolName")
	 }
)



#' S4 generic: PoolIndex
#' 
#' @name PoolIndex
#' @param id see method arguments
#' @param ... see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="PoolIndex",
	 def=function(id,...){
	     standardGeneric("PoolIndex")
	 }
)



#' S4 generic: check_pool_ids
#' 
#' @name check_pool_ids
#' @param obj see method arguments
#' @param pools see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="check_pool_ids",
	 def=function(obj,pools){
	     standardGeneric("check_pool_ids")
	 }
)



#' S4 generic: GeneralPoolId
#' 
#' @name GeneralPoolId
#' @param id see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="GeneralPoolId",
	 def=function(id){
	     standardGeneric("GeneralPoolId")
	 }
)



#' S4 generic: by_PoolIndex
#' 
#' @name by_PoolIndex
#' @param obj see method arguments
#' @param poolNames see method arguments
#' @param timeSymbol see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="by_PoolIndex",
	 def=function(obj,poolNames,timeSymbol){
	     standardGeneric("by_PoolIndex")
	 }
)



#' S4 generic: by_PoolName
#' 
#' @name by_PoolName
#' @param obj see method arguments
#' @param poolNames see method arguments
#' @s4methods
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setGeneric(
	 name="by_PoolName",
	 def=function(obj,poolNames){
	     standardGeneric("by_PoolName")
	 }
)
