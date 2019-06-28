correctnessOfFc=function
(object 
)
{
   res=TRUE
   supported_formats <- c("Delta14C","AbsoluteFractionModern")
   f=object@format
   if (!any(grepl(f,supported_formats))){
      err_str=cat("The required format:",f," describing the c_14 fraction is not supported.\n 
   	     The supported formats are: ",supported_formats,". \n",sep="")
      stop(simpleError(err_str))
      return(res)
   }
}
setClass( 
    Class="Fc",
    ,
    contains="VIRTUAL"
    ,
    slots=c(format='character')
    ,
    validity=correctnessOfFc 
)
setMethod(
    f="getFormat",
    signature="Fc",
    definition=function(
			object 
			){
        return(object@format)
    }
)
