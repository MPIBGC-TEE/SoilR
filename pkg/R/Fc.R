
#' @auto

#' @auto

#' @auto
setMethod(
    f="getFormat",
    signature="Fc",
    definition=function(
			object 
			){
        return(object@format)
    }
)
