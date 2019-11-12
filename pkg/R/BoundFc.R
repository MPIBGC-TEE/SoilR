
#' automatic title
#' 
#' @autocomment These comments were created by the auto_comment_roclet by
#' @param format : : no manual documentation
#' @param ... : : no manual documentation
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
  f="BoundFc",
  signature=signature(format='character'),
  definition=function 
(
  format,     
	   ...    
){
    obj <- as(TimeMap(...),"BoundFc")
    obj@format=format
    validObject(obj) 
    return(obj)
}
)

#' automatic title
#' 
#' @autocomment These comments were created by the auto_comment_roclet by
#' @param format : : no manual documentation
#' @param ... : : no manual documentation
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
  f="BoundFc",
  signature=signature(format='missing'),
  definition=function 
(
   format, 
  ... 
){
    l <- list(...)
    obj <- as(TimeMap(...),"BoundFc")
    obj@format=l$map$format
    validObject(obj) 
    return(obj)
}
)

#' automatic title
#' 
#' @autocomment These comments were created by the auto_comment_roclet by
#' @param F : : no manual documentation
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "Delta14C",
   signature("BoundFc"),
   definition=function
   (
    F 
	 ){
      f=F@format
            targetFormat="Delta14C"
            if (f==targetFormat){
	       return(F)
	    }
	    if (f=="AbsoluteFractionModern"){
	     f_afn=F@map
             f_d14C=function(t){
	         fd=Delta14C_from_AbsoluteFractionModern(f_afn(t))
	     return(fd)
	    }
	    D14C=F
	    D14C@map=f_d14C
	    D14C@format=targetFormat
	    return(D14C)
	    } 
      stop("conversion not implemented for this format")
    }	 
)

#' automatic title
#' 
#' @autocomment These comments were created by the auto_comment_roclet by
#' @param F : : no manual documentation
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
  f= "AbsoluteFractionModern",
      signature("BoundFc"),
      definition=function
	    (F 
	    ){
        f=F@format
              targetFormat="AbsoluteFractionModern"
              if (f==targetFormat){
	         return(F)
	      }
	      if (f=="Delta14C"){
	       f_d14C=F@map
               f_afn=function(t){
	           fprime=AbsoluteFractionModern_from_Delta14C(f_d14C(t))
	       return(fprime)
	       }
	       AFM_tm=F
	       AFM_tm@map=f_afn
	       AFM_tm@format=targetFormat
	       return(AFM_tm)
	      } 
            stop("conversion not implemented for this format")
     }	 
)
