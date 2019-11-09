


#' automatic title
#' 
#' @param .Object : no manual documentation
#' @param times : no manual documentation
#' @param mat : no manual documentation
#' @param initialValues : no manual documentation
#' @param inputFluxes : no manual documentation
#' @param solverfunc : no manual documentation
#' @param pass : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
    f="initialize",
    signature="Model",
    definition=function 
    (
        .Object,
        times=c(0,1),
        mat=ConstLinDecompOp(matrix(nrow=1,ncol=1,0)), 
        initialValues=numeric()
        ,
        inputFluxes= BoundInFluxes(
            function(t){
                return(matrix(nrow=1,ncol=1,1))
            },
            0,
            1
        )
        ,
        solverfunc=deSolve.lsoda.wrapper
        ,
        pass=FALSE
        ){
        .Object@times=times
        .Object@mat=mat
        .Object@initialValues=initialValues
        .Object@inputFluxes=inputFluxes
        .Object@solverfunc=solverfunc
        if (pass==FALSE) correctnessOfModel(.Object) 
        return(.Object)
    }
)






#' Constructor for class \linkS4class{Model}
#' 
#' This function creates an object of class \linkS4class{Model}, The arguments
#' can be given in different form as long as they can be converted to the
#' necessary internal building blocks.  (See the links)
#' 
#' This function \code{Model} wraps the internal constructor of class
#' \linkS4class{Model}. The internal constructor requires the argument \code{A}
#' to be of class \linkS4class{DecompOp} and argument \code{inputFluxes} to be
#' of class \linkS4class{InFluxes}. Before calling the internal constructor
#' \code{Model} calls \link{GeneralDecompOp} on its argument \code{A} and
#' \link{InFluxes} on its argument \code{inputFluxes} to convert them into
#' the required classes. Both are generic functions. Follow the links to see
#' for which kind of inputs conversion methods are available. The attempted
#' conversion allows great flexibility with respect to arguments and
#' independence from the actual implementation. However if your code uses the
#' wrong argument the error will most likely occur in the delegate functions.
#' If this happens analyse the errormassage (or use \code{traceback()}) to see
#' which function was called and try to call the constructor of the desired
#' subclass explicitly with your arguments.  The subclasses are linked in the
#' class documentation \linkS4class{DecompOp} or \linkS4class{InFluxes}
#' respectively.
#' 
#' Note also that this function checks its arguments quite elaborately and
#' tries to detect accidental unreasonable combinations, especially concerning
#' two kinds of errors. 
#' \enumerate{ 
#' \item unintended extrapolation of time series data 
#' \item violoations of massbalance by the DecompOp argument. 
#'}
#' 
#' SoilR has a lot of unit tests which are installed with the package and are
#' sometimes instructive as examples.  To see example scenarios for parameter
#' check look at:
#' \Sexpr{system.file('tests','runit.correctness_of_Model.R',package='SoilR')}
#' 
#' @param t A vector containing the points in time where the solution is
#' sought.
#' @param A something that can be converted by \link{GeneralDecompOp} to any of
#' the available subclasses of \code{\linkS4class{DecompOp}}.
#' @param ivList A numeric vector containing the initial amount of carbon for
#' the n pools. The length of this vector is equal to the number of pools. This
#' is checked by an internal function.
#' @param inputFluxes something that can be converted by \link{InFluxes}
#' to any of the available subclasses of \linkS4class{InFluxes}.
#' @param solverfunc The function used to actually solve the ODE system. The
#' default is \code{\link{deSolve.lsoda.wrapper}} but you can also provide your
#' own function that the same interface.
#' @param pass Forces the constructor to create the model even if it does not
#' pass internal sanity checks
#' @return An object of class \linkS4class{Model} that can be queried by many
#' methods to be found there.
#' @seealso This function is called by many of the \link{predefinedModels}. \cr
#' Package functions called in the examples:\cr
#' \code{\link{example.2DInFluxes.Args}},\cr
#' \code{\link{example.2DGeneralDecompOpArgs}},\cr
#' @examples
#' # examples from external files
#' # inst/tests/requireSoilR/runit.all.possible.Model.arguments.R test.all.possible.Model.arguments:
#' 
#'   # This example shows different kinds of arguments to the function Model.
#'   # The model objects we will build will share some common features.
#'   #  - two pools 
#'   #  - initial values 
#' 
#'        iv<-  c(5,6)
#' 
#'   #  - times 
#' 
#'        times <- seq(1,10,by=0.1)
#' 
#'   # The other parameters A and inputFluxes will be different
#'   # The function Model will transform these arguments 
#'   # into objects of the classes required by the internal constructor.
#'   # This leads to a number of possible argument types. 
#'   # We demonstrate some of the possibilities here.
#'   # Let us first look at the choeices for argument 'A'.
#'   
#'   #) 
#'   possibleAs  <- example.2DGeneralDecompOpArgs()
#'   
#'   # Since "Model" will call "InFluxes" on its "inputFluxes" 
#'   # argument there are again different choices
#'   # we have included a function in SoilR that produces 2D examples
#'   
#'   possibleInfluxes <- example.2DInFluxes.Args()
#'  print(possibleInfluxes$I.vec)
#'   # We can build a lot of  models from the possible combinations
#'   # for instance   
#'   #m1 <- Model(
#'   #        t=times,
#'   #        A=matrix(nrow=2,byrow=TRUE,c(-0.1,0,0,-0.2)),
#'   #        ivList=iv,
#'   #        inputFluxes=possibleInfluxes$I.vec) 
#'   ## We now produce that all combinations of As and InputFluxes
#'   combinations <- listProduct(possibleAs,possibleInfluxes)
#'   print(length(combinations))
#'   # an a Model for each
#'   models <- lapply(
#'               combinations,
#'               function(combi){
#'                 #Model(t=times,A=combi$A,ivList=iv,inputFluxes=combi$I)
#'                 Model(t=times,A=combi[[1]],ivList=iv,inputFluxes=combi[[2]])
#'               }
#'             )
#'   ## lets check that we can compute something# 
#'   lapply(models,getC)
#' 
Model <-function(t,			
   A,			
   ivList,		
   inputFluxes, 
   solverfunc=deSolve.lsoda.wrapper,		
   pass=FALSE  
)
{   
     obj=new(
        Class="Model"
        ,t
        ,GeneralDecompOp(A)
        ,ivList
        ,InFluxes(inputFluxes,numberOfPools=length(ivList))
        ,solverfunc
        ,pass
        )
     return(obj)
}




#' automatic title
#' 
#' @param object : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "getInFluxes",
      signature(object="Model"),
      definition=function(object){
      object@inputFluxes
   }
)



#' automatic title
#' 
#' @param x : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "plot",
      signature(x="Model"),
      definition=function 
      (x){
      plot(getTimes(x),getC(x)[,1])
   }
)



#' automatic title
#' 
#' @param x : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "plotPoolGraph",
      signature(x="Model"),
      definition=function 
      (x){
      op=getDecompOp(x)
      iflvec=getInFluxes(x)

      #call the function
      #internalConnections<-list(tuple(1,2),tuple(2,3),tuple(3,1),tuple(3,4))
      #inBoundConnections<-list(1,3)
      #outBoundConnections<-list(4)
      #plotPoolGraphFromTupleLists(internalConnections,inBoundConnections,outBoundConnections)
      
   }
)



#' automatic title
#' 
#' @param x : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "print",
      signature(x="Model"),
      definition=function
      (x){
   }
)



#' automatic title
#' 
#' @param object : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "summary",
      signature(object="Model"),
      definition=function 
      (object){
   }
)



#' automatic title
#' 
#' @param object : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "show",
      signature(object="Model"),
      definition=function
      (object){
   }
)



#' automatic title
#' 
#' @param object : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "getDecompOp",
      signature= "Model",
      definition=function(object){
      return(object@mat)
   }
)



#' automatic title
#' 
#' @param object : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "getTimes",
      signature= "Model",
      definition=function(object){
         times=matrix(ncol=1,object@times)
         colnames(times)="times"
      return(times)
   }
)



#' automatic title
#' 
#' @param object : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
    f='getRightHandSideOfODE'
    ,signature= "Model"
    ,definition= function(object){
        ns=length(object@initialValues)
        Atm=object@mat
        A=getFunctionDefinition(Atm)
        itm=object@inputFluxes
        input=getFunctionDefinition(itm)
        ydot=NpYdot(A,input)
    }
)



#' automatic title
#' 
#' @param object : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
    f= "getC"
    ,signature= "Model"
    ,definition= function(object){
        ns=length(object@initialValues)
        ydot=getRightHandSideOfODE(object)
        sVmat=matrix(object@initialValues,nrow=ns,ncol=1)
        Y=solver(object@times,ydot,sVmat,object@solverfunc) 
        #f=function(i){paste("C",i,sep="")}
        return(Y)
   }
)



#' automatic title
#' 
#' @param object : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "getReleaseFlux",
      signature= "Model",
      definition=function 
      (
      object 
      ){
      C=getC(object)
      times=object@times
      Atm=object@mat
      A=getFunctionDefinition(Atm)
      n=length(object@initialValues)
      rfunc=RespirationCoefficients(A)
      l=sapply(times,rfunc)
      if (n==1) { r=matrix(ncol=n,l)}
      else {r=t(l)}
      R=r*C
      return(R)
   }
)



#' automatic title
#' 
#' @param object : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "getAccumulatedRelease",
      signature= "Model",
      definition=function 
      (object){
      times=object@times
      R=getReleaseFlux(object)
      n=ncol(R)
      if (n==1) {
          Rfuns=list(splinefun(times,R))
      }
      else{
        Rfuns=list(splinefun(times,R[,1]))
        for (i in 2:n){
            Rf=splinefun(times,R[,i])
            Rfuns=append(Rfuns,Rf)
        }
      }
      rdot=function(y,t0){
           rv=matrix(nrow=n,ncol=1)
           for (i in 1:n){
               rv[i,1]=Rfuns[[i]](t0)
           }
           return(rv)
      }
      sVmat=matrix(0,nrow=n,ncol=1)
      Y=solver(object@times,rdot,sVmat,object@solverfunc)
      f=function(i){paste("AccumulatedRelease",i,sep="")}
      return(Y)
   }
)
getSingleCol=function(x,slot_name){
    res=""
    if(slot_name=="times"){ res=getTimes(x)}
    if(slot_name=="C"){ res=getC(x)}
    if(slot_name=="ReleaseFlux"){ res=getReleaseFlux(x)}
    if(slot_name=="AccumulatedRelease"){ res=getAccumulatedRelease(x)}
    return(res)
}



#' automatic title
#' 
#' @param x : no manual documentation
#' @param i : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod("[",signature(x="Model",i="character",j="missing",drop="missing"), 
        definition=function 
        (x,i){
            n=length(i)
            df=getSingleCol(x,i[1])
            if (n>1){
                for (k in 2:n){
                    df=cbind(df,getSingleCol(x,i[k]))
                }
            }
            return(df)
        }
)
