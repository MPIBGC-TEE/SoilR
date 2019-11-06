


#' Automatic description: getC,Model_by_PoolNames-method
#' 
#' @name getC,Model_by_PoolNames-method
#' @param object : object of class:\code{Model_by_PoolNames}, no manual
#' documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
    f= "getC"
    ,signature= "Model_by_PoolNames"
    ,definition=
        function(object){
            DotY=getRightHandSideOfODE(object)
            sVmat=matrix(object@initialValues,nrow=length(object@initialValues),ncol=1)
            Y=solver(
                times=object@times
                ,ydot=DotY
                ,startValues=sVmat
                ,solverFunc=object@solverfunc
            ) 
            #ydot=NpYdot(A,input)
            #sVmat=matrix(object@initialValues,nrow=ns,ncol=1)
            #Y=solver(object@times,ydot,sVmat,object@solverfunc) 
            #f=function(i){paste("C",i,sep="")}
            return(Y)
        }
    )




#' Automatic description: getReleaseFlux,Model_by_PoolNames-method
#' 
#' @name getReleaseFlux,Model_by_PoolNames-method
#' @param object : object of class:\code{Model_by_PoolNames}, no manual
#' documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "getReleaseFlux",
      signature= "Model_by_PoolNames",
      definition=function 
      (
      object 
      ){
      C=getC(object)
      times=object@times
      initialValues=object@initialValues
      timeSymbol=object@timeSymbol
      state_variable_names=names(initialValues)
      Atm=object@mat
      timeSymbol=object@timeSymbol
      BFunc=getCompartmentalMatrixFunc( Atm ,timeSymbol ,state_variable_names)
      n=length(initialValues)
      testvec=matrix(nrow=1,ncol=n,1)
      rfunc= function(sol,t){
          -testvec%*%BFunc(sol,t)
      }
      t_inds=1:length(times)
      l=sapply(t_inds,function(i){ rfunc(C[i,],times[[i]])})
      if (n==1) { r=matrix(ncol=n,l)}
      else {r=t(l)}
      R=r*C
      return(R)
   }
)



#' Automatic description: getRightHandSideOfODE,Model_by_PoolNames-method
#' 
#' @name getRightHandSideOfODE,Model_by_PoolNames-method
#' @param object : object of class:\code{Model_by_PoolNames}, no manual
#' documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
    f='getRightHandSideOfODE'
    ,signature= "Model_by_PoolNames"
    ,definition= function(object){
            initialValues=object@initialValues
            timeSymbol=object@timeSymbol
            state_variable_names=names(initialValues)
            ns=length(initialValues)
            Atm=object@mat
            AFunc=getCompartmentalMatrixFunc(Atm,timeSymbol,state_variable_names)
            itm=object@inputFluxes
            inputFunc=getFunctionDefinition(itm,timeSymbol,state_variable_names)

            DotY=function(Y,t){
                # fixme mm 07-31:
                # we could computed the combination of InternalFluxes and
                # OutFluxes directly (as opposed to multiplication with the 
                # Compartmental matrix
                A=AFunc(Y,t)
                I=inputFunc(Y,t)
                res=A%*%Y+I
            }
            return(DotY)
        }
)
