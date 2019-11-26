
#' Pool Contents for all times
#' 
#' @template getC-description-common
#' @template Model-param
#' @template PoolWiseReturnMatrix
#' @autocomment 
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




#' automatic title
#' 
#' @template Model-param
#' @autocomment 
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



#' Provide the (vector valued) derivative of the stocks with respect to time  
#' 
#' This function is required by the ODE solvers.
#' @param object The model 
#' @autocomment 
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

##' Plot the graph of pool connections
##' 
##' @param x The modelrun the of connection graph of which is plotted
##' @autocomment 
#setMethod(
#   f= "plotPoolGraph",
#      signature(x="Model"),
#      definition=function 
#      (x){
#      op=getDecompOp(x)
#      iflvec=getInFluxes(x)
#
#      #call the function
#      #internalConnections<-list(tuple(1,2),tuple(2,3),tuple(3,1),tuple(3,4))
#      #inBoundConnections<-list(1,3)
#      #outBoundConnections<-list(4)
#      #plotPoolGraphFromTupleLists(internalConnections,inBoundConnections,outBoundConnections)
#      
#   }
#)
