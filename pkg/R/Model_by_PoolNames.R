
setMethod(
  f= "Model_by_PoolNames"
  ,signature= c(
       mod='missing'
      ,times="numeric"
      ,mat="UnBoundNonLinDecompOp_by_PoolNames"
      ,initialValues="numeric"
      ,inputFluxes="InFluxList_by_PoolName"
      #,solverFunc="function"# =ANY
      ,timeSymbol='character'
   )
  ,definition= function(
        times
        ,mat
        ,initialValues
        ,inputFluxes
        ,solverFunc=deSolve.lsoda.wrapper		
        ,timeSymbol='t'
  ){
      new(
        Class='Model_by_PoolNames'
        ,times=times
        ,mat=mat
        ,initialValues=initialValues
        ,inputFluxes=inputFluxes
        ,solverFunc=solverFunc
        ,timeSymbol=timeSymbol
  
      )
   }
 )

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


#' All Fluxes and stocks for all times
#' 
#' @template Model-param
#' @template PoolWiseReturnMatrix
#' @autocomment 
setMethod(
    f= "getSolution"
    ,signature= "Model_by_PoolNames"
    ,definition=
        function(object,params){
            obn <- object@mat
            intfs <- obn@internal_fluxes
            ofs <- obn@out_fluxes
            
            ivp <- IVP_maker( 
              in_fluxes=object@inputFluxes,
	            internal_fluxes=intfs,
	            out_fluxes=ofs,
	            time_symbol=object@timeSymbol,
	            startValues=object@initialValues
            )
            #sVmat=matrix(object@initialValues,nrow=length(object@initialValues),ncol=1)
            ks<-c()
            sol <- deSolve::lsoda(
              y=ivp$startValues,
              times=object@times,
              func=ivp$ydot,
              #parms=ks
            )
            #sol <- deSolve::lsoda(y=ivp$startValues,times=times,func=ivp$ydot,parms=ks)

            return(sol)
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
                # we could compute the righthandside from 
                # the combination of InternalFluxes and
                # OutFluxes directly (as opposed to multiplication with the 
                # Compartmental matrix
                A=AFunc(Y,t)
                I=inputFunc(Y,t)
                res=A%*%Y+I
            }
            return(DotY)
        }
)

#' Plot the graph of pool connections
#' 
#' @param x The modelrun the connection graph of which is plotted
#' @autocomment 
setMethod(
   f= "plotPoolGraph",
      signature(x="Model_by_PoolNames"),
      definition=function 
      (x){
        obn <- x@mat
        intfs <- obn@internal_fluxes
        ofs <- obn@out_fluxes
        
        internalConnections<-lapply(
          intfs, 
          function(flr){
            tuple(
                as.character(flr@sourceName)
                ,as.character(flr@destinationName)
            )
          }
        )
        
        inBoundConnections<-lapply(
            x@inputFluxes,
            function(fl){ as.character(fl@destinationName) }
        )

        outBoundConnections<- lapply(
            ofs,
            function(fl){ as.character(fl@sourceName) }
        )
        plotPoolGraphFromTupleLists(internalConnections,inBoundConnections,outBoundConnections)
      
   }
)
#' Create an overview plot 
#' 
#' The method solves the model and plots the solutions
#' It is intended to provide a quick overview.
#' @param x The model (run) the results of which are plotted
#' @autocomment 
setMethod(
   f= "plot",
   signature(x="Model_by_PoolNames"),
   definition=function (x){
     plot(getTimes(x),getC(x)[,1])
   }
)
#' Extract the times vector
#' 
#' Since the \code{times} had to be provided to create the model this method
#' yields no new information. 
#' It is usually called internally by other functions that deal with models. 
#' @template Model-param
#' @autocomment 
#fixme 12/17/2019: should be defined only once and inherited  by the other modelrun classes
setMethod(
   f= "getTimes",
      signature= "Model_by_PoolNames",
      definition=function(object){
         times=matrix(ncol=1,object@times)
         colnames(times)="times"
      return(times)
   }
)
