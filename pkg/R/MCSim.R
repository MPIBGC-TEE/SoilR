setClass(
   Class="MCSim",
   representation=representation(
	 model="NlModel",
   tasklist="list"
   )
)



#' Automatic description: initialize,MCSim-method
#' 
#' @name initialize,MCSim-method
#' @param .Object : object of class:\code{MCSim}, no manual documentation
#' @param model : no manual documentation
#' @param tasklist : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
    f="initialize",
    signature="MCSim",
    definition=function(.Object,model=new(Class="NlModel"),tasklist=list()){
    .Object@model=model
    .Object@tasklist=tasklist
    return(.Object)
    }
)



#' Automatic description: plot,MCSim-method
#' 
#' @name plot,MCSim-method
#' @param x : object of class:\code{MCSim}, no manual documentation
#' @param y : no manual documentation
#' @param ... : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
    f="plot",
    signature="MCSim",
      definition=function
     (x, 
      ...
      ){
      nop=getNumberOfPools(x)
      aRS=availableResidentSets(x)
      tasklist <-x[["tasklist"]]
      for (rs in aRS){
        tasklist[[paste("number of",rs)]] <- quote(nrow(particleSets[["rs"]]))
      }
      resnames=names(x@tasklist)
      l=computeResults(x)
      for (name in resnames){
        plot(
        type="l",
        l[["cr"]][,"time"],
        l[["cr"]][,name],
        xlab="time",
        ylab=name
        )
      }
    }
)



#' Automatic description: availableResidentSets,MCSim-method
#' 
#' @name availableResidentSets,MCSim-method
#' @param object : object of class:\code{MCSim}, no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
    f="availableResidentSets",
    signature="MCSim",
    definition=function(object){
    n=getNumberOfPools(object)
    res=c(
      mapply(stockKey,1:n),
      "particles_in_the_system"
      )
    return(res)
    }
)



#' Automatic description: availableParticleSets,MCSim-method
#' 
#' @name availableParticleSets,MCSim-method
#' @param object : object of class:\code{MCSim}, no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
    f="availableParticleSets",
    signature="MCSim",
    definition=function(object){
    n=getNumberOfPools(object)
    res=c(
      mapply(stockKey,1:n),
      mapply(leaveKey,1:n),
      "particles_leaving_the_system"
      )
    return(res)
    }
)



#' Automatic description: availableParticleProperties,MCSim-method
#' 
#' @name availableParticleProperties,MCSim-method
#' @param object : object of class:\code{MCSim}, no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
    f="availableParticleProperties",
    signature="MCSim",
    definition=function(object){
    n=getNumberOfPools(object)
    f=function(i){paste("t_entryPool_",i,sep="")}
    res=c("t_entrySystem",mapply(f,1:n),"t_exitSystem")
    return(res)
    }
)



#' Automatic description: [[,MCSim-method
#' 
#' @name [[,MCSim-method
#' @param x : object of class:\code{MCSim}, no manual documentation
#' @param i : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod("[[",
    signature(x = "MCSim"),
    definition=function 
    (
      x, 
      i  
    ) 
    {
      if (i=="tasklist"){
        value=x@tasklist
        return(value)
      }else{
        stop(paste("MCSim has no property  ",i,".",sep=""))
      } 
    }
)



#' Automatic description: [[<-,MCSim-method
#' 
#' @name [[<-,MCSim-method
#' @param x : object of class:\code{MCSim}, no manual documentation
#' @param i : no manual documentation
#' @param j : no manual documentation
#' @param ... : no manual documentation
#' @param value : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod("[[<-",
    signature(x = "MCSim"),
    definition=function 
    (
      x,  
      i,  
      j,  
      ..., 
      value
    ) 
    {
      if (i=="tasklist"){
        x@tasklist=value
        return(x)
      }else{
        stop(paste("MCSim has no property  ",i,".",sep=""))
      } 
    }
)



#' Automatic description: getNumberOfPools,MCSim-method
#' 
#' @name getNumberOfPools,MCSim-method
#' @param object : object of class:\code{MCSim}, no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "getNumberOfPools",
      signature(object="MCSim"),
      definition=function(object){
      return(getNumberOfPools(object@model))
   }
)



#' Automatic description: computeResults,MCSim-method
#' 
#' @name computeResults,MCSim-method
#' @param object : object of class:\code{MCSim}, no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
    f="computeResults",
    signature="MCSim",
    definition=function(
    object 
    ){
            tcn="time"				
            extractColumn=function(df,colname){df[,colname]}
            reduce2singledf=function(l){
            	colavg=function(colname){
            		mat=mcmapply(extractColumn,l,MoreArgs=list(colname))
            		rs=function(i){mean(mat[i,])}
            		newcol=mcmapply(rs,1:nrow(mat))
            		return(newcol)
            	}
            	colnames=setdiff(names(l[[1]]),tcn)
            	res=cbind(l[[1]][,tcn],mapply(colavg,colnames))
            	res=as.data.frame(res)
            	names(res)<-names(l[[1]])
            	return(res)
            }
            singleThreadParticleSimulator=function(pseudoarg){
              tasklist=object@tasklist
              resnames=names(tasklist)
              force(resnames)
            	resultline=function(t,resultlist){
            		tf=data.frame(t)
                names(tf) <- "time"
                rd=cbind(tf,as.data.frame(resultlist))
            		return(rd)
            	}
              avp=availableParticleProperties(object)
            	enteringSet=function(t,n,ip){
                npd<- as.data.frame(matrix(ncol=length(avp),nrow=n,dimnames=list(c(),avp)))
                npd[,"t_entrySystem"]=rep(t,n)
                npd[,paste("t_entryPool_",ip,sep="")]=rep(t,n)
            		return(npd)
            	}
              mod=object@model
              op=getDecompOp(mod)
              times=getTimes(mod)
            	nt=length(times)			
            	st=times[[1]]					
              iv=getInitialValues(mod)
              Cs =getC(mod,as.closures=TRUE)
              Os =getOutputFluxes(mod,as.closures=TRUE)
              Tr=getTransferCoefficients(mod,as.closures=TRUE)
              ntr=names(Tr)
              vI=getFunctionDefinition(getInFluxes(mod))
              nop=getNumberOfPools(object)
              particleSets=list()
              zeroFrame <- as.data.frame(matrix(ncol=length(avp),nrow=0,dimnames=list(c(),avp)))
              for (ip in 1:nop){
                particleSets[[stockKey(ip)]] <- zeroFrame
                particleSets[[leaveKey(ip)]] <- zeroFrame
              }
              outKey="particles_leaving_the_system"
              for (ip in 1:nop){
                particleSets[[stockKey(ip)]] <- rbind(
                  particleSets[[stockKey(ip)]],
                  enteringSet(st,iv[[ip]],ip)
                )
              }
            	results=as.data.frame(matrix(ncol=length(tasklist)+1,nrow=0,dimnames=list(c(),c("time",names(tasklist)))))
            	t_old=st
            	for (it in 2:nt){
                t=times[[it]]
                sts=t-t_old				
                particleSets[[outKey]] <- zeroFrame
                for ( i in 1:nop){
                  ii=function(t){return(vI(t)[i,])}
                  np_new=integrate(ii,lower=t,upper=t+sts)$val
            		  particleSets[[stockKey(i)]] <- rbind(
                    particleSets[[stockKey(i)]],
                    enteringSet(t,np_new,i)
                  ) 
                  o_i <- Os[[i]]
                  c_i <-Cs[[i]] 
                  delta_oi <-integrate(o_i,lower=t,upper=t+sts)$val
            		  p_leave <- delta_oi/c_i(t+sts) 
                  np=nrow(particleSets[[stockKey(i)]])
            		  r <- runif(np,0,1.0)	
            		  llp1=r<p_leave			
                  particleSets[[leaveKey(i)]] <- as.data.frame(particleSets[[stockKey(i)]][llp1,])
                  ls=!llp1	      		
                  particleSets[[stockKey(i)]] <- as.data.frame(particleSets[[stockKey(i)]][ls,])
                  recs=getOutputReceivers(op,i)
                  for (j in recs){
                    t_ij <- Tr[[key(i,j)]]
                    o_ij <- function(time){
                      return(o_i(time)*t_ij(time))
                    } 
                    p_inject <- integrate(o_ij,lower=t,upper=t+sts)$val/delta_oi
                    targetKey=stockKey(j)
                    Source <- particleSets[[leaveKey(i)]]
                    Destination <- particleSets[[targetKey]]
                    np=nrow(Source)
                    r <- runif(np,0,1.0)	
                    llp1=r<p_inject 
                    particleSets[[targetKey]]=rbind(
                      particleSets[[targetKey]],
                      as.data.frame(particleSets[[leaveKey(i)]][llp1,])
                    )
                    ls=!llp1			
                    particleSets[[leaveKey(i)]] <- as.data.frame(particleSets[[leaveKey(i)]][ls,])
                  }
                  particleSets[[outKey]] <-  rbind(
                    particleSets[[outKey]],
                    particleSets[[leaveKey(i)]]
                  )
                }
                if (nrow(particleSets[[outKey]]) >0){
                  particleSets[[outKey]][,"t_exitSystem"]=t
                }
                resultlist=list()
                for (name in names(tasklist)){
                  resultlist[[name]] <- eval(tasklist[[name]])
                }
            		results=rbind(results,resultline(t,resultlist))
                t_old <- t
            	}	
            	return(results)
            }
      simParams=list(t)
            numberOfProcessors=detectCores()
            dfl=mclapply(rep(simParams,numberOfProcessors),singleThreadParticleSimulator,mc.cores=numberOfProcessors)
            cr=reduce2singledf(dfl)
      return(list(tcn=tcn,cr=cr))
  }
)
