test.ExtractionOfConstantLinDecompOpAndXi<-function(){
    LNcorn=0.17/0.004 # Values for corn clover reported in Parton et al. 1987
    xi_func=function(t){2+sin(t)}
    t_model1=seq(0,5200,1) #
    lag=1
    Ex1=CenturyModel_new(t_model1,LN=0.5,Ls=0.1,In=0.1,xi=xi_func,xi_lag=lag)
    Ct=getC(Ex1)
    Rt=getReleaseFlux(Ex1)
    # now extract the operator (all model objects can do this)
    op=getDecompOp(Ex1)
    
    # and from the operator the matrix 
    print(getConstantCompartmentalMatrix(op))
    
    # This will (intentionally) only work for certain operators that have this
    # method which are at the moment only two classes: - "ConstLinDecompOp" -
    # "ConstLinDecompOpWithLinearScaleFactor"
    #
    # Therefore it is also questionable to make this a mehtod of the Model
    # class so that you could call it directly on Ex1 (Unless we create a
    # special model class the model does not know if the method call makes
    # sense because at the moment it works with all operators


    # you can also extract the automatically created ScalarTimeMap object
    tm=getLinearScaleFactor(op)
    print(tm)
    # from which you can get a normal function as usual
    xi_func_extracted=getFunctionDefinition(tm) 
    # Note that this incorporates a possible lag (If you want to check  plot it together with the original xi_func


    # You can also extract the ConstLinDecomp object (the constant part of the operator
    clo=getConstLinDecompOp(op) #(and call the getConstantCompartmentalMatrix on this 
    # object yourself
    #From the clo object you can also extract the fluxrates 
    cintflrs_by_ind=getConstantInternalFluxRateList_by_PoolIndex(clo)
    coflrs_by_ind=getConstantOutFluxRateList_by_PoolIndex(clo)

    # you can also reindex the flux rates by PoolNames if you provide a vector
    # of poolNames where the names are almost free to choose as long as they
    # would qualify as variable names in R (which is tested) This convention is
    # relevant for the other direction from PoolName to PoolIndex since for
    # nonlinear models this includes the translation of scalar function arguments
    # to vector arguments
    poolNames=c('a','b','c','d','e')
    cintflrs_by_name=by_PoolName(cintflrs_by_ind,poolNames)
    coflrs_by_name=by_PoolName(coflrs_by_ind,poolNames)

    
    # The influxes are not part of the compartmental operator but we can get
    # them back from the model object.  
    # In the case of constant influxes this is pretty easy even if they had
    # been originally given as vectors but for all nonautonomous cases  our
    # traditional vector valued functions are unfortunate as it is more or less
    # impossible to know if some components of a vector valued function are
    # zero for all times and SoilR would have to assume the most general case
    # that all components are non zero....
    # To achieve structural transparency it is much better to have a 'vector
    # of functions', which is the reason I changed the way CenturyModel_new
    # defines the influxes. This will scale also to nonautonomous fluxes. It
    # will reach its limits though for nonlinear fluxes as it turns out that
    # for these the only way that preserves the full structural information is
    # to use scalar functions that do not even have vector arguments.  I have
    # not implemented this yet in 'Century_new' because I did not know how the
    # pools are called in Century, but it will be shown in Corrados example.
    ifls=getInFluxes(Ex1)
    cifls_by_ind<-ConstantInFluxList_by_PoolIndex(ifls)

    # now we can finally plot the connections This is very much ad hoc still
    # but it shows how it has to be done In a first step it will finally be a
    # method of model but this requires the model object to be ablte to
    # transform its influxes and compartmental operator to flux lists with the
    # challanges mentioned above
    #internalConnections<-list(tuple(1,2),tuple(2,3),tuple(3,1),tuple(3,4))
    #inBoundConnections<-list(1,3)
    #outBoundConnections<-list(4)
    internalConnections<-lapply(
        cintflrs_by_ind
        ,function(flr){
            tuple(
                as.integer(flr@sourceIndex)
                ,as.integer(flr@destinationIndex)
            )
        }
    )
    outBoundConnections<- lapply(
        coflrs_by_ind
        ,function(fl){ as.integer(fl@sourceIndex) }
    )
    inBoundConnections<-lapply(
        cifls_by_ind
        ,function(fl){ as.integer(fl@destinationIndex) }
    )

    plotPoolGraphFromTupleLists(internalConnections,inBoundConnections,outBoundConnections)
#
#    #now we try to plot the pool graph
#    plotPoolGraph(Ex1)
#
#
#    ## second example with data
#    #t_data=100:4000
#    #xi_df=xi=data.frame(t=t_data,xi=xi_func(t_data))
#    #t_model2=seq(101,4001,1) # In the time range of t_data considering the lag  
#    #Ex2=CenturyModel_new(t_model2,LN=0.5,Ls=0.1,In=0.1,xi=xi_df,lag=lag)
#    #Ct=getC(Ex2)
#    #Rt=getReleaseFlux(Ex2)
#
}
