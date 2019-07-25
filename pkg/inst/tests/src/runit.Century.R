test.ExtractionOfConstantLinDecompOpAndXi<-function(){
    LNcorn=0.17/0.004 # Values for corn clover reported in Parton et al. 1987
    xi_func=function(t){2+sin(t)}
    t_model1=seq(0,5200,1) #
    lag=1
    Ex1=CenturyModel(t_model1,LN=0.5,Ls=0.1,In=0.1,xi=xi_func,lag=lag)
    Ct=getC(Ex1)
    Rt=getReleaseFlux(Ex1)
    # now extract the operator (all model objects can do this)
    op=getDecompOp(Ex1)
    
    # and from the operator the matrix 
    print(getConstantCompartmentalMatrix(op))
    
    # This will (intentionally) only work
    # for certain operators that have this method
    # which are at the moment only two classes:
    # - "ConstLinDecompOp"
    # - "ConstLinDecompOpWithLinearScaleFactor"
    #
    # Therefore it is also questionable to make this a mehtod 
    # of the Model class so that you could call it directly on Ex1
    # (Unless we create a special model class the model does not know 
    # if the method call makes sense because it works 
    # with all operators

    # you can also extract the automatically created ScalarTimeMap object
    tm=getLinearScaleFactor(op)
    print(tm)
    # from which you can get a normal function as usual
    xi_func_extracted=getFunctionDefinition(tm) 
    # Note that this incorporates the lag (you could plot it together with the original xi_func


    # You can also extract the ConstLinDecomp object (the constant part of the operator
    clo=getConstLinDecompOp(op) #(and call the getConstantCompartmentalMatrix on this 
    # object yourself
    #From the clo object you can also extract the fluxrates and (hopefully soon plot the compartmental graph)
    print(ConstantOutFluxRateList_by_PoolIndex(clo))
    print(ConstantInternalFluxRateList_by_PoolIndex(clo))



    # second example with data
    t_data=100:4000
    xi_df=xi=data.frame(t=t_data,xi=xi_func(t_data))
    t_model2=seq(101,4001,1) # In the time range of t_data considering the lag  
    Ex2=CenturyModel(t_model2,LN=0.5,Ls=0.1,In=0.1,xi=xi_df,lag=lag)
    Ct=getC(Ex2)
    Rt=getReleaseFlux(Ex2)

}
