test.FluxRateConstructors=function(){
    cir1=ConstantInternalFluxRate(source=1,destination=2,rate_constant=0.3)
    cir2=ConstantInternalFluxRate(src_to_dest='1_to_2',rate_constant=0.3)
    checkEquals(cir1,cir2)
    
    cir3=ConstantInternalFluxRate(source='barrel',destination='belly',rate_constant=0.3)
    # out flux rates 
    cor1=ConstantOutFluxRate(source=1,rate_constant=0.3)
    cor2=ConstantOutFluxRate(source='belly',rate_constant=0.3)
}
