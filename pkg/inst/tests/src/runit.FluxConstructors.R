f<-function(X,t){sum(X)*t}
test.FluxConstructors=function(){
    # internal fluxes
    fl1=InternalFlux(source=1,destination=2,map=f)
    fl2=InternalFlux(src_to_dest='1_to_2',map=f)
    checkEquals(fl1,fl2)
    
    # with pool names
    fl1=InternalFlux(source='barrel',destination='glass',map=f)
    
    # out fluxes
    fl=OutFlux(source=1,map=f)
}
