f<-function(X,t){sum(X)*t}
test.PoolConnectionConstructors=function(){
    pc1=PoolConnection(source=1,destination=2)
    pc2=PoolConnection(src_to_dest='1_to_2')
    checkEquals(pc1,pc2)
    
    pc1=PoolConnection(source='barrel',destination='belly')

    #
    ps=PoolSource(source=1)
    ps=PoolSource(source='Barrel')
}
