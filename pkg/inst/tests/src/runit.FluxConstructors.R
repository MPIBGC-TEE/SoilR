#fixme mm:
#Convert to new form
#test.FluxConstructors=function(){
#f<-function(X,t){sum(X)*t}
#    s_string=OutFlux(source='barrel',map=f)
#
#    t_string=InFlux(destination='belly',map=f)
#
#    
#    c_string=InternalFlux(source='barrel',destination='belly',map=f)
#    checkEquals(
#          s_string@sourceId
#         ,c_string@sourceId
#    )
#    checkEquals(
#          t_string@destinationId
#         ,c_string@destinationId
#    )
#   
#    # pool ids can also be numeric 
#    s_num=OutFlux(source=1,map=f)
#    t_num=InFlux(destination=2,map=f)
#    c_num=InternalFlux(source=1,destination=2,map=f)
#    checkEquals(
#         c_num@sourceId
#        ,s_num@sourceId
#    )
#    checkEquals(
#          c_num@destinationId
#         ,t_num@destinationId
#    )
#    # an for connections also 'strings_of_numumbers'
#    c_num_string=InternalFlux(src_to_dest='1_to_2',map=f)
#    checkEquals(
#         c_num
#        ,c_num_string
#    )
#
#    # In the presence of a vector of pool names  they can be converted from string to  numeric 
#    s_num_conv<-by_PoolIndex(s_string,poolNames=c('barrel','belly'))
#    checkEquals(
#         s_num
#        ,s_num_conv
#    )
#
#    t_num_conv<-by_PoolIndex(t_string,poolNames=c('barrel','belly'))
#    checkEquals(
#         t_num
#        ,t_num_conv
#    )
#    
#    c_num_conv<-by_PoolIndex(c_string,poolNames=c('barrel','belly'))
#    checkEquals(
#         c_num
#        ,c_num_conv
#    )
#    # when no conversion is necessary the call does no harm
#    s_num_num<-by_PoolIndex(s_num,poolNames=c('barrel','belly'))
#    checkEquals(
#         s_num_num
#        ,s_num
#    )
#    t_num_num<-by_PoolIndex(t_num,poolNames=c('barrel','belly'))
#    checkEquals(
#         t_num_num
#        ,t_num
#    )
#    c_num_num<-by_PoolIndex(c_num,poolNames=c('barrel','belly'))
#    checkEquals(
#         c_num_num
#        ,c_num
#    )
#    # We can also convert back from numeric to string  
#    s_string_conf<-by_PoolName(s_num,poolNames=c('barrel','belly'))
#    checkEquals(
#         s_string
#        ,s_string_conf
#    )
#
#    t_string_conf<-by_PoolName(t_num,poolNames=c('barrel','belly'))
#    print(t_string_conf)
#    checkEquals(
#         t_string
#        ,t_string_conf
#    )
#    
#    c_string_conf<-by_PoolName(c_num,poolNames=c('barrel','belly'))
#    checkEquals(
#         c_string
#        ,c_string_conf
#    )
#    # when no conversion is necessary the call does no harm
#    s_string_string<-by_PoolName(s_string,poolNames=c('barrel','belly'))
#    checkEquals(
#         s_string_string
#        ,s_string
#    )
#
#    t_string_string<-by_PoolName(t_string,poolNames=c('barrel','belly'))
#    checkEquals(
#         t_string_string
#        ,t_string
#    )
#
#    c_string_string<-by_PoolName(c_string,poolNames=c('barrel','belly'))
#    checkEquals(
#         c_string_string
#        ,c_string
#    )
#}
