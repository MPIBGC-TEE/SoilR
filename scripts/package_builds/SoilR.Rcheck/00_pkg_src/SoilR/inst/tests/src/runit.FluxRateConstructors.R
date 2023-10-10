test.RateConstructors=function(){
    k<-0.3
    s_string=ConstantOutFluxRate_by_PoolName(sourceName='barrel',rate_constant=k)

    t_string=ConstantInFluxRate_by_PoolName(destinationName='belly',rate_constant=k)

    
    c_string=ConstantInternalFluxRate_by_PoolName(sourceName='barrel',destinationName='belly',rate_constant=k)
    checkEquals(
          s_string@sourceName
         ,c_string@sourceName
    )
    checkEquals(
          t_string@destinationName
         ,c_string@destinationName
    )
   
    # pool ids can also be numeric 
    s_num=ConstantOutFluxRate_by_PoolIndex(sourceIndex=1,rate_constant=k)
    t_num=ConstantInFluxRate_by_PoolIndex(destination=2,rate_constant=k)
    c_num=ConstantInternalFluxRate_by_PoolIndex(source=1,destination=2,rate_constant=k)
    checkEquals(
         c_num@sourceIndex
        ,s_num@sourceIndex
    )
    checkEquals(
          c_num@destinationIndex
         ,t_num@destinationIndex
    )
    # an for connections also 'strings_of_numbers'
    c_num_string=ConstantInternalFluxRate_by_PoolIndex(src_to_dest='1_to_2',rate_constant=k)
    checkEquals(
         c_num
        ,c_num_string
    )
    # or 'strings_of_strings'
    c_string_string1=ConstantInternalFluxRate_by_PoolName(src_to_dest='barrel_to_belly',rate_constant=k)
    c_string_string2=ConstantInternalFluxRate_by_PoolName(src_to_dest='barrel->belly',rate_constant=k)
    checkEquals( c_string ,c_string_string1)
    checkEquals( c_string ,c_string_string2)

    ## In the presence of a vector of pool names  they can be converted from string to  numeric 
    s_num_conv<-by_PoolIndex(s_string,poolNames=c('barrel','belly'))
    checkEquals(
         s_num
        ,s_num_conv
    )

    t_num_conv<-by_PoolIndex(t_string,poolNames=c('barrel','belly'))
    checkEquals(
         t_num
        ,t_num_conv
    )
    
    c_num_conv<-by_PoolIndex(c_string,poolNames=c('barrel','belly'))
    checkEquals(
         c_num
        ,c_num_conv
    )
    # We can also convert back from numeric to string since the rate constant
    # is not a function of the statevector (which would necessiate a conversion
    # of the function body. But for constants only the topology changes
    # and we can do it
    s_string_conf<-by_PoolName(s_num,poolNames=c('barrel','belly'))
    checkEquals(
         s_string
        ,s_string_conf
    )

    t_string_conf<-by_PoolName(t_num,poolNames=c('barrel','belly'))
    print(t_string_conf)
    checkEquals(
         t_string
        ,t_string_conf
    )
    
    c_string_conf<-by_PoolName(c_num,poolNames=c('barrel','belly'))
    checkEquals(
         c_string
        ,c_string_conf
    )
}
