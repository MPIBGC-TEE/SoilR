setClass(
   Class="PoolSource",
   slots=c(sourceId='PoolId')
)
PoolSource=function(source){
    new('PoolSource',sourceId=PoolId(id=source))
}
