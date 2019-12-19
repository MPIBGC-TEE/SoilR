
G=function(x,y=1,z=2){x+y+z}
G(x=3) 
# until here everything is fine
# now we use this function as a method with two implicit ANY s in the signature
setGeneric(name='G1',def=function(x,y,z){standardGeneric('G1')})
setMethod(f='G1',signature=c(x='numeric'),def=G)
G1(x=3)
