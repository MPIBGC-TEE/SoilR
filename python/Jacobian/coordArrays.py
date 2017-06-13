#!/usr/bin/python3
# vim: set expandtab ts=4
from numpy import linspace 
def griddict(vectors):
    grid=vectors[0]
    for vector in vectors[1:]:
        grid=tensorProduct(grid,vector)
    return(grid)
def tensorProduct(grid,vector):
    newgrid=dict()
    for gk,gv in grid.items(): 
        for vk,vv in vector.items(): 
            #all + add tupels 
            newgrid[gk+vk]=grid[gk]+vector[vk]
    return(newgrid)

x1min=0        
x1max=1        
x2min=0        
x2max=2        
x3min=0        
x3max=3        
ranges=[[x1min,x1max],[x2min,x2max],[x3min,x3max]]
vectors=[{(i,):((l[1]-l[0])/5.0*i,) for i in range(5)} for l in ranges] 
#print(griddict(vectors))   
#alternative without indices
def tuplelist(veclist):
    tp=[(val,) for val in veclist[0]]
    for vec in veclist[1:]:
        tp=[ltup+(val,) for ltup in tp for val in vec]
    return(tp)    
vectors=[ linspace(l[1],l[0],5) for l in ranges] 
#print(tuplelist(vectors))
