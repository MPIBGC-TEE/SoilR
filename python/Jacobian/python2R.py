#!/usr/bin/python3
# vim:set ff=unix expandtab ts=4 sw=4:
from sympy.printing.rcode import rcode
from sympy import Expr,MutableDenseMatrix
from string import Template
def Rcode(obj):
    if isinstance(obj,Expr):
        res=rcode(obj)
    elif isinstance(obj,MutableDenseMatrix):
        res=matrix2matrix(obj)
    else:
        res=str(obj)
    return(res)

def dict2assignments(dic):
    if type(dic)!=dict:
        raise "not a dictionary"
    res=",".join([Rcode(key)+"="+Rcode(dic[key]) for key in dic.keys()])
    print(res)
    return(res)

def matrix2matrix(m):
    nr,nc=m.shape
    T=Template("matrix(byrow=TRUE,nrow=${nr},c(${values}))")
    #text= T.substitute(nr=nr,values=",".join([ Rcode(v) for v in m.values()]))
    #does not work becuase values is empty for zeros 
    text= T.substitute(nr=nr,values=",".join([ Rcode(m[i,j]) for i in range(nr) for j in range(nc)]))
    return(text)
