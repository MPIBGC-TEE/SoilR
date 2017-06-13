#!/usr/bin/python
# vim: set expandtab ts=4
import numpy as np
import sympy as sp
def evalOnGrid(X,Y,expr,symbols):
    x,y=symbols
    fpair=lambda xv,yv: expr.subs({x:xv,y:yv}).evalf()
    frows=lambda xrow,yrow:map(fpair,xrow,yrow)
    Z = np.array(map(frows,X,Y),dtype=float)
    return(Z)
def evalOnVecList(X,expr,symbolicVec):
    #symbolicVec is supposed to be a vector (matrix(n,1))
    #X is supposed to be a list of vectors where the positions of values match the positions in the sympolicVec
    Z = [expr.subs({symbolicVec[i]:xv[i] for i in range(symbolicVec.rows)}).evalf() for xv in X]
    return(Z)
    
