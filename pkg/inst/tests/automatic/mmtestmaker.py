#!/usr/bin/python
from C14example import *
from Manzoniexample import *
#########################################################################
#########################################################################
#########################################################################
##########################main part with example matrices ##########################
TwoPoolConstantInputRate=Matrix(2,1,[0.1,0.2])
TwoPoolZeroInputRate=Matrix(2,1,[0,0])
OnePoolZeroInputRate=Matrix(1,1,[0])
ThreePoolConstantInputRate=Matrix(3,1,[1,2,3])
FourPoolConstantInputRate=Matrix(4,1,[1,2,3,4])

# #we follow the manzoni nomenclature here
# r=Rational(1,4) 
# #r is the released fraction of the output of pool1 for r=1 all C is released
# # and the second pool recieves nothing
# # if r=0 all the output of the first pool is without loss transferred to the
# # second pool
# # this means that pool one receives all the input
# k1=Rational(1,20)
# #the decay constants are considered positive in the Manzoni paper
# Manzoniexample(\
#         "op",
#         Matrix(1,1,
#             [
#                       -k1
#             ]
#         ),
#         [1]
#     ).write2file()
# ################################################################################
# #we follow the manzoni nomenclature here
# r=Rational(1,4) 
# #r is the released fraction of the output of pool1 for r=1 all C is released
# # and the second pool recieves nothing
# # if r=0 all the output of the first pool is without loss transferred to the
# # second pool
# inputfractions=[1,0]
# # this means that pool one receives all the input
# k1=Rational(1,20)
# k2=Rational(1,30)
# #the decay constants are considered positive in the Manzoni paper
# Manzoniexample(\
#         "TwopSeriell",
#         Matrix(2,2,
#             [
#                       -k1,  0, 
#                  (1-r)*k1,-k2
#             ]
#         ),
#         [1,0]
#     ).write2file()
# ################################################################################
# #we follow the manzoni nomenclature here
# alpha=Rational(1,3)
# # alpha is the fraction of the inputrate that goes to the first pool
# k1=Rational(1,20)
# k2=Rational(1,30)
# #the decay constants are considered positive in the Manzoni paper
# Manzoniexample(\
#         "TwopParallel",
#         Matrix(2,2,
#             [
#               -k1,  0,              
#               0  ,-k2
#             ]
#         ),
#         [alpha,1-alpha]
#     ).write2file()
################################################################################
#we follow the manzoni nomenclature here
#r is the released fraction of the output of pool1 for r=1 all C is released
# and the second pool recieves nothing
# if r=0 all the output of the first pool is without loss transferred to the
# second pool
# f is the fraction of the material leaving the second pool that is fed back to 
# the first one . In the Manzoni paper it is always set to one 
# this means that pool one receives all the input
c1,c2,k1,k2,r,f=symbols("c1,c2,k1,k2,r,f")
subslist=[
        (c1,1),
        (c2,0),
        (r,Rational(1,4)),
        (k1,Rational(1,10)),
        (k2,Rational(1,5)),
        (f,1)]
meanTransitTime=((1-r)*k1+k2)/(r*k1*k2)
Manzoniexample(\
        "TwopFeedback",
        Matrix(2,2,
            [
                     -k1, f*k2, 
                (1-r)*k1,  -k2
            ]
        ),
        Matrix(2,1,[c1,c2]),
        meanTransitTime,
        subslist
    ).write2file()
################################################################################
c1,k1=symbols("c1,k1")
subslist=[
        (c1,1),
        (k1,Rational(1,10))
        ]
meanTransitTime=1/k1
Manzoniexample(\
        "op",
        Matrix(1,1,
            [
                     -k1
            ]
        ),
        Matrix(1,1,[c1]),
        meanTransitTime,
        subslist
    ).write2file()
################################################################################
