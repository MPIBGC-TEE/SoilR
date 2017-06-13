#!/usr/bin/env python3
from Rexample import *
#from C14example import *
from C14exampleFromDelta14C import *
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
Rexample(\
        "CarlosExample",
        Matrix(2,2,
            [
                -Rational(1,20),             0, 
                             0,-Rational(1,30)
            ]
        ),
        [3,2],TwoPoolZeroInputRate
    ).write2file()
Rexample(\
        "TwopParallel_ZeroInput",
        Matrix(2,2,
            [
                -Rational(1,20),             0, 
                             0,-Rational(1,30)
            ]
        ),
        [3,2],TwoPoolZeroInputRate
    ).write2file()
Rexample(\
        "TwopParallel_ZeroDecayInputOnly",
        Matrix(2,2,
            [
                0, 0, 
                0, 0
            ]
        ),
        [3,2],TwoPoolConstantInputRate
    ).write2file()
Rexample(\
        "TwopParallel_constantInput",
        Matrix(2,2,
            [
                -Rational(1,20),             0, 
                             0,-Rational(1,30)
            ]
        ),
        [3,2],TwoPoolConstantInputRate
    ).write2file()
Rexample(\
        "ThreepFeedback_1",
        Matrix(3,3,
            [
                -Rational(1,2), Rational(1,3),   0,
                 Rational(1,2),-Rational(2,3),   0,  
                             0, Rational(1,3),  -1   
            ]
        ),
        [3,2,2.5],
        ThreePoolConstantInputRate
    ).write2file()
Rexample(\
        "ThreepSerial_1",
        Matrix(3,3,
            [
                -Rational(1,2),             0,   0,
                 Rational(1,2),-Rational(1,3),   0,  
                             0, Rational(1,6),  -1   
            ]
        ),
        [3,2,1],
        ThreePoolConstantInputRate
    ).write2file()
Rexample(\
        "ThreepSerial_2",
        Matrix(3,3,
            [
                -1, 0, 0,
                 1,-2, 0,  
                 0, 1,-2   
            ]
        ),
        [3,2,1],
        ThreePoolConstantInputRate
    ).write2file()
#fixme:mm
# The analytical solution yields nan for t=0
# which leads to trouble afterwards in R plotting
#Rexample(\
#        "FourpSerial_1",
#        Matrix(4,4,
#            [
#               -1, 0, 0, 0,
#                1,-2, 0, 0,  
#                0, 1,-2, 1,   
#                0, 1, 1,-1   
#            ]
#        ),
#        [3,2,1,0],
#        FourPoolConstantInputRate
#    ).write2file()
C14example(\
        "OnePool_C14_ZeroDecay_Zero",
        Matrix(1,1,
            [
                0           #Decay matrix
            ]
        ),
        [1],                #c0 =initial c value >>>not<<< c14
        [1],                #f0 =initial f values >>>not<<< f14atm
        Matrix(1,1,[0]),    #Inputrate
        1.0/2 #c14fraction
    ).write2file()
C14example(\
        "OnePool_C14_ZeroDecay_constantInputrate",
        Matrix(1,1,
            [
                0           #Decay matrix
            ]
        ),
        [1],                #c0 =initial c value >>>not<<< c14
        [1],                #f0 =initial f values >>>not<<< f14atm
        Matrix(1,1,[1]),    #constant Inputrate
        1.0/2 #c14fraction
    ).write2file()
th=5730
k=log(Rational(1,2))/th
C14example(\
        "OnePool_C14_equalDecay_ZeroInput",
        Matrix(1,1,
            [
                k           #Decay constant equal to the c14 radiaaktive decay
            ]
        ),
        [1],                #c0 =initial c value >>>not<<< c14
        [1],                #f0 =initial f values >>>not<<< f14atm
        Matrix(1,1,[0]),    #Inputrate
        1.0/2 #c14fraction
    ).write2file()
C14example(\
        "TwoPool_C14_equalDecay_ZeroInput",
        Matrix(2,2,
            [
                k ,0,           #Decay constant equal to the c14 radiaaktive decay
                0 ,k
            ]
        ),
        [1,2],                #c0 =initial c value >>>not<<< c14
        [1,2],                #f0 =initial f values >>>not<<< f14atm
        Matrix(2,1,[0,0]),    #Inputrate
        1.0/2 #c14fraction
    ).write2file()

C14exampleFromDelta14C(\
        "OnePool_C14_ZeroDecay_Zero",
        Matrix(1,1,
            [
                0           #Decay matrix
            ]
        ),
        [1],                #c0 =initial c value >>>not<<< c14
        [1],                #f0 =initial f value >>>not<<< f14atm
        Matrix(1,1,[0]),    #Inputrate
        1.0/2 #c14fraction
    ).write2file()
C14exampleFromDelta14C(\
        "OnePool_C14_ZeroDecay_constantInputrate",
        Matrix(1,1,
            [
                0           #Decay matrix
            ]
        ),
        [1],                #c0 =initial c value >>>not<<< c14
        [1],                #f0 =initial f value >>>not<<< f14atm
        Matrix(1,1,[1]),    #constant Inputrate
        1.0/2               #c14fraction
    ).write2file()
th=5730
k=log(Rational(1,2))/th
C14exampleFromDelta14C(\
        "OnePool_C14_equalDecay_ZeroInput",
        Matrix(1,1,
            [
                k           #Decay constant equal to the c14 radiaaktive decay
            ]
        ),
        [1],                #c0 =initial c value >>>not<<< c14
        [1],                #f0 =initial f values >>>not<<< f14atm
        Matrix(1,1,[0]),    #Inputrate
        1.0/2 #c14fraction
    ).write2file()
C14exampleFromDelta14C(\
        "TwoPool_C14_equalDecay_ZeroInput",
        Matrix(2,2,
            [
                k ,0,           #Decay constant equal to the c14 radiaaktive decay
                0 ,k
            ]
        ),
        [1,2],                #c0 =initial c value  >>>not<<< c14
        [1,2],                #f0 =initial f values >>>not<<< f14atm
        Matrix(2,1,[0,0]),    #Inputrate
        1.0/2 #c14fraction
    ).write2file()

#################################################################################
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
# unfortunately the analytical solution yields na for t=0 which leads to subsequent problems with R plotting
#Manzoniexample(\
#        "TwopFeedback",
#        Matrix(2,2,
#            [
#                     -k1, f*k2, 
#                (1-r)*k1,  -k2
#            ]
#        ),
#        Matrix(2,1,[c1,c2]),
#        meanTransitTime,
#        subslist
#    ).write2file()
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
