#!/usr/bin/python3
# vim:set ff=unix expandtab ts=4 sw=4:
import sys
from sympy import symbols, Symbol, Matrix
from sympy.functions import sin, cos, Abs
from sympy.matrices import MatrixBase 
from sympy.abc import x
import random
import unittest
import numpy as np
from concurrencytest import ConcurrentTestSuite, fork_for_tests
from RExample import *
from Latex import *
import Rcode
from python2R import dict2assignments, matrix2matrix
from tools import who_am_i
from datetime import datetime

class TestSequenceFunctions(unittest.TestCase):
    def run(self,*args):
        print("####################################################################")
        print(self.id)
        print("####################################################################")
        before=datetime.now()
        super().run(*args)
        after=datetime.now()
        print("time consumed=",after-before)
        print("####################################################################")

    def test_RothC(self):
        C_0,t =symbols("C_0, t",real=True) # these always have to be present because the translation relies on them
        #times and time steps for the numeric modelling
        t_start,t_end,tn=symbols("t_start,t_end,tn")
        C_1, C_2, C_3, C_4, C_5=symbols("C_1, C_2, C_3, C_4, C_5")
        C=Matrix(5,1,[C_1,C_2,C_3,C_4,C_5])

        k_1=Symbol("k_1",real=True) # Decomposition rate of the DPM pool.
        k_2=Symbol("k_2",real=True) # Decomposition rate of the RPM pool.
        k_3=Symbol("k_3",real=True) # Decomposition rate of the BIO pool.
        k_4=Symbol("k_4",real=True) # Decomposition rate of the HUM pool.
        k_5=Symbol("k_5",real=True) # Decomposition rate of the IOM pool.
        DR=Symbol("DR",real=True) # ratio of decomposable plant material to resistant plant material (DPM/RPM).
        clay=Symbol("clay",real=True) # percent clay in mineral soil.
        In=Symbol("In",real=True) # Carbon inputs.

        x=1.67*(1.85+1.6*exp(-0.0786*clay))

        alphas={"1_to_3":(0.46/(x+1)),
                "2_to_3":(0.46/(x+1)),
                "4_to_3":(0.46/(x+1)),
                "1_to_4":(0.54/(x+1)),
                "2_to_4":(0.54/(x+1)),
                "3_to_4":(0.54/(x+1))}

        I=Matrix(5,1,[In*DR/(DR+1), In/(DR+1), 0, 0, 0])
        F=Matrix(5,1,[k_1*C_1, k_2*C_2,
                      (k_3-k_3*0.46/(x+1))*C_3,
                      (k_4-k_4*0.54/(x+1))*C_4,
                      #k_5*C_5])
                      0*C_5])
        
        Mod=RExample(C,alphas,F,I,who_am_i())
        Mod.addParameterSet({k_1:10, k_2:0.3, k_3:0.66, k_4:0.02, #k_5:0, 
                           DR:1.44, clay:23.4, In:1.7,
                           C_0:Matrix(5,1,[0.3, 0.3, 0.3, 0.3, 0.3]),
                           t_start:0,
                           t_end:200,
                           tn:400
                           })
        J=Mod.jacobian()
        # in this case the system is linear and the 
        # Jacobian represents a Matrix that is not C dependent

        # furthermore the RothC model  has a stagnant pool (k5=0) which is 
        # expressed mathematically by the fact that the Jacobian is 
        # rank deficient and thus not invertable 
        # The nullspace or kernel of J is of dimension 1 which translates
        # into a one dimensional vectorspace of fixed points in contrast
        # to the unique solution of a non singular Jacobian as usual.
        
        # We represent this degree of freedom by the free choice of C5
        # and solve only the remaining system
        solutions=J[0:4,0:4].LUsolve(-I[0:4,0]) 
        #ss=J[0:2,0:2].LUsolve(I[0:2,0]) 
        pprint(solutions)
        Mod.suggestFixedPoint({
            C_1:simplify(solutions[0]),
            C_2:simplify(solutions[1]),
            C_3:simplify(solutions[2]),
            C_4:simplify(solutions[3]),
            C_5:C_5
        })
        Mod.latexSummary()
        Mod.writeSoilRModel()

    def test_MEND(self):
        C_0,t =symbols("C_0, t",real=True)
        t_start,t_end,tn=symbols("t_start,t_end,tn")
        P, M, Q, B, De, EP, EM=symbols("P, M, Q, B, De, EP, EM")
        C=Matrix(7,1,[P,M,Q,B,De,EP,EM])

        V_P=Symbol("V_P", real=True,positive=True) # maximum specific decomposition rate for P by EP.
        K_P=Symbol("K_p", real=True,positive=True) # half-saturation constant for decomposition of P.
        V_M=Symbol("V_M", real=True,positive=True) # maximum specific decomposition rate for M by EM.
        K_M=Symbol("K_M", real=True,positive=True) # half-saturation constant for decomposition of M.
        K_BA=Symbol("K_BA", real=True,positive=True) 
        V_D=Symbol("V_D", real=True,positive=True) # maximum specific uptake rate of D for growth of B
        K_D=Symbol("K_D", real=True,positive=True) # half-saturation constant of uptake of D for growth of B
        m_R=Symbol("m_R", real=True,positive=True) # specific maintenance factor or rate.
        E_C=Symbol("E_C", real=True,positive=True) # carbon use efficiency.
        f_D=Symbol("f_D", real=True,positive=True) # fraction of decomposed P allocated to D
        g_D=Symbol("g_D", real=True,positive=True) # fraction of dead B allocated to D
        p_EP=Symbol("p_EP", real=True,positive=True) # fraction of mR for production of EP.
        p_EM=Symbol("p_EM", real=True,positive=True) # fraction of mR for production of EM.
        r_EP=Symbol("r_EP", real=True,positive=True) # turnover rate of EP.
        r_EM=Symbol("r_EM", real=True,positive=True) # turnover rate of EM.
        Q_max=Symbol("Q_max", real=True,positive=True) # maximum DOC sorption capacity.
        K_des=Symbol("K_des", real=True,positive=True) # desorption rate.
        #K_ads=Symbol("K_ads", real=True) # specific adsorption rate.
        K_ads=K_BA*K_des # specific adsorption rate.
        I_P=Symbol("I_P", real=True,positive=True) # input rate of P.
        I_D=Symbol("I_D", real=True,positive=True) # input rate of D.
        P_E=p_EP + p_EM
        F_R=B*((1/E_C)-1)*(De*(V_D+m_R))/(K_D+De)
        F_E=B*m_R
        O_B=F_R+F_E
        F_U=De*((V_D+m_R)/E_C)*(B/(K_D+De))
        F_A=De*(K_ads*(1-Q)/Q_max)
        O_D=F_U+F_A
        f_E=(1-(F_R/O_B))
        f_U=(1-(F_A/O_D))
        f_A=(1-(F_U/O_D))
        alphas={"1_to_2":1-f_D, "1_to_5":f_D,
                "2_to_5":1, "3_to_5":1,
                "4_to_1":(1-g_D)*(1-P_E)*f_E, "4_to_5":g_D*(1-P_E)*f_E, "4_to_6":p_EP*f_E, "4_to_7":p_EM*f_E,
        "5_to_4":f_U, "5_to_3":f_A}
        I=Matrix(7,1,[I_P,0,0,0,I_D,0,0])
        F=Matrix(7,1,[(V_P*EP*P)/(K_P+P),
                      (V_M*EM*M)/(K_M+M),
                      (K_des*Q)/Q_max,
                      O_B,
                      O_D,
                      r_EP*EP,
                      r_EM*EM])
        
        Mod=RExample(C,alphas,F,I,who_am_i())
        Cdot=I+Mod.getOperator()
        # Unique solutions we can often compute automatically and 
        # extract the information automatically
        # but for this example the computation is too expensive 
        # Fortunately the fixed points are published in the original pater
        A=1-E_C+(1-p_EP-p_EM)*E_C*(1-g_D)*(I_D/I_P+1)
        Beq=(I_D+I_P)/(1/(E_C-1)*m_R)
        f1={
            P:K_P/(V_P*p_EP/r_EP*E_C/A*(I_D/I_P+1)-1), 
            M:K_M/(V_M*p_EM/r_EM*E_C/((1-f_D)*A)*(1+I_D/I_P)-1),
            Q:Q_max/(1+V_D/(m_R*K_D*K_BA)),
            B:Beq,
            De:K_D/(V_D/m_R),
            EP:Beq*m_R*p_EP/r_EP,
            EM:Beq*m_R*p_EM/r_EM
        }
        p1={
            V_P:2.5, 
            K_P:50, 
            V_M:1, 
            K_M:250, 
            V_D:0.0005, 
            K_D:0.26, 
            m_R:0.00028,
            E_C:0.47, 
            f_D:0.5, 
            g_D:0.5, 
            p_EP:0.01, 
            p_EM:0.01, 
            r_EP:0.001, 
            r_EM:0.001, 
            Q_max:1.7, 
            K_BA:6,
            #K_ads:0.006, 
            K_des:0.001, 
            I_P:0.00008, 
            I_D:0.00008,
            C_0:Matrix(7,1,[.3, .3, .3, .3, .3, .3, .3]),
            t_start:0,
            t_end:200,
            tn:400}
        f1_num={key:val.subs(p1) for key,val in f1.items()} 
        Mod.suggestFixedPoint(f1)
        #Mod.suggestFixedPoint(f1_num)
        #Mod.addParameterSet(p1)
        Mod.latexSummary()       
        Mod.writeSoilRModel()
    def test_MEND_NUM(self):
        C_0,t =symbols("C_0, t",real=True)
        t_start,t_end,tn=symbols("t_start,t_end,tn")
        P, M, Q, B, De, EP, EM=symbols("P, M, Q, B, De, EP, EM",nonnegative=True)
        C=Matrix(7,1,[P,M,Q,B,De,EP,EM])

        V_P=Symbol("V_P", real=True) # maximum specific decomposition rate for P by EP.
        K_P=Symbol("K_p", real=True) # half-saturation constant for decomposition of P.
        V_M=Symbol("V_M", real=True) # maximum specific decomposition rate for M by EM.
        K_M=Symbol("K_M", real=True) # half-saturation constant for decomposition of M.
        K_BA=Symbol("K_BA", real=True) 
        V_D=Symbol("V_De", real=True) # maximum specific uptake rate of D for growth of B
        K_D=Symbol("K_D", real=True) # half-saturation constant of uptake of D for growth of B
        m_R=Symbol("m_R", real=True) # specific maintenance factor or rate.
        E_C=Symbol("E_C", real=True) # carbon use efficiency.
        f_D=Symbol("f_D", real=True) # fraction of decomposed P allocated to D
        g_D=Symbol("g_D", real=True) # fraction of dead B allocated to D
        p_EP=Symbol("p_EP", real=True) # fraction of mR for production of EP.
        p_EM=Symbol("p_EM", real=True) # fraction of mR for production of EM.
        r_EP=Symbol("r_EP", real=True) # turnover rate of EP.
        r_EM=Symbol("r_EM", real=True) # turnover rate of EM.
        Q_max=Symbol("Q_max", real=True) # maximum DOC sorption capacity.
        K_des=Symbol("K_des", real=True) # desorption rate.
        K_ads=K_BA*K_des # specific adsorption rate.
        #K_ads=Symbol("K_ads", real=True) # specific adsorption rate.
        I_P=Symbol("I_P", real=True) # input rate of P.
        I_D=Symbol("I_D", real=True) # input rate of D.
        P_E=p_EP + p_EM
        F_R=B*((1/E_C)-1)*(De*(V_D+m_R))/(K_D+De)
        F_E=B*m_R
        O_B=F_R+F_E
        F_U=De*((V_D+m_R)/E_C)*(B/(K_D+De))
        F_A=De*(K_ads*(1-Q)/Q_max)
        O_D=F_U+F_A
        f_E=(1-(F_R/O_B))
        f_U=(1-(F_A/O_D))
        f_A=(1-(F_U/O_D))
        alphas={"1_to_2":1-f_D, "1_to_5":f_D,
                "2_to_5":1, "3_to_5":1,
                "4_to_1":(1-g_D)*(1-P_E)*f_E, "4_to_5":g_D*(1-P_E)*f_E, "4_to_6":p_EP*f_E, "4_to_7":p_EM*f_E,
        "5_to_4":f_U, "5_to_3":f_A}
        I=Matrix(7,1,[I_P,0,0,0,I_D,0,0])
        F=Matrix(7,1,[(V_P*EP*P)/(K_P+P),
                      (V_M*EM*M)/(K_M+M),
                      (K_des*Q)/Q_max,
                      O_B,
                      O_D,
                      r_EP*EP,
                      r_EM*EM])
        
        Mod=RExample(C,alphas,F,I,who_am_i())
        Cdot=I+Mod.getOperator()
        #analytical fixed point solutions
        #for unique solutions we can extract the information automatically
        #solutions=solve(Cdot,[C[i] for i in range(0,len(C))])
        #Mod.suggestFixedPoint( {C[i]:solutions[C[i]] for i in range(0,len(C))})
        
        #s1={V_P:2.5, K_P:50, V_M:1, K_M:250, V_D:0.0005, K_D:0.26, m_R:0.00028, E_C:0.47, f_D:0.5, g_D:0.5, p_EP:0.01, p_EM:0.01, r_EP:0.001, r_EM:0.001, Q_max:1.7, K_ads:0.006, K_des:0.001, I_P:0.00008, I_D:0.00008}

        A=1-E_C+(1-p_EP-p_EM)*E_C*(1-g_D)*(I_D/I_P+1)
        Beq=(I_D+I_P)/(1/(E_C-1)*m_R)
        f1={
            P:K_P/(V_P*p_EP/r_EP*E_C/A*(I_D/I_P+1)-1), 
            M:K_M/(V_M*p_EM/r_EM*E_C/((1-f_D)*A)*(1+I_D/I_P)-1),
            Q:Q_max/(1+V_D/(m_R*K_D*K_BA)),
            B:Beq,
            De:K_D/(V_D/m_R),
            EP:Beq*m_R*p_EP/r_EP,
            EM:Beq*m_R*p_EM/r_EM
        }
        p1={
            V_P:2.5, 
            K_P:50, 
            V_M:1, 
            K_M:250, 
            V_D:0.0005, 
            K_D:0.26, 
            m_R:0.00028,
            E_C:0.47, 
            f_D:0.5, 
            g_D:0.5, 
            p_EP:0.01, 
            p_EM:0.01, 
            r_EP:0.001, 
            r_EM:0.001, 
            Q_max:1.7, 
            K_BA:6,
            #K_ads:0.006, 
            K_des:0.001, 
            I_P:0.00008, 
            I_D:0.00008,
            C_0:Matrix(7,1,[.3, .3, .3, .3, .3, .3, .3]),
            t_start:0,
            t_end:200,
            tn:400 }
        f1_num={key:val.subs(p1) for key,val in f1.items()} 

        Mod.suggestFixedPoint(f1_num)
        Mod.addParameterSet(p1)
 #       #numerical solutionss
 #       CdotNum=Cdot.subs(p1)
 #       JNum=Mod.jacobian().subs(p1)
 #       from scipy.optimize import fsolve,root
 #       from numpy import array
 #       import math
 #       def evalJ(intup):
 #           # read from the input tuple
 #           dic={C[i]:intup[i] for i in range(0,len(intup))}
 #           M=JNum.subs(dic)
 #           return(array(M.tolist(),dtype="float"))
 #       #Jeval=lambdify(tuple(C),JNum,use_array=True)

 #       def evalCdotNum(intup):
 #           # read from the input tuple
 #           dic={C[i]:intup[i] for i in range(0,len(intup))}
 #           outtup=tuple(CdotNum.subs(dic))
 #           return(outtup)

 #       #sol=fsolve(
 #       #    #full_output=True,
 #       #    func=evalCdotNum,
 #       #    fprime=evalJ,
 #       #    maxfev=10000,
 #       #    x0=tuple([20 for i in range(0,len(C))])) 
 #       #print(sol)
 #       #print(evalCdotNum(sol))
 #       sol2=root(
 #           evalCdotNum,
 #           tuple([.1 for i in range(0,len(C))]),
 #           method="hybr",
 #           jac=evalJ)
 #       print(sol2)
 #       #test the result
 #       print(evalCdotNum(sol2.x))
        Mod.latexComponents()       
        #Mod.writeSoilRModel()
    def test_Yasso(self):
        C_0,t =symbols("C_0, t",real=True)
        t_start,t_end,tn=symbols("t_start,t_end,tn")
        A, W, E, N, H=symbols("A, W, E, N, H")
        C=Matrix(5,1,[A,W,E,N,H])

        k_A=Symbol("k_A",real=True) # Decomposition rate of the A pool.
        k_W=Symbol("k_W",real=True) # Decomposition rate of the W pool.
        k_E=Symbol("k_E",real=True) # Decomposition rate of the E pool.
        k_N=Symbol("k_N",real=True) # Decomposition rate of the N pool.
        k_H=Symbol("k_H",real=True) # Decomposition rate of the H pool.
        p1=Symbol("p1",real=True) # Transfer coefficient.
        p2=Symbol("p2",real=True) # Transfer coefficient.
        p3=Symbol("p3",real=True) # Transfer coefficient.
        p4=Symbol("p4",real=True) # Transfer coefficient.
        p5=Symbol("p5",real=True) # Transfer coefficient.
        p6=Symbol("p6",real=True) # Transfer coefficient.
        p7=Symbol("p7",real=True) # Transfer coefficient.
        p8=Symbol("p8",real=True) # Transfer coefficient.
        p9=Symbol("p9",real=True) # Transfer coefficient.
        p10=Symbol("p10",real=True) # Transfer coefficient.
        p11=Symbol("p11",real=True) # Transfer coefficient.
        p12=Symbol("p12",real=True) # Transfer coefficient.
        pH=Symbol("pH",real=True) # Transfer coefficient.
        In=Symbol("In",real=True) # Litter inputs.

        alphas={"2_to_1":p1, "3_to_1":p2, "4_to_1":p3, "1_to_2":p4,
                "3_to_2":p5, "4_to_2":p6, "1_to_3":p7, "2_to_3":p8,
                "4_to_3":p9, "1_to_4":p10, "2_to_4":p11, "3_to_4":p12,
                "1_to_5":pH, "2_to_5":pH, "3_to_5":pH, "4_to_5":pH}

        I=Matrix(5,1,[In,0,0,0,0])
        F=Matrix(5,1,[k_A*A, k_W*W, k_E*E, k_N*N, k_H*H])
        
        Mod=RExample(C,alphas,F,I,who_am_i())
        Cdot=I+Mod.getOperator()
        #for unique solutions we can extract the information automatically
        solutions=solve(Cdot,[C[i] for i in range(0,len(C))])
        Mod.suggestFixedPoint( {C[i]:solutions[C[i]] for i in range(0,len(C))})

        Mod.addParameterSet({k_A:0.66,k_W:4.3, k_E:0.35, k_N:0.22, k_H:0.0033,
                           p1:0.32, p2:0.01, p3:0.93, p4:0.34, p5:0, p6:0, p7:0,
                           p8:0, p9:0.01, p10:0, p11:0, p12:0.92, pH:0.04, In:0,
                           t_start:0,
                           t_end:2e3,
                           tn:2e2,
                           C_0:Matrix(5,1,[.1,.1,.1,.1,.1])
                           })
         
        Mod.latexSummary()
        Mod.writeSoilRModel()

    def test_bacwave(self):
        C_0,t =symbols("C_0, t",real=True) # these always have to be present because the translation relies on them
        #times and time steps for the numeric modelling 
        t_start,t_end,tn=symbols("t_start,t_end,tn") 
        S, X, C_0=symbols("S, X, C_0") 
        C=Matrix(2,1,[S,X]) 
        
        u_max=Symbol("u_max",real=True) # a scalar representing the maximum relative growth rate of bacteria (hr-1)
        k_s=Symbol("k_s",real=True) # a scalar representing the substrate constant for growth (ug C /ml soil solution)
        theta=Symbol("theta",real=True,nonnegative=True) # a scalar representing soil water content (ml solution/cm3 soil)
        D_max=Symbol("D_max",real=True) # a scalar representing the maximal relative death rate of bacteria (hr-1)
        k_d=Symbol("k_d",real=True) # a scalar representing the substrate constant for death of bacteria (ug C/ml soil solution)
        k_r=Symbol("k_r",real=True) # a scalar representing the fraction of death biomass recycling to substrate (unitless)
        Y=Symbol("Y",real=True) # a scalar representing the yield coefficient for bacteria (ug C/ugC)
        BGF=Symbol("BGF",real=True) # a scalar representing the constant background flux of substrate (ug C/cm3 soil/hr)
        Exu_max=Symbol("Exu_max",real=True) # a scalar representing the maximal exudation rate (ug C/(hr cm3 soil))
        Exu_T=Symbol("Exu_T",real=True,positive=True) # a scalar representing the  time constant for exudation, responsible for duration of exudation (1/hr).
        #respired carbon fraction
        
        # the alphas have to be a function of C and t (at most)
        alphas={"1_to_2":Y,"2_to_1":k_r}

        F=Matrix(2,1,[
            (X/Y)*(u_max*S)/((k_s*theta)+S), 
            (D_max*k_d*X)/(k_d+(S/theta)) 
            ])
        I=Matrix(2,1,[BGF+(Exu_max*exp(-Exu_T*t)),0])
        
        tPM=RExample(C,alphas,F,I,who_am_i())
        # we try to solve for the fixed points
        mat=I+tPM.getOperator()
        asymptotic_mat=Matrix(2,1,[limit(l,t,oo) for l in mat])
        ##solutions=solve(mat,X,S) # would lead a "time dependent fixed point" which seems a bit of a contradiction
        # the order of the unknowns seems to be important solve(mat,S,X) takes much longer
        asymptotic_solutions=solve(asymptotic_mat,X,S) # the order of the unknowns seems to be important solve(mat,S,X) takes much longer
        #nontrivial=solutions[2]
        # in this case we choose manually the third solution
        nontrivial=asymptotic_solutions[2]
        pprint(nontrivial)
        tPM.suggestFixedPoint({
            X:simplify(nontrivial[0]),
            S:simplify(nontrivial[1])
            })
        
        dic={ 
            u_max:0.063, 
            k_s:3.0,
            theta:0.23, 
            D_max:0.26, 
            k_d:14.5, 
            k_r:0.4, 
            Y:0.44, 
            BGF:0.15, 
            Exu_max:8, 
            Exu_T:0.8, 
            # the following are allways required for R
            t_start:0,
            t_end:800,
            tn:8000,
            C_0:Matrix(2,1,[ 0.5,1.5 ])
        	}
        tPM.addParameterSet(dic)
        tPM.writeSoilRModel()
        tPM.latexSummary()

if __name__ == '__main__':
    suite = unittest.TestLoader().loadTestsFromTestCase(TestSequenceFunctions)
    # Run same tests across 16 processes
    concurrent_suite = ConcurrentTestSuite(suite, fork_for_tests(1))
    #concurrent_suite = ConcurrentTestSuite(suite, fork_for_tests(16))
    runner = unittest.TextTestRunner()
    runner.run(concurrent_suite)

    #unittest.main()
 
