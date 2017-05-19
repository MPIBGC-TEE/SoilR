from Rexample import *
class RexampleWithTransfer(Rexample):
    def __init__(self,name,matrix,iv):
        nr=matrix.rows 
        inputrates=zeros(nr,1)
        iv=Matrix(iv)/Matrix(iv).norm()
        super(RexampleWithTransfer,self).__init__(name,matrix,iv,inputrates)
        self.addTransfer()
        self.trunk="runit.Manzoni."+name
    def addTransfer(self):
        self.anlresptotal=sum(self.anlresp.col(0))
        matCk=Matrix(n,1,self.ck)
        self.normCk=matCk/matCk.norm()
        self.meanTransitTime=integrate(self.anlresptotal*t,(t,0,oo))
    def meanTransitTime(self):
        Text="\
        tm=getMeanTransferTime(A,\n"
        +rlistprint(self.rc_sym_strs,self.shift)\
        +")\n"        
        return(Text)
        


