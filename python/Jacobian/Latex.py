#!/usr/bin/python3
# vim:set ff=unix expandtab ts=4 sw=4:
import re as regexp
import os
import subprocess
from pathlib import Path
from sympy import *
from string import *
from contextlib import contextmanager
import matplotlib.pyplot as plt
class LatexException(Exception):
    def __init__(self,err):
        self.err=err
    def __str__(self):
        return("problems during LaTeX compilation "+repr(self.err))


def ml(ex):
    st=latex(ex)
    st=regexp.sub("smallmatrix","matrix",st)

    st=regexp.sub("left\[","left(",st)
    st=regexp.sub("right\]","right)",st)

    #st=regexp.sub("\\\\begin{pmatrix}","\\\\left(",st) #pmatrix would not be broken by the breqn package
    #st=regexp.sub("\\\\end{pmatrix}","\\\\right)",st)
    st=regexp.sub("\\\\begin{pmatrix}","",st) #pmatrix would not be broken by the breqn package
    st=regexp.sub("\\\\end{pmatrix}","",st)
    return(st) 

class LatexTemplate(Template):
     def substitute(self, *args, **kws):
         if len(args) > 1:
             raise TypeError('Too many positional arguments')
         if not args:
              mapping = kws
         elif kws:
              mapping = _multimap(kws, args[0])
         else:
              mapping = args[0]
         # Helper function for .sub()
         def convert(mo):
             # Check the most common path first.
             named = mo.group('named') or mo.group('braced')
             if named is not None:
                 val = ml(mapping[named])
                 # We use this idiom instead of str() because the latter will
                 # fail if val is a Unicode containing non-ASCII characters.
                 return '%s' % (val,)
             if mo.group('escaped') is not None:
                return self.delimiter
             if mo.group('invalid') is not None:
                 self._invalid(mo)
             raise ValueError('Unrecognized named group in pattern', self.pattern)
         return self.pattern.sub(convert, self.template)
class Latex:
    def __init__(self,dir,filenameTrunk):
        self.dir=dir
        self.main=filenameTrunk+".tex"
        self.pdf=filenameTrunk+".pdf"
        self.log=filenameTrunk+".log"
        self.input="inputPart.tex"
        self.values={}
        self.textParts=[]
    def addFigure(self,name,capStr,width="\\textwidth"):
        incStr="\\includegraphics{VdotSurf"
        FigureStr=Template("\
        \n\\begin{figure}\
        \\includegraphics[width=$width]{$name}\
        \\captionsetup{singlelinecheck=off}\
        \\caption[.]{$capStr}\
        \n\\end{figure}\n").substitute(name=name,capStr=capStr,width=width)
        self.textParts.append(FigureStr)

    def addExpression(self,string):
        self.values["string"]=eval(string)
    @classmethod 
    def text(self,tString,*args,**kws):
        t=LatexTemplate(tString).substitute(*args,**kws)+" "
        return(t)
        
    def add_string(self,tString,*args,**kws):
        self.textParts.append(tString)

    def addText(self,tString,*args,**kws):
        t=Latex.text(tString,*args,**kws)
        #t=LatexTemplate(tString).substitute(*args,**kws)+" "
        self.textParts.append(t)

    def addDisplayMath(self,mathString,*args,**kws):
        t=LatexTemplate(mathString).substitute(*args,**kws)
        self.textParts.append("\n\\[\n"+t+"\n\\]\n")
    
    def dmath(self,mathString,*args,**kws):
        t=LatexTemplate(mathString).substitute(*args,**kws)
        return("\n\\begin{dmath}\n"+t+"\n\\end{dmath}")

    def dmath_star(self,mathString,*args,**kws):
        t=LatexTemplate(mathString).substitute(*args,**kws)
        return("\n\\begin{dmath*}\n"+t+"\n\\end{dmath*}")

    def add_dmath(self,mathString,*args,**kws):
        self.textParts.append(self.dmath(mathString,*args,**kws))

    def add_dmath_star(self,mathString,*args,**kws):
        self.textParts.append(self.dmath_star(mathString,*args,**kws))
    
    def add_dgroup(self,mathStringList,*args,**kws):
        t="\n".join([self.dmath(mS,*args,**kws) for mS in mathStringList]) 
        delim="dgroup"
        self.textParts.append("\n\\begin{"+delim+"}\n"+t+"\n\\end{"+delim+"}\n")

    def dgroup_star(self,mathStringList,*args,**kws):
        t="\n".join([self.dmath_star(mS,*args,**kws) for mS in mathStringList]) 
        delim="dgroup*"
        return("\\begin{"+delim+"}\n"+t+"\n\\end{"+delim+"}\n")

    def add_dgroup_star(self,mathStringList,*args,**kws):
        t="\n".join([self.dmath_star(mS,*args,**kws) for mS in mathStringList]) 
        delim="dgroup*"
        self.textParts.append(self.dgroup_star(mathStringList,*args,**kws))


    def addEquationArray(self,mathStringList,*args,**kws):
        t="\\\\".join([LatexTemplate(mS).substitute(*args,**kws) for mS in mathStringList]) 
        self.textParts.append("\n\\begin{eqnarray}\n"+t+"\n\\end{eqnarray}\n")

    def addEquationArrayNoNumber(self,mathStringList,*args,**kws):
        t="\\\\".join([LatexTemplate(mS).substitute(*args,**kws) for mS in mathStringList]) 
        self.textParts.append("\n\\begin{eqnarray*}\n"+t+"\n\\end{eqnarray*}\n")

    def printParts(self):
        return("".join(self.textParts))

    def writeMain(self):
        Text="\
        \\documentclass[10pt,a4paper]{article}\n\
        \\usepackage{amsmath,amssymb,amsfonts,amscd,color,graphicx,caption}\n\
        \\usepackage{breqn} %should be loaded last \n\
        \\begin{document}\n"
        Text+=self.printParts()\
    +"\
        \\end{document}".replace("        ","")
        f=open(self.main,"w")
        f.write(Text)
        f.close()

        
    def write(self):
        orgdir=os.getcwd()
        dp=Path(self.dir)
        if not(dp.exists()):
            dp.mkdir()
        os.chdir(self.dir)
        try:
            self.writeMain()
            #out=subprocess.check_output(["pdflatex","-interaction=nonstopmode", self.main],stderr=subprocess.STDOUT)
            #print(self.main)
            subprocess.check_call(["rm","-rf", self.pdf])
            out=subprocess.check_output(["pdflatex","-interaction=nonstopmode", self.main])
        except subprocess.CalledProcessError as e:
            out=e.output
            print(out)
            raise(LatexException(e))
        finally: 
            os.chdir(orgdir)



    @contextmanager
    def automaticMatplotLibFigure(self,ref,caption):
        fig=plt.figure()
        yield fig
        p=Path(self.dir,ref)
        pStr=p.as_posix()
        fig.savefig(pStr+".pdf")
        self.addFigure(ref,caption)

