#!/usr/bin/Rscript

Desc<-desc::desc("../pkg/DESCRIPTION")
dependencies<-Desc$get_deps()
imports<-dependencies[dependencies$type=="Imports",2]
#print(imports)
install.packages(pkgs=imports, repos="https://cloud.r-project.org")

