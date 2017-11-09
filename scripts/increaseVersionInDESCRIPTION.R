#!/usr/bin/env Rscript
# find relative path to this script from the current wd
# this script will be linked to .git/hooks/pre-commit
# so that it is executed before a commit
initial.options <- commandArgs(trailingOnly = FALSE)
file.arg.name <- "--file="
script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
script.basename <- dirname(script.name)
script.basename='.'
path.to.DESCRIPTION<- file.path(script.basename,'..','..','pkg','DESCRIPTION')
DescriptionMatrix <- read.dcf(path.to.DESCRIPTION)

versionString <- DescriptionMatrix[[1,'Version']]  
s <- '-'
Parts <- strsplit(versionString,split=s)[[1]]
trunk <- Parts[[1]]
counter <- as.integer(Parts[[2]])
newVersionString <- paste(trunk,counter+1,sep=s) 
print(newVersionString)
DescriptionMatrix[[1,'Version']]   <- newVersionString
write.dcf(DescriptionMatrix,file=path.to.DESCRIPTION)

