#!/usr/bin/Rscript
# vim:set ff=unix expandtab ts=2 sw=2:
require(devtools)
require('tools')
# find relative path to this script from the current wd
pkgName <- 'SoilR'
initial.options <- commandArgs(trailingOnly = FALSE)
file.arg.name <- "--file="
script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
ss<- dirname(script.name)
ss<- '.'
ignoreListPath<-  file.path(ss,"manually_maintained_list_of_ignored_words.txt") 
manDictPath <-  file.path(ss,"acceptedWords.rds") 
autoDictPath <-  file.path(ss,"SoilR_object_names.rds") 
saveRDS(readLines(ignoreListPath),manDictPath)

packageDir=file.path(ss,"..","..","pkg")
# install pkg to be able to find the names of classes and functions
if (!is.element('devtools',installed.packages())){
	install.packages('devtools',repos='https://cran.uni-muenster.de')
}
#install(packageDir)
require(pkgName,character.only=TRUE)
fqPkgName <- sprintf('package:%s',pkgName)
print(fqPkgName)
objectNames<- ls(as.environment(fqPkgName))
objectNames <- c(objectNames,getClasses(fqPkgName))
objectNames <- c(objectNames,pkgName)

'InFlux' %in% objectNames
manDir <- file.path(packageDir,'man')
f <- 'Model.Rd'
p <- file.path(manDir,f)
lines<- RdTextFilter(p)
for (name in objectNames){
  lines <- gsub(pattern=name,replacement='',x=lines)
}
myFilter <- function(ifile,encoding='unknown',keepSpacing=TRUE,drop=character()){
  lines <- RdTextFilter(ifile,encoding,keepSpacing,drop)
  for (name in objectNames){
    lines <- gsub(pattern=name,replacement='',x=lines)
  }
  return(lines)
}
res <- aspell(
  files=c(p),
  filter=myFilter,
  control=list( "--master=en_US" , "--add-extra-dicts=en_GB") 
)
print(res)
##objectNames <- c('TimeMap')
#saveRDS(objectNames,autoDictPath)
#
##funcList=c(aspell_package_R_files,aspell_package_Rd_files,aspell_package_vignettes)
#res <- aspell_package_Rd_files( dir=packageDir,
#              control=list( "--master=en_US" , "--add-extra-dicts=en_GB") 
#            , drop=c('\\author','\\name','\\alias') , dictionaries=autoDictPath)
#print(res)
