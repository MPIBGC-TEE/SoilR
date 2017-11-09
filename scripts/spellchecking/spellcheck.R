#!/usr/bin/Rscript
# vim:set ff=unix expandtab ts=2 sw=2:
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

sr=file.path(ss,"..","..","pkg")
# install pkg to be able to find the names of classes and functions
if (!is.element('devtools',installed.packages())){
	install.packages('devtools',repos='https://cran.uni-muenster.de')
}
require(devtools)
#install(sr)
require(pkgName,character.only=TRUE)
fqPkgName <- sprintf('package:%s',pkgName)
print(fqPkgName)
objectNames<- ls(as.environment(fqPkgName))
#objectNames <- c('TimeMap')
saveRDS(objectNames,autoDictPath)

#funcList=c(aspell_package_R_files,aspell_package_Rd_files,aspell_package_vignettes)
res <- aspell_package_Rd_files( dir=sr,control=list( "--master=en_US" , "--add-extra-dicts=en_GB") , drop=c('\\author','\\name','\\alias') , dictionaries=autoDictPath)
print(res)
