#!/usr/bin/Rscript
# vim:set ff=unix expandtab ts=2 sw=2:
# find relative path to this script from the current wd
pkgName <- 'SoilR'
initial.options <- commandArgs(trailingOnly = FALSE)
file.arg.name <- "--file="
script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
script.basename <- dirname(script.name)
ss<- dirname(script.name)
ignoreListPath<-  file.path(ss,"manually_maintained_list_of_ignored_words.txt") 
manDictPath <-  file.path(ss,"acceptedWords.rds") 
autoDictPath <-  file.path(ss,"SoilR_object_names.rds") 
saveRDS(readLines(ignoreListPath),manDictPath)

sr=file.path(script.basename,"..","..","pkg")
# install pkg to be able to find the names of classes and functions
if (!is.element('devtools',installed.packages())){
	install.packages('devtools',repos='https://cran.uni-muenster.de')
}
require(devtools)
install(sr)
require(pkgName,character.only=TRUE)
fqPkgName <- sprintf('package:%s',pkgName)
print(fqPkgName)
objectNames<- ls(as.environment(fqPkgName))
print(objectNames)
filter <- function(){
  #do something with the object names here
}

#funcList=c(aspell_package_R_files,aspell_package_Rd_files,aspell_package_vignettes)
res <- aspell_package_Rd_files(
  dir=sr,
  control=list(
    "--master=en_US"
    # if we add additional dictionaries less errors are reported
    # since more words are "known".
    ,
    "--add-extra-dicts=en_GB"
  )
  ,
  drop=c('\\author','\\name','\\alias')
  ,
  dictionaries=c(manDictPath)
  ,
  ignore=objectNames
)
print(res)
