#!/usr/bin/Rscript
# vim:set ff=unix expandtab ts=2 sw=2:
require(devtools)
require('tools')
# find relative path to this script from the current wd
initial.options <- commandArgs(trailingOnly = FALSE)
file.arg.name <- "--file="
cond <- any(grepl(pattern=file.arg.name,initial.options))
if(cond){
  script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
  ss<- dirname(script.name)
  print(sprintf('The script resides in %s.',ss))
}else{
  ss<- '.'
}

pkgName <- 'SoilR'
ignoreListFn<- "manually_maintained_list_of_ignored_words.txt"
trunk <- strsplit(ignoreListFn,split='\\.')[[1]][[1]]
print(trunk)
ignoreListPath <-  file.path(ss,ignoreListFn) 
# read the list of ignored words
allLines <- readLines(ignoreListPath)
words <- allLines[3:length(allLines)]
# write it back sorted
writeLines(c(allLines[1:2],sort(words)),con=ignoreListPath)
# and create an rds file with the same content
manDictPath <-  file.path(ss,sprintf("%s.rds",trunk)) 
print(manDictPath)
saveRDS(words,manDictPath)

packageDir=file.path(ss,"..","..","pkg")
# install pkg to be able to find the names of classes and functions
if (!is.element('devtools',installed.packages())){
	install.packages('devtools',repos='https://cran.uni-muenster.de')
}
install(packageDir)
require(pkgName,character.only=TRUE)
fqPkgName <- sprintf('package:%s',pkgName)
print(fqPkgName)
objectNames<- ls(as.environment(fqPkgName))
funcs<-list()
for (fn in objectNames){
  f<-eval(as.symbol(fn))
  if (is.function(f)){
    funcs[[fn]]<-f
    }
}
print(names(formals(funcs[[1]])))
argNames <- unique(as.character(unlist(lapply(funcs,function(fun){names(formals(fun))}))))
writeLines(argNames,'argNames')
objectNames <- c(objectNames,getClasses(fqPkgName))
objectNames <- c(objectNames,pkgName)
#objectNames <- c(objectNames,argNames)
print(objectNames)
manDir <- file.path(packageDir,'man')
files=list.files(path=manDir,pattern='.*.Rd')

myFilter <- function(
  ifile,
  encoding='unknown',
  keepSpacing=TRUE,
  drop=c('\\references')
){
  lines <- RdTextFilter(ifile,encoding,keepSpacing,drop)
  for (name in objectNames){
    lines <- gsub(pattern=name,replacement='',x=lines)
  }
  return(lines)
}
res <- aspell(
  files=list.files(path=manDir,pattern='.*.Rd',full.names=TRUE),
  filter=myFilter,
  control=list( "--master=en_US" , "--add-extra-dicts=en_GB") ,
  dictionaries=c(manDictPath) 
)
print(res)
writeLines(summary(res),'possibly_mis-spelled_words')

