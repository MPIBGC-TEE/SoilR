#!/usr/bin/Rscript
# vim:set ff=unix expandtab ts=2 sw=2:
require(devtools)
require('tools')
# find relative path to this script from the current wd
initial.options <- commandArgs(trailingOnly = FALSE)
file.arg.name <- "--file="
RScript_cond <- any(grepl(pattern=file.arg.name,initial.options))
if(RScript_cond){
  script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
  ss<- dirname(script.name)
  print(sprintf('The script resides in %s.',ss))
}else{
  ss<- '.'
}
arg.name='--args'
RdTargetFileCond <- any(grepl(pattern=arg.name,initial.options))
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
  #fqPkgName <- sprintf('package:%s',pkgName)
objectNames <- c()
argNames  <- c()
funcs<-list()
n <- length(search())
for (pkg in head(search(),min(3,n))){
  pkgObjects <- ls(pkg)
  for (fn in pkgObjects){
    f<-eval(as.symbol(fn))
    if (is.function(f)){
    funcs[[fn]]<-f
    }
  }
  argNames <- c(argNames,unique(as.character(unlist(lapply(funcs,function(fun){names(formals(fun))})))))
  objectNames<- c(objectNames, pkgObjects)
  objectNames <- c(objectNames,getClasses(pkg))
  #objectNames <- c(objectNames,pkg)
}
print(objectNames)


manDir <- file.path(packageDir,'man')
if(RdTargetFileCond){
  RdTargetFileName<-initial.options[grep(pattern=arg.name,initial.options)+1]
  files <- list(file.path(manDir,RdTargetFileName))
}else{
  files=list.files(path=manDir,pattern='.*.Rd',full.names=TRUE)
}
print(files)

myFilter <- function(
    ifile,
    encoding='unknown',
    keepSpacing=TRUE,
    drop=c('\\references')
  ){
  # remove function and class names
  lines <- RdTextFilter(ifile,encoding,keepSpacing,drop)
  for (on in setdiff(objectNames,c('(','{','[' ,'[<-','[[','[[<-','[.AsIs','[<-.data.frame','[.data.frame','[[<-.data.frame','[[.data.frame'))){
    #print(on)
    lines <- gsub(pattern=sprintf('(\\W)%s(\\W)',on),replacement='\\1 \\2',x=lines)
  }
  # remove function arguments
  #for (an in argNames){
  # lines <- gsub(pattern=sprintf('\\item\\{%s\\}',an),replacement=' ',x=lines)
  #}
  return(lines)
}
res <- aspell(
  files=files,
  filter=myFilter,
  control=list( "--master=en_US" , "--add-extra-dicts=en_GB") ,
  dictionaries=c(manDictPath) 
)
print(res)
writeLines(summary(res),'possibly_mis-spelled_words')

