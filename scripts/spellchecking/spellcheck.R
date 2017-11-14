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
  #fqPkgName <- sprintf('package:%s',pkgName)
objectNames <- c()
argNames  <- c()
funcs<-list()
for (pkg in head(search(),16)){
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


manDir <- file.path(packageDir,'man')
files=list.files(path=manDir,pattern='.*.Rd')

myFilter <- function(
    ifile,
    encoding='unknown',
    keepSpacing=TRUE,
    drop=c('\\references')
  ){
  #lines <- readLines(file.path(manDir,'AbsoluteFractionModern_from_Delta14C-method_6803e156.Rd'))
  # remove function and class names
  lines <- RdTextFilter(ifile,encoding,keepSpacing,drop)
  #p <- paste(objectNames,collapse='|')
  for (on in setdiff(objectNames,c('(','{','[' ,'[<-','[[','[[<-','[.AsIs','[<-.data.frame','[.data.frame','[[<-.data.frame','[[.data.frame'))){
    #print(on)
    lines <- gsub(pattern=on,replacement='',x=lines)
  }
  # remove function arguments
  #p<- sprintf('\\item\\{(%s)\\}',paste(argNames,collapse='|'))
  for (an in argNames){
   lines <- gsub(pattern=sprintf('\\itme\\{%s\\}',an),replacement='',x=lines)
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

