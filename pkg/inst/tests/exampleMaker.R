
# vim:set ff=unix ts=2 sw=2:
argstring <- function(arglist){
	argnames <- names(arglist)
	paste(
		lapply(
			seq_along(arglist),
			function(i){
				sprintf(
					'%s = %s',
					argnames[[i]],
					arglist[[i]]
				)
			}
		),
		collapse=',\n\t'
	)
}

Example <- function(targetSym='obj',targetFuncName,combi){
	if('prolog' %in% names(combi)){
		prolog <- str_trim(str_split(combi[['prolog']],pattern='\n')[[1]])
	}else{
		prolog <- ""
	}
	argList <- combi[['args']]
	comm<- combi[['comm']]
	print(length(comm))
  argNames <- names(argList)
	paste(
		paste(
			sprintf('# %s',str_trim(str_split(comm,pattern='\n')[[1]])),
			collapse="\n"
		),
		'\n',
		paste(prolog,collapse='\n'),
		'\n\n',
		sprintf(
  		'%s <- %s(\n\t%s\n)',
			targetSym,
			targetFuncName,
			argstring(argList)
		)
	)
}
 
writeTests<- function(listName,env=parent.frame()){
	d <- 'automaticR'
	if(!dir.exists(d)){
		dir.create(d)
	}
	path <- file.path(d,sprintf('runit.%s.R',listName))
	intro <- '# This file has been automatically created by the script "exampleMaker.R". Change this script for permanent changes.'
	test_list <- c(intro,get(listName,env))
	write(paste(test_list),file=path)
}
writeEx <- function(listName,env=parent.frame()){
	d <- 'automaticR'
	if(!dir.exists(d)){
		dir.create(d)
	}
	path <- file.path(d,sprintf('%s.R',listName))
	list_ <- get(listName,env)
	write(paste(list_),file=path)
}

scalarValuedTimeMapArgs<- list(	
	list(
		args=list(
      	times='0:99',
      	data='C14Atm_NH[1:100,2]',
				lag='1.1'
		),
		comm='a vector of times,a vector of scalar fractions per time step and a scalar lag'
	)
)


vectorValuedTimeMapArgs<- list(	
  list(
		args=list(
			map='function(t){
				c(
					1+sin(t),
					2+sin(2*t)
				)
			}',
			starttime='1',
			endtime='10'
		),
		comm='A function that is valid in a time interval bounded by tstart and tend'
	)
	,
	list(
		args=list(
      	times='0:99',
      	data='C14Atm_NH[1:100,2]',
				lag='c(1.1,1.2)'
		),
		comm='a vector of times, a vector of scalar fractions per time step and a vector lag'
	)
	,
	list(
		prolog='
		times <- seq(1,10,by=0.1)
		a <- array(dim=c(2,length(times)))
		a[1,] <- -0.1*(sin(times)+1.1)
		a[2,] <- -0.2*(sin(times)+1.2)'
		,
  	args=list(
			map='list(times=times,data=a)'
		),
		comm=' We could also imagine time series data 
		stored in an array consisting of
	  many stacked vectors, one for each time step.
		and combine both to a list.'
	)
)
#argstring(combi[[1]])
BoundFcArgs <- c(vectorValuedTimeMapArgs,scalarValuedTimeMapArgs)
TimeMapArgs <- c(vectorValuedTimeMapArgs,scalarValuedTimeMapArgs)
BoundFcExamples <- unlist(
  lapply(
		BoundFcArgs,
		function(combi){
			adds <- list("'Delta14C'","'AbsoluteFractionModern'")
			lapply(
				adds,
				function(add){
					combi[[1]] <- append(combi[[1]],list(format=add)) 
					targetSym <- 'x'
					Example(targetSym,"BoundFc",combi)
				}
			)
		}
	)
)
BoundFcTests<- unlist(
  lapply(
		seq_along(BoundFcArgs),
		function(i){
			combi <- BoundFcArgs[[i]]
			targetFuncName <- 'BoundFc'
			adds <- list("'Delta14C'","'AbsoluteFractionModern'")
			lapply(
				seq_along(adds),
				function(j,i){
					add <- adds[[j]]
					combi[[1]] <- append(combi[[1]],list(format=add)) 
					targetSym <- sprintf('bfc_%s',2*(i-1)+j)
					exText <- Example(targetSym,targetFuncName,combi)
					testText <- paste(
							c(
								sprintf('test.%s.%s <- function(){',targetFuncName,targetSym),
								exText,
								sprintf('plot(%s)',targetSym),
								'}'
							),
						  collapse='\n'
					)
				}
				,i
			)
		}
	)
)

TimeMapExamples <- lapply(
	seq_along(vectorValuedTimeMapArgs),
	function(i){
		combi <- vectorValuedTimeMapArgs[[i]]
		targetSym=sprintf('tm_%s',i)
		Example(targetSym,"TimeMap",combi)})

TimeMapTests<- unlist(
  lapply(
		seq_along(TimeMapArgs),
		function(i){
			combi <- TimeMapArgs[[i]]
			targetFuncName <- 'TimeMap'
			targetSym <- sprintf('bfc_%s',i)
			exText <- Example(targetSym,targetFuncName,combi)
			testText <- paste(
					c(
						sprintf('test.%s.%s <- function(){',targetFuncName,targetSym),
						exText,
						sprintf('plot(%s)',targetSym),
						'}'
					),
				  collapse='\n'
			)
		}
	)
)
writeEx('TimeMapExamples')
writeEx('BoundFcExamples')
writeTests('BoundFcTests')
writeTests('TimeMapTests')
#writeExamples <- function(listOfExamples){
#  lapply(listOfExamples,toString)
#}
#writeExamples(Examples)
