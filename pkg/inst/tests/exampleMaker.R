
# vim:set ff=unix ts=2 sw=2:
argstring <- function(arglist){
	argnames <- names(arglist)
	lapply(
		seq_along(arglist),
		function(i){
			sprintf(
				'%s = %s',
				argnames[[i]],
				arglist[[i]]
			)
		}
	)
}
Example <- setClass(
	'Example',
	slots=c(args='list',comment='character',targetFuncName='character')
)

setGeneric(
	name='toString',
	def=function(obj){
		standardGeneric('toString')
	}
)
setMethod(
	f='toString',
	signature=c('Example'),
	def=function(obj){
		sprintf(
	  	'%s(%s)',
			obj@targetFuncName,
			argstring(obj@args)
		)
	}
)

Examples <- list(	
	Example(
		args=list(
			map=function(t){
				c(
					1+sin(t),
					2+sin(2*t)
				)
			},
			t.start=1,
			t.end=10
		),
		comment='The 14C/C fraction of the intput can be given by function  that is valid in a time interval'
	)
)
argstring(list(a=c(1,3),b=matrix())
#toString(Example(args=list(a=1,b=2)))
#writeExamples <- function(listOfExamples){
#  lapply(listOfExamples,toString)
#}
#writeExamples(Examples)
