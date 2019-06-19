#
# vim:set ff=unix expandtab ts=2 sw=2:
# As long as TimeMap is still supported in legacy user code
# a conversion to the new Classes must be possible and tested
test.castTimeMap2BoundLinDecompOp <- function(){
	tm=TimeMap.new(0,1,function(t){t})
	BoundLinDecompOp(tm)
}
