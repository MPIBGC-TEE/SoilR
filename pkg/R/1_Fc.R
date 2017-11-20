#
# vim:set ff=unix expandtab ts=2 sw=2:

    ### The \eqn{^{14}C}{14C} fraction is a necessary ingredient of any \code{\linkS4class{Model14}} object.
    ### In the most general case it is a real valued function of time, accompanied     
    ### by a string describing the unit or format (i.e. "Delta14C" or "afn" for Absolute Fraction Modern) .
    ### In the most simple case it is constant real number plus format.
setClass( 
    Class="Fc",
    ,
    contains="VIRTUAL"
)
