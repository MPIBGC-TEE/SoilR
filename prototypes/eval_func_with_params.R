# example flux func, defined incompletely since k_0 is not set
flux <- function(C){
  k <- k_0*2
  k*C
}
params <- list(k_0=0.4)

# the evaluation of flux(3) fails as expected
# but  
# with(params,flux(3))
# also fails, since the parameters are not made visible INSIDE the body 
# of flux
# To achieve this something like the following is necessarry
param_flux <- function(C,params){with(params,k_0*C)}
print(param_flux(3,params))

# This is cumbersome. 
# It would be nice to be able to define function with free parameters
# and produce a parameterized version automatically
# so that we could say:
# flux_(3,params)) where flux_ = make_param_aware(flux)

make_param_aware <- function(func){
  fn <- func
  code <- deparse(body(func))
  new_code <- c( 'with(params,' ,code ,')')
  body(fn)<-( parse(text=new_code))
  formals(fn)<-c(formals(func),alist(params=))
  fn
}
flux_p  <- make_param_aware(flux)
print(flux_p(3,params))
