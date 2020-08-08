#' A flux and pool name based description is interesting for models 
#' where the traditional matrix based approach becomes difficult to manage:
#' \enumerate{
#'   \item  For models with many pools the matrix representation makes the source code noisy and difficult to check.
#'   \item  Especially for nonlinear models, where the matrix is not only a function of time but also of the state vector
#'          the latter has to be decomposed in the user code.
#'   \item  Although mathematically equivalent the traditional matrix based representation is more opaque to automatic inspection by R.
#'          As a result it is not possible to automatically resolve the connectivity between the pools, or determine which pools have in/out-fluxes
#'          since for vector and matrix valued functions R can not determine which components are allways zero.
#' }
#' The newer flux and pool name based approach has several advantages:
#' \enumerate{
#'    \item  Instead of the whole matrix (nxn) only the existing fluxes have to be provided.
#'    \item  The fluxfunctions are scalar.
#'    \item  Nonlinear fluxfunctions can be written as functions of the state variable. The correct arguments are mapped automatically.
#'    \item  Since only the existing fluxes are provided the model structure can be found by inspection. 
#'           E.g. connectivity graph can be drawn, which is very helpful top find mistakes in models with many pools.
#' }
