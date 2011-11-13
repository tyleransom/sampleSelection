linearPredictors <- function( x, ... ) {
    UseMethod("linearPredictors")
}

linearPredictors.probit <- function( x, ... ) {
   mm <- naresid(na.action(x), model.matrix(x))
                           # naresid works (for now).  Should we keep it or replace it?
   mm %*% x$estimate
}
