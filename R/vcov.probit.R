## probit
vcov.probit <- function(object, ...) {
  result <- maxLik:::vcov.maxLik( object )
  if(!is.null(result))
      rownames( result ) <- colnames( result ) <- names( object$estimate )
  return( result )
}
