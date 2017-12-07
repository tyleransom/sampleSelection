coef.selection <- function( object, part="full", ... ) {
   if( !( part %in% c( "full", "outcome", "est" ) ) ) {
      stop( "argument 'part' must be either 'full', 'outcome', 'est'" )
   }
   if( part == "full" & !is.null( object$coefAll ) ) {
      coefValues <- object$coefAll
   } else if("maxLik" %in% class(object)) {
      coefValues <- NextMethod( "coef", object, ...)
   } else {
       coefValues <- object$coefficients
   }
   if( part == "outcome" ) {
      coefValues <- coefValues[ object$param$index$outcome ]
   } else {
      attributes( coefValues )$index <- object$param$index
   }
   attributes( coefValues )$part <- part
   attributes( coefValues )$tobitType <- object$tobitType
   class( coefValues ) <- c( "coef.selection", class(coefValues) )
   return( coefValues )
}
