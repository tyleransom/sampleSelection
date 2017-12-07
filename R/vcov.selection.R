vcov.selection <- function( object, part = "full", ... ) {

   if( !( part %in% c( "full", "outcome", "est" ) ) ) {
      stop( "argument 'part' must be either 'full', 'outcome', or 'est'" )
   }

   if( part == "full" & !is.null( object$vcovAll ) ){
      result <- object$vcovAll
   } else if( object$method == "ml" ){
      result <- NextMethod( "vcov", object, ...)
   } else if( object$method == "2step" ) {
      result <- object$vcov
   }

   if( part == "outcome" ) {
      result <- result[ object$param$index$outcome, object$param$index$outcome ]
   }

   return( result )
}
