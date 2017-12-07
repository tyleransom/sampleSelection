logLik.selection <- function( object, ... ) {
   if( !inherits( object, "maxLik" ) ) {
      stop( "the logLik() method currently only works for models estimated",
         " by maximum likelihood (ML)",
         " but it does not work for models estimated by the 2-step method." )
   }
   result <- NextMethod( object )
   attr( result, "nobs" ) <- nObs( object )
   attr( result, "df" ) <- sum( activePar( object ) )
   class( result ) <- "logLik"
   return( result )
}
