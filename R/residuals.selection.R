residuals.selection <- function( object, part = "outcome",
      type = "deviance", ... ) {

   if( !( part %in% c( "outcome", "selection" ) ) ) {
      stop( "argument 'part' must be either 'outcome' or 'selection'" )
   }

   # 2-step estimation
   if( object$method == "2step" ) {
      if( part == "selection" ) {
         result <- residuals( object$probit, type = type, ... )
      } else if( part == "outcome" ) {
         response <- model.frame( object$probit )[ , 1 ]
         result <- rep( NA, length( response ) )
         if( object$tobitType == 2 ) {
            result[ response == 1 ] <- residuals( object$lm, ... )
         } else if( object$tobitType == 5 ) {
            result[ response == 0 ] <- residuals( object$lm1, ... )
            result[ response == 1 ] <- residuals( object$lm2, ... )
         } else {
            stop( "unknown tobit type '",  object$tobitType,
               "' in object$tobitType" )
         }
         names( result ) <- row.names( model.frame( object$probit ) )
      } else {
         stop( "argument 'part' must be either 'outcome' or 'selection'" )
      }
   # maximum likelihood estimation
   } else if( object$method == "ml" ) {
      mf <- model.frame( object )
      sResponse <- mf[[
         as.character( formula( object$termsS )[[ 2 ]] ) ]]
      if( part == "selection" ) {
         fitVal <- fitted( object, part = "selection" )
         if( type == "response" ) {
            result <- sResponse - fitVal
         } else if( type == "deviance" ) {
            result <- ifelse( sResponse == 1,
               sqrt( -2 * log( fitVal ) ), -sqrt( -2 * log( 1 - fitVal ) ) )
         } else if( type == "pearson" ) {
            result <- ( sResponse - fitVal ) / sqrt( fitVal * ( 1 - fitVal ) )
         } else {
            stop( "argument 'type' must be either 'deviance', 'pearson',",
               " or 'response'" )
         }
      } else if( part == "outcome" ) {
         if( object$tobitType == 2 ) {
            oResponse <- mf[[
               as.character( formula( object$termsO )[[ 2 ]] ) ]]
         } else if( object$tobitType == 5 ) {
            o1Response <- mf[[
               as.character( formula( object$termsO1 )[[ 2 ]] ) ]]
            o2Response <- mf[[
               as.character( formula( object$termsO2 )[[ 2 ]] ) ]]
            oResponse <- rep( NA, length( sResponse ) )
            oResponse[ sResponse == 0 ] <- o1Response[ sResponse == 0 ]
            oResponse[ sResponse == 1 ] <- o2Response[ sResponse == 1 ]
         } else {
            stop( "unknown tobit type '",  object$tobitType,
               "' in object$tobitType" )
         }
         result <- oResponse - fitted( object )
      } else {
         stop( "argument 'part' must be either 'outcome' or 'selection'" )
      }
      names( result ) <- row.names( mf )
   }

   return( result )
}
