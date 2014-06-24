# based on the code of "fg nu" posted at
# http://stackoverflow.com/questions/14005788/predict-function-for-heckman-model
predict.selection <- function( object, newdata = NULL,
   part = ifelse( type %in% c( "unconditional", "conditional" ),
      "outcome", "selection" ),
   type = "unconditional", ... ) {

   if( ! object$tobitType %in% c( 2, 5 ) ) {
      stop( "internal error: unknown tobitType '", object$tobitType,
         "' Please contact the maintainer of the sampleSelection package" )
   }
   
   if( is.null( newdata ) ) {
      # regressor matrix for the selection equation
      if( part == "selection" || type == "conditional" ) {
         mXSelection <- model.matrix( object, part = "selection" )
      }
      
      # regressor matrix for the outcome equation
      if( part == "outcome" ) {
         mXOutcome <- model.matrix( object, part = "outcome" )
      
         # remove inverse Mills ratio
         if( object$method == "2step" ) {
            if( object$tobitType == 2 ) {
               mXOutcome <- mXOutcome[ , -ncol( mXOutcome ) ]
            } else if( object$tobitType == 5 ) {
               for( i in 1:2 ) {
                  mXOutcome[[i]] <- mXOutcome[[i]][ , -ncol( mXOutcome[[i]] ) ]
               }
            }
         }
      }
      
   } else {
      # regressor matrix for the selection equation
      if( part == "selection" || type == "conditional" ) {
         tempS <- eval( object$call$selection )
         formS <- as.formula( tempS )[-2]
         mfS <- model.frame( formS, data = newdata, na.action = na.pass )
         mXSelection <- model.matrix( formS, mfS )
      }
      
      # regressor matrix for the outcome equation
      if( part == "outcome" ) {
         tempO <- eval( object$call$outcome )
         if( object$tobitType == 2 ) {
            formO <- as.formula( tempO )[-2]
            mfO <- model.frame( formO, data = newdata, na.action = na.pass )
            mXOutcome <- model.matrix( formO, mfO )
         } else if( object$tobitType == 5 ) {
            mXOutcome <- list()
            for( i in 1:2 ) {
               formO <- as.formula( tempO[[i]] )[-2]
               mfO <- model.frame( formO, data = newdata, na.action = na.pass )
               mXOutcome[[i]] <- model.matrix( formO, mfO )
            }
         }
      }
   }
   
   # estimated parameters
   if( part == "selection" || type == "conditional" ) {
      vIndexBetaS <- object$param$index$betaS
      vBetaS <- coef( object )[ vIndexBetaS ]
   }
   
   if( part == "outcome" ) {
      if( object$tobitType == 2 ) {
         vIndexBetaO <- object$param$index$betaO
         vBetaO <- coef( object )[ vIndexBetaO ]
         dLambda <- coef( object )[ "rho" ] * coef( object )[ "sigma" ]
         # remove coefficient of inverse Mills ratio
         if( object$method == "2step" ) {
            vBetaO <- vBetaO[ names( vBetaO ) != "invMillsRatio" ]
         }
      } else if( object$tobitType == 5 ) {
         vIndexBetaO <- list()
         vIndexBetaO[[1]] <- object$param$index$betaO1
         vIndexBetaO[[2]] <- object$param$index$betaO2
         vBetaO <- list()
         dLambda <- list()
         for( i in 1:2 ) {
            vBetaO[[ i ]] <- coef( object )[ vIndexBetaO[[ i ]] ]
            dLambda[[ i ]] <- coef( object )[ paste0( "rho", i ) ] *
               coef( object )[ paste0( "sigma", i ) ]
            # remove coefficient of inverse Mills ratio
            if( object$method == "2step" ) {
               vBetaO[[ i ]] <- vBetaO[[ i ]][
                  names( vBetaO[[ i ]] ) != "invMillsRatio" ]
            }
         }
      }
   }
   
   
   # depending on the type of prediction requested, return
   # TODO allow the return of multiple prediction types
   if( part == "selection" ) {
      if( type == "link" ) { 
         pred <- mXSelection %*% vBetaS
      } else if( type == "response" ) {
         pred <- pnorm( mXSelection %*% vBetaS )
      } else {
         stop( "if argument 'part' is equal to 'selection',",
            " argument 'type' must be either 'link' or 'response'" )
      }
   } else if( part == "outcome" ) {
      if( type == "unconditional" ) {
         if( object$tobitType == 2 ) {
            pred <- mXOutcome %*% vBetaO
         } else if( object$tobitType == 5 ) {
            pred <- NULL
            for( i in 1:2 ) {
               pred <- cbind( pred, mXOutcome[[ i ]] %*% vBetaO[[ i ]] )
            }
            colnames( pred ) <- c( "E[yo1]", "E[yo2]" )
         }
      } else if( type == "conditional" ) {
         linPred <- mXSelection %*% vBetaS
         if( object$tobitType == 2 ) {
            mXOutcome <- mXOutcome[
               rownames( mXOutcome ) %in% rownames( mXSelection ), ]
            mXSelection <- mXSelection[
               rownames( mXSelection ) %in% rownames( mXOutcome ), ]
            pred <- cbind( mXOutcome %*% vBetaO -
                  dLambda * dnorm( linPred ) / pnorm( - linPred ),
               mXOutcome %*% vBetaO +
                  dLambda * dnorm( linPred ) / pnorm( linPred ) )
            colnames( pred ) <- c( "E[yo|ys=0]", "E[yo|ys=1]" )
         } else if( object$tobitType == 5 ) {
            for( i in 1:2 ) {
               mXSelection <- mXSelection[
                  rownames( mXSelection ) %in% rownames( mXOutcome[[i]] ), ]
            }
            for( i in 1:2 ) {
               mXOutcome[[i]] <- mXOutcome[[i]][
                  rownames( mXOutcome[[i]] ) %in% rownames( mXSelection ), ]
            }
            pred <- NULL
            for( i in 1:2 ) {
               pred <- cbind( pred, mXOutcome[[ i ]] %*% vBetaO[[ i ]] -
                     dLambda[[ i ]] * dnorm( linPred ) / pnorm( - linPred ),
                  mXOutcome[[ i ]] %*% vBetaO[[ i ]] +
                     dLambda[[ i ]] * dnorm( linPred ) / pnorm( linPred ) )
            }
            colnames( pred ) <-
               c( "E[yo1|ys=0]", "E[yo1|ys=1]", "E[yo2|ys=0]", "E[yo2|ys=1]" )
         }
      } else {
         stop( "if argument 'part' is equal to 'outcome',",
            " argument 'type' must be either 'unconditional' or 'conditional'" )
      }
   } else {
      stop( "argument 'part' must be either 'selection' or 'outcome'" )
   }

   pred <- drop( pred )

   return( pred )
}
