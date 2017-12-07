tobit2Intfit <- function(YS, XS, YO, XO, boundaries, start = "ml",
                      weights = NULL, printLevel = 0, returnLogLikStart = FALSE,
                      maxMethod = "BHHH",
                      ...) {
   ## Fit intervaL regression model with sample selection
   ## The model is as follows:
   ## 
   ## The latent variables are:
   ## YS* = XS'BS + u1
   ## YO* = XO'BO + u2
   ## 
   ## The observables are:
   ##      / 0  if  YS* <= 0
   ## YS = \ 1  if  YS* > 0
   ## 
   ##      / NA  if  YS = 0
   ##      | 1   if  a1 < YO* <= a2 & YS = 1
   ## YO = | 2   if  a2 < YO* <= a3 & YS = 1
   ##      | ... 
   ##      \ M   if  a(M) < YO* <= a(M+1) & YS = 1
   ## 
   ##  M       number of intervals
   ##
   ##  YS      binary or logical vector, 0 (FALSE) corresponds to
   ##          YO not observed, 1 (TRUE) if observed
   ##  XS      matrix of explanatory variables for selection equation,
   ##          should include exclusion restriction
   ##  YO      outcome vector: vector of integers with values between 1 and M
   ##  XS      matrix of explanatory variables for outcomes
   ##  ...     additional parameters for maxLik
   ##  
   ## Result:
   ## Object of class 'selection', derived from 'maxLik'.
   ## Includes all the components of maxLik and additionally
   ## ...
   ## 
   loglik <- function( beta) {
      betaS <- beta[ibetaS]
      betaO <- beta[ibetaO]
      sigma <- exp(beta[iSigma])
      rho <- tanh(beta[iRho])
      if( ( rho < -1) || ( rho > 1)) return(NA)
      vcovMat <- matrix(c(1,-rho,-rho,1), 2, 2)
      XS.b <- drop(XS %*% betaS)
      XO.b <- drop(XO %*% betaO)
      # pre-compute the difference between the CDF of the bivariate normal
      # distribution for the interval between the boundaries
      pmvnDiff <- rep( NA, nObs )
      for( i in which(YS==1) ) {
         pmvnDiff[i] <-
            pmvnorm( upper = c( ( boundaries[ YO[i] + 1 ] - XO.b[i] ) / sigma,
               XS.b[i] ), sigma = vcovMat ) -
            pmvnorm( upper = c( ( boundaries[ YO[i] ] - XO.b[i] ) / sigma,
               XS.b[i] ), sigma = vcovMat )
      }
      pmvnDiff <- pmax( pmvnDiff, .Machine$double.eps )
      loglik <- rep( NA, nObs )
      ## YS == 0, YO == NA
      loglik[YS==0] <- pnorm( -XS.b[YS==0], log.p = TRUE )
      ## YS == 1
      loglik[YS==1] <- log( pmvnDiff[YS==1] )

      ## --- gradient ---
      grad <- matrix(0, nObs, nParam)
      
      # pre-compute the difference between the PDF of the bivariate normal
      # distribution for the interval between the boundaries
      dmvnDiff <- rep( NA, nObs )
      for( i in which(YS==1) ) {
         dmvnDiff[i] <-
            dmvnorm( x = c( ( boundaries[ YO[i] ] - XO.b[i] ) / sigma,
               XS.b[i] ), sigma = vcovMat ) - 
            dmvnorm( x = c( ( boundaries[ YO[i] + 1 ] - XO.b[i] ) / sigma,
               XS.b[i] ), sigma = vcovMat )
      }
      # gradients for the parameters for selection into policy (betaS)
      grad[YS==0, ibetaS] <-
         - dnorm( -XS.b[YS==0] ) * XS[YS==0, ] / pnorm( -XS.b[YS==0] )
      grad[YS==1, ibetaS] <- (
         pnorm( ( ( boundaries[ YO[YS==1] + 1 ] - XO.b[YS==1] ) / sigma
            + rho * XS.b[YS==1] ) / sqrt( 1 - rho^2 ) ) -
         pnorm( ( ( boundaries[ YO[YS==1] ] - XO.b[YS==1] ) / sigma
            + rho * XS.b[YS==1] ) / sqrt( 1 - rho^2 ) ) ) *
         dnorm( XS.b[YS==1] ) * XS[ YS==1, ] /
         pmvnDiff[YS==1]

      # gradients for the parameters for the outcome (betaO)
      grad[YS==1, ibetaO] <- (
         pnorm( ( XS.b[YS==1]
            + rho * ( ( boundaries[ YO[YS==1] + 1 ] - XO.b[YS==1] ) / sigma )
            ) / sqrt( 1 - rho^2 ) ) *
         dnorm( ( boundaries[ YO[YS==1] + 1 ] - XO.b[YS==1] ) / sigma ) - 
         pnorm( ( XS.b[YS==1]
            + rho * ( ( boundaries[ YO[YS==1] ] - XO.b[YS==1] ) / sigma )
            ) / sqrt( 1 - rho^2 ) ) *
         dnorm( ( boundaries[ YO[YS==1] ] - XO.b[YS==1] ) / sigma ) ) *
         ( -XO[ YS==1, ] / sigma ) /
         pmvnDiff[YS==1]

      # gradient for the standard deviation (sigma)
      grad[YS==1, iSigma] <- (
         ifelse( is.infinite( boundaries[ YO[YS==1] + 1 ] ), 0,
            pnorm( ( XS.b[YS==1] + rho *
               ( ( boundaries[ YO[YS==1] + 1 ] - XO.b[YS==1] ) / sigma ) ) /
                  sqrt( 1 - rho^2 ) ) *
            dnorm( ( boundaries[ YO[YS==1] + 1 ] - XO.b[YS==1] ) / sigma ) *
            ( ( XO.b[YS==1] - boundaries[ YO[YS==1] + 1 ] ) / sigma^2 ) ) -
         ifelse( is.infinite( boundaries[ YO[YS==1] ] ), 0,
            pnorm( ( XS.b[YS==1] + rho *
               ( ( boundaries[ YO[YS==1] ] - XO.b[YS==1] ) / sigma ) ) /
                  sqrt( 1 - rho^2 ) ) *
            dnorm( ( boundaries[ YO[YS==1] ] - XO.b[YS==1] ) / sigma ) *
            ( ( XO.b[YS==1] - boundaries[ YO[YS==1] ] ) / sigma^2 ) ) ) * 
         sigma / ( pmvnDiff[YS==1] )
      
      # gradient for the correlation parameter (rho)
      grad[YS==1, iRho] <- ( dmvnDiff[YS==1] * (1 - rho^2) ) / pmvnDiff[YS==1]  
      
      attr(loglik, "gradient") <- grad

      return(loglik)
   } 
   

   gradlik <- function(x) {
      l <- loglik(x)
      return(attr(l, "gradient"))
   }
   
   YOorig <- YO
   YO <- as.integer( YO )
   if( min( YO[YS==1] ) <= 0 ) {
      stop( "YO should only have strictly positive integer values" )
   }
   if( 0 %in% table(YO)) {
      stop( "At least one intervals does not contain observations")
   }
   nInterval <- max( YO[YS==1] )
   if( length( boundaries ) != nInterval + 1 ) {
      stop( "argument 'boundaries' must have (number of intervals + 1 = ", 
         nInterval + 1, ") elements but it has ", length( boundaries ),
         " elements" )
   }
   if( !all( sort( boundaries ) == boundaries ) ) {
      stop( "the boundaries in the vector definded by argument 'boundaries' ",
         "must be in ascending order" )
   }
   if( is.factor( YOorig ) ){
      intervals <- data.frame( YO = levels( YOorig ) )
   } else {
      intervals <- data.frame( YO = 1:nInterval )
   }
   intervals$lower <- boundaries[ - length(boundaries) ]
   intervals$upper <- boundaries[-1]
   intervals$count <- sapply( c( 1:nInterval ),
      function(x) sum( YO[ YS == 1 ] == x, na.rm = TRUE ) )
   if( printLevel >= 1 ) {
      print( intervals )
   }
   
   ## If no starting values for the parameters are given, 2-step Heckman is
   ## estimated with first stage probit and second stage OLS on interval 
   ## midpoints
   # Calculating Interval midpoints

   if( is.null( start ) ) {
      start <- "ml"
   }
   if( is.numeric(start) ){
      if( length(start) != (ncol(XS) + ncol(XO) + 2) ) {
      stop( "The vector of starting values has an incorrect length.", 
         " Number of parameters: ", ncol(XS) + ncol(XO) + 2,
         ". Length of provided vector: ", length( start ) )
      }
      startVal <- start
   } else if( start %in% c( "ml", "2step" ) ) {
      intMeans <- ( intervals$lower + intervals$upper ) / 2
      
      # For infinite boundaries we use mean interval width as value
      intWidths <- intervals$upper - intervals$lower
      meanWidth <- mean( intWidths[ is.finite( intWidths ) ] )
      negInf <- is.infinite( intMeans ) & intMeans < 0
      if( any( negInf ) ) {
         intMeans[ negInf ] <- intervals$upper[ negInf ] - meanWidth
      }
      posInf <- is.infinite( intMeans ) & intMeans > 0
      if( any( posInf ) ) {
         intMeans[ posInf ] <- intervals$lower[ posInf ] + meanWidth
      }
      yMean <- intMeans[YO]

      # estimation as a normal tobit-2 model (either by ML or the 2-step method)
      Est <- heckit( YS ~ XS - 1, yMean ~ XO - 1, method = start )
      # Extracting starting values
      startVal <- as.numeric(coef(Est))
      if(start == "2step") {
         startVal <- startVal[ - ( length( startVal ) - 2 ) ]
      }
      startVal[length(startVal)-1] <- log(startVal[length(startVal)-1]^2)
      startVal[length(startVal)] <- atanh(startVal[length(startVal)])
   } else {
      stop( "argument 'start' must be \"ml\", \"2step\", or",
         " a numeric vector" )
   }
   
   ## ---------------
   NXS <- ncol( XS )
   if(is.null(colnames(XS))) {
      colnames(XS) <- paste0( rep( "XS", NXS ),
         seq( from = 1, length.out = NXS ) )
   }
   NXO <- ncol( XO )
   if(is.null(colnames(XO))) {
      colnames(XO) <- paste0( rep( "XO", NXO ),
         seq( from = 1, length.out = NXO ) )
   }
   nObs <- length( YS )
   NO <- length( YS[YS > 0] )
   
   ## parameter indices
   ibetaS <- seq( from = 1, length.out = NXS )
   ibetaO <- seq( from = NXS+1, length.out = NXO )
   iSigma <- NXS + NXO + 1
   iRho <- NXS + NXO + 2
   nParam <- iRho
   
   # names of parameters (through their starting values)
   names( startVal ) <-
      c( colnames( XS ), colnames( XO ), "logSigma", "atanhRho" )
   
   # weights
   if( !is.null( weights ) ) {
      stop( "weights have not been implemented yet. Sorry!" )
   }
   
   ## output, if asked for it
   if( printLevel > 0) {
      cat("YO observed:", NO, "times; not observed:", nObs - NO,
          "times:\n")
      cat( "Number of intervals: ", nInterval, "\n" )
      print(table(YS, YO, exclude=NULL))
      cat( "Boundaries:\n")
      print(boundaries)
      cat( "Initial values:\n")
      print(startVal)
   }
   if( printLevel > 1) {
      cat( "Log-likelihood value at initial values:\n")
      print(loglik(startVal))
   }
   
   # browser()
   # # check if the likelihood values of all possible outcomes sum up to one
   # YS[] <- 0; ll0 <- loglik( startVal )
   # YS[] <- 1; YO[] <- 1; ll11 <- loglik( startVal )
   # YS[] <- 1; YO[] <- 2; ll12 <- loglik( startVal )
   # YS[] <- 1; YO[] <- 3; ll13 <- loglik( startVal )
   # all.equal( exp(ll0) + exp(ll11) + exp(ll12) + exp(ll13), rep( 1, nObs ) )
   # # check analytical derivatives
   # compareDerivatives(loglik, gradlik, t0=startVal )
   # range(numericGradient(loglik, t0=startVal)-gradlik(startVal))
   
   if( returnLogLikStart ) {
      return( loglik( startVal ) )
   }
   
   ## estimate
   result <- maxLik(loglik, 
                    start=startVal,
                    method=maxMethod,
                    print.level = printLevel, ... )
   if( result$code == 100 ) {
      stop( "maxLik: Return code 100: Initial value out of range" ) 
   }
   result$tobitType <- 2
   result$method <- "ml"
   result$start <- startVal
   result$intervals <- intervals
   
   # Calculating sigma, sigmaSq, and rho with standard errors
   result$coefAll <- c( result$estimate,
      sigma = unname( exp( result$estimate[ "logSigma" ] ) ),
      sigmaSq = unname( exp( 2 * result$estimate[ "logSigma" ] ) ),
      rho = unname( tanh( result$estimate[ "atanhRho" ] ) ) )
   
   jac <- cbind( diag( length( result$estimate ) ),
      matrix( 0, length( result$estimate ), 3 ) )
   rownames( jac ) <- names( result$estimate )
   colnames( jac ) <- c( names( result$estimate ), "sigma", "sigmaSq", "rho" )
   jac[ "logSigma", "sigma" ] <- exp( result$estimate[ "logSigma" ] )
   jac[ "logSigma", "sigmaSq" ] <- 2 * exp( 2 * result$estimate[ "logSigma" ] )
   jac[ "atanhRho", "rho" ] <- 1 - ( tanh( result$estimate[ "atanhRho" ] ) )^2
   result$vcovAll <- t( jac ) %*% vcov( result ) %*% jac

   class( result ) <- c( "selection", class( result ) )
   return( result )
}
