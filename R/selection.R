selection <- function(selection, outcome,
                      data=sys.frame(sys.parent()),
                      weights = NULL,
                      subset,
                      method="ml",
                      type = NULL,
                      start=NULL,
                      boundaries = NULL,
                      ys=FALSE, xs=FALSE,
                      yo=FALSE, xo=FALSE,
                      mfs=FALSE, mfo=FALSE,
                      printLevel=print.level, print.level=0,
                      ...) {
   ## Heckman-style sample-selection models
   ## selection:   formula
   ##              LHS: must be convertable to two-level factor (e.g. 0-1, 1-2, "A"-"B")
   ##              RHS: ordinary formula as in lm()
   ## type         model type:
   ##          2: tobit 2
   ##          5: tobit 5
   ##          treatment: spherical treatment model
   ##          NULL: guess the correct type
   ## outcome:     formula, or a list of two formulas.
   ##              If the outcome contains one formula, it is
   ##              assumed that we observe the formula only if selection equals to the first level
   ##              of selection (e.g. "0", "1" or "A" in the examples above).
   ##              If the outcome contains two formulas, we assume that the first one is observed if
   ##              selection equals to the first level, otherwise the second formula.
   ## ys, xs, yo, xo, mfs, mfo: whether to return the response, model matrix or
   ##              the model frame of outcome and selection equation(s)
   ## First the consistency checks
   ## ...          additional arguments for tobit2fit and tobit5fit
   ##
   if(is.null(type)) {
      type <- detectModelType(selection, outcome)
   }
   checkModelType(type, selection, outcome)
   if(length(outcome) == 1) {
      outcome <- outcome[[1]]
   }
   if(printLevel > 0)
       cat("Tobit", type, "model\n")
   if(!missing(data)) {
      if(!inherits(data, "environment") & !inherits(data, "data.frame") & !inherits(data, "list")) {
         stop("'data' must be either environment, data.frame, or list (currently a ", class(data), ")")
      }
   }
   ## probitEndogenous <- model.frame( selection, data = data)[ , 1 ]
   ## probitLevels <- levels( as.factor( probitEndogenous ) )
   ## if( length( probitLevels ) != 2 ) {
   ##    stop( "the left hand side of 'selection' has to contain",
   ##       " exactly two levels (e.g. FALSE and TRUE)" )
   ## }
   
   if( !is.null( weights ) && type != 2 ) {
      warning( "argument 'weights' is ignored in type-", type, " models" )
      weights <- NULL
   }
   
   # data$probitDummy <- probitEndogenous == probitLevels[ 2 ]
   ## now check whether two-step method was requested
   cl <- match.call()
   if(method == "2step") {
      if( !is.null( boundaries ) ) {
         stop( "2-step estimation of a model with an outcome equation",
            " that has an interval-variable as dependent variable",
            " has not yet been implemented" )
      }
      if(type == 2) {
          twoStep <- heckit2fit(selection, outcome, data=data,
            weights = weights, printLevel = printLevel, ... )
      } else if(type == 5) {
          twoStep <- heckit5fit(selection, outcome, data=data,
            printLevel = printLevel, ... )
      } else {
          stop("unknown type")
      }
      twoStep$call <- cl
      class(twoStep) <- c("selection", class(twoStep))
      return(twoStep)
   }
   ## Now extract model frames etc
   ## YS (selection equation)
   mf <- match.call(expand.dots = FALSE)
   m <- match(c("selection", "data", "subset"), names(mf), 0)
   mfS <- mf[c(1, m)]
   mfS$drop.unused.levels <- TRUE
   mfS$na.action <- na.pass
   mfS[[1]] <- as.name("model.frame")
   names(mfS)[2] <- "formula"
                                        # model.frame requires the parameter to
                                        # be 'formula'
   mfS <- eval(mfS, parent.frame())
   mtS <- terms(mfS)
   XS <- model.matrix(mtS, mfS)
   YS <- model.response(mfS)
   YSLevels <- levels( as.factor( YS ) )
                           # Here we might test if the selection
                           # outcome is a binary variable.  However,
                           # we do it later to allow model.frame()
                           # to work with only selected/unselected
                           # data for prediction purposes.
   ## if( length( YSLevels ) != 2 ) {
   ##    stop( "the left hand side of the 'selection' formula has to contain",
   ##       " exactly two levels (e.g. FALSE and TRUE)" )
   ## }
   YS <- as.integer(YS == tail(YSLevels, 1))
                           # selection will be kept as integer
                           # internally
   ## check for NA-s.  Because we have to find NA-s in several frames, we cannot use the standard na.
   ## functions here.  Find bad rows and remove them later.
   ## We check XS and YS separately, because mfS may be a data frame with complex structure (e.g.
   ## including matrices)
   badRow <- is.na(YS) | is.infinite(YS)
   badRow <- badRow | apply(XS, 1, function(v)
      any(is.na(v) | is.infinite(v)))
   ## YO (outcome equation)
   ## Here we should include a possibility for the user to
   ## specify the model.  Currently just a guess.
   if(type %in% c(2, "treatment")) {
      oArg <- match("outcome", names(mf), 0)
                                        # find the outcome argument
      m <- match(c("outcome", "data", "subset",
                   "offset"), names(mf), 0)
      ## replace the outcome list by the first equation and evaluate it
      mfO <- mf[c(1, m)]
      if( is.null( boundaries ) ) {
         mfO$drop.unused.levels <- TRUE
      }
      mfO$na.action <- na.pass
      mfO[[1]] <- as.name("model.frame")
                                        # eval it as model frame
      names(mfO)[2] <- "formula"
      mfO <- eval(mfO, parent.frame())
                                        # Note: if unobserved variables are
                                        # marked as NA, eval returns a
                                        # subframe of visible variables only.
                                        # We have to check it later
      mtO <- attr(mfO, "terms")
      XO <- model.matrix(mtO, mfO)
      YO <- model.response(mfO)
      if( !is.null( boundaries ) ) {
         outcomeVar <- "interval"
      } else if(is.logical(YO) |
            (is.factor(YO) & length(levels(YO)) == 2)) {
         outcomeVar <- "binary"
      } else {
         outcomeVar <- "continuous"
      }
      if(type == 2) {
         badRow <- badRow | ((is.na(YO) | is.infinite(YO))
                             & (!is.na(YS) & YS == 1))
         badRow <- badRow | (apply(XO, 1, function(v)
            any(is.na(v) | is.infinite(v))) & (!is.na(YS) & YS == 1))
                           # rows in outcome, which contain NA and are observable -> bad too
                           # YO unobserved but should be observed
                           # for tobit-2
      } else if( type == "treatment" ) {
         badRow <- badRow | is.na(YO) | is.infinite(YO)
         badRow <- badRow | apply(XO, 1, function(v)
            any(is.na(v) | is.infinite(v)))
                           # YO unobserved in any case
                           # for treatment models
      } else {
         stop( "Internal error ('badRow'). Please contact the maintainer",
            " of this package" )
      }

      if( !is.null( weights ) ) {
         if( length( weights ) != length( badRow ) ) {
            stop( "number of weights (", length( weights ), ") is not equal",
               " to the number of observations (", length( badRow ), ")" )
         }
         badRow <- badRow | is.na( weights )
      }   
      
      if(printLevel > 0) {
         cat(sum(badRow), "invalid observations\n")
      }
      if( method == "model.frame" ) {
         mf <- mfS
         mf <- cbind( mf, mfO[ , !(names( mfO ) %in% names( mf )), drop = FALSE ])
         return( mf[ !badRow, ] )
      }
      if( length( YSLevels ) != 2 ) {
         stop( "the left hand side of the 'selection' formula\n",
              "has to contain",
              " exactly two levels (e.g. FALSE and TRUE)" )
      }
      XS <- XS[!badRow,, drop=FALSE]
      YS <- YS[!badRow]
      XO <- XO[!badRow,, drop=FALSE]
      YO <- YO[!badRow]
      weightsNoNA <- weights[ !badRow ]
      if(type == 2) {
         YO[ YS == 0 ] <- NA
         XO[ YS == 0, ] <- NA
      }
      NXS <- ncol(XS)
      NXO <- ncol(XO)
      iGamma <- 1:NXS
      iBeta <- max(iGamma) + seq(length=NXO)
      if( outcomeVar %in% c( "continuous", "interval" ) ) {
         iSigma <- max(iBeta) + 1
         iRho <- max(iSigma) + 1
      } else if( outcomeVar == "binary" ) {
          iRho <- max(iBeta) + 1
      } else {
         stop( "Internal error ('iRho'). Please contact the maintainer",
            " of this package" )
      }
      nParam <- iRho
      twoStep <- NULL
      if(is.null(start) & is.null( boundaries ) ) {
                           # start values by Heckman 2-step method
         start <- numeric(nParam)
         if(type == 2) {
            twoStep <- heckit2fit(selection, outcome, data=data,
                                  printLevel = printLevel,
                                  weights = weights )
         } else if( type == "treatment" ) {
            twoStep <- heckitTfit(selection, outcome, data=data,
                                  printLevel = printLevel,
                                  weights = weights )
         } else {
            stop( "Internal error ('start'). Please contact the maintainer",
               " of this package" )
         }
         coefs <- coef(twoStep, part="full")
         start[iGamma] <- coefs[twoStep$param$index$betaS]
         if( outcomeVar == "continuous" ) {
            start[iBeta] <- coefs[twoStep$param$index$betaO]
            start[iSigma] <- coefs[twoStep$param$index$sigma]
         } else if( outcomeVar == "binary" ) {
             start[iBeta] <- coefs[twoStep$param$index$betaO]/coefs[twoStep$param$index$sigma]
         } else {
            stop( "Internal error ('start-2'). Please contact the maintainer",
               " of this package" )
         }
         start[iRho] <- coefs[twoStep$param$index$rho]
         if(start[iRho] > 0.99) {
             start[iRho] <- 0.99
         } else if(start[iRho] < -0.99) {
             start[iRho] <- -0.99
         }
      }
      if(is.null(names(start))) {
         # add names to start values if not present
         if( outcomeVar == "continuous" ) {
            names(start) <- c(colnames(XS), colnames(XO), "sigma",
                              "rho")
         } else if( outcomeVar == "binary" ) {
            names(start) <- c(colnames(XS), colnames(XO), 
                              "rho")
         } else if( outcomeVar != "interval" ) {
            stop( "Internal error ('names'). Please contact the maintainer",
               " of this package" )
         }
      }
      if(type == 2) {
         if( outcomeVar == "continuous" ) {
            estimation <- tobit2fit(YS, XS, YO, XO, start, weights = weightsNoNA,
                                    printLevel=printLevel, ...)
            iErrTerms <- c(sigma=iSigma, rho=iRho )
         } else if( outcomeVar == "interval" ) {
            estimation <- tobit2Intfit(YS, XS, YO, XO,
               boundaries = boundaries, start = start,
               weights = weightsNoNA,
               printLevel = printLevel, ... )
            if( isTRUE( cl$returnLogLikStart ) ) {
               return( estimation )
            }
            start <- estimation$start
            iErrTerms <- c( logSigma = iSigma, atanhRho = iRho,
               sigma = iRho + 1, sigmaSq = iRho + 2, rho = iRho + 3 )
         } else if( outcomeVar == "binary" ) {
            estimation <- tobit2Bfit(YS, XS, YO, XO, start, weights = weightsNoNA,
                                     printLevel=printLevel, ...)
            iErrTerms <- c(rho=iRho)
         } else {
            stop( "Internal error ('est-1'). Please contact the maintainer",
               " of this package" )
         }
      } else if( type == "treatment" ) {
         if( outcomeVar == "continuous" ) {
            estimation <- tobitTfit(YS, XS, YO, XO, start, weights = weightsNoNA,
                                    printLevel=printLevel, ...)
            iErrTerms <- c(sigma=iSigma, rho=iRho )
         } else {
            stop("treatment effect models are only implemented",
               " for continuous dependent variables",
               " but not for ", outcomeVar, " dependent variables" )
         }
      } else {
         stop( "Internal error ('est-2'). Please contact the maintainer",
            " of this package" )
      }
      param <- list(index=list(betaS=iGamma,
                    betaO=iBeta,
                    errTerms=iErrTerms,
                    outcome = iBeta ),
                    NXS=ncol(XS), NXO=ncol(XO),
                    N0=sum(YS==0), N1=sum(YS==1),
                    nObs=length(YS), nParam=length(start),
                    df=length(YS) - length(start),
                    levels=YSLevels
                           # levels[1]: selection 1; levels[2]: selection 2
                    )
   } else if(type == 5) {
      if( !is.null( boundaries ) ) {
         stop( "estimation of tobit-5 models with an outcome equation",
            " that has an interval-variable as dependent variable",
            " has not yet been implemented" )
      }
      ## extract the outcome formulas.  Anyone able to explain why do we need to do the complicated stuff?
      oArg <- match("outcome", names(mf), 0)
                                        # find the outcome argument
      # ocome <- as.list(mf[[oArg]])
      # formula1 <- ocome[[2]]
      # formula2 <- ocome[[3]]
                                        # If the formulas are not written explicitly but given as variables, 'formula*' are
                                        # the corresponding variable names and we have to extract the formulas in a different way:
      # if(!("formula" %in% class(formula1)))
          formula1 <- outcome[[1]]
      # if(!("formula" %in% class(formula2)))
          formula2 <- outcome[[2]]
                                        # Now we have extracted both formulas
      m <- match(c("outcome", "data", "subset",
                   "offset"), names(mf), 0)
      ## replace the outcome list by the first equation and evaluate it
      mf[[oArg]] <- formula1
      mf1 <- mf[c(1, m)]
      mf1$drop.unused.levels <- TRUE
      mf1$na.action = na.pass
      mf1[[1]] <- as.name("model.frame")
                                        # eval it as model frame
      names(mf1)[2] <- "formula"
      mf1 <- eval(mf1, parent.frame())
      mtO1 <- attr(mf1, "terms")
      XO1 <- model.matrix(mtO1, mf1)
      YO1 <- model.response(mf1, "numeric")
      badRow <- badRow | (is.na(YO1) & (!is.na(YS) & YS == 0))
      badRow <- badRow | (apply(XO1, 1, function(v) any(is.na(v))) & (!is.na(YS) & YS == 0))
      ## repeat all the stuff with second equation
      mf[[oArg]] <- formula2
      mf2 <- mf[c(1, m)]
      mf2$drop.unused.levels <- TRUE
      mf2$na.action <- na.pass
      mf2[[1]] <- as.name("model.frame")
                                        # eval it as model frame
      names(mf2)[2] <- "formula"
      mf2 <- eval(mf2, parent.frame())
      mtO2 <- attr(mf2, "terms")
      XO2 <- model.matrix(mtO2, mf2)
      YO2 <- model.response(mf2, "numeric")
      badRow <- badRow | (is.na(YO2) & (!is.na(YS) & YS == 1))
      badRow <- badRow | (apply(XO2, 1, function(v) any(is.na(v))) & (!is.na(YS) & YS == 1))
      if( ( is.logical(YO1) | ( is.factor(YO1) & length(levels(YO1)) == 2 ) ) &
            ( is.logical(YO2) | ( is.factor(YO2) & length(levels(YO2)) == 2  ) ) ){
         outcomeVar <- "binary"
      } else {
         outcomeVar <- "continuous"
      }
      if( method == "model.frame" ) {
         mf <- mfS
         mf <- cbind( mf, mf1[ , ! names( mf1 ) %in% names( mf ), drop = FALSE ] )
         mf <- cbind( mf, mf2[ , ! names( mf2 ) %in% names( mf ), drop = FALSE ] )
         return( mf[ !badRow, ] )
      }
      if( length( YSLevels ) != 2 ) {
         stop( "the left hand side of the 'selection' formula\n",
              "has to contain",
              " exactly two levels (e.g. FALSE and TRUE)" )
      }
      ## indices in for the parameter vector.  These are returned in order to provide the user a way
      ## to extract certain components from the coefficients
      NXS <- ncol(XS)
      NXO1 <- ncol(XO1)
      NXO2 <- ncol(XO2)
      XS <- XS[!badRow,, drop=FALSE]
      YS <- YS[!badRow]
      XO1 <- XO1[!badRow,, drop=FALSE]
      YO1 <- YO1[!badRow]
      XO2 <- XO2[!badRow,, drop=FALSE]
      YO2 <- YO2[!badRow]
      YO1[ YS == 1 ] <- NA
      YO2[ YS == 0 ] <- NA
      XO1[ YS == 1, ] <- NA
      XO2[ YS == 0, ] <- NA
      iBetaS <- 1:NXS
      iBetaO1 <- seq(tail(iBetaS, 1)+1, length=NXO1)
      iSigma1 <- tail(iBetaO1, 1) + 1
      iRho1 <- tail(iSigma1, 1) + 1
      iBetaO2 <- seq(tail(iRho1, 1) + 1, length=NXO2)
      iSigma2 <- tail(iBetaO2, 1) + 1
      iRho2 <- tail(iSigma2, 1) + 1
      nParam <- iRho2
      twoStep <- NULL
      if(is.null(start)) {
         start <- numeric(nParam)
         if(printLevel > 0) {
            cat("Start values by Heckman 2-step method (", nParam, " componenets)\n", sep="")
         }
         twoStep <- heckit5fit(selection, as.formula(formula1), as.formula(formula2),
                           data=data, printLevel = printLevel, ... )
         ind <- twoStep$param$index
         start <- coef(twoStep, part="full")[c(ind$betaS,
                                  ind$betaO1, ind$sigma1, ind$rho1,
                                  ind$betaO2, ind$sigma2, ind$rho2)]
         names( start ) <- sub( "^[SO][12]?:", "", names( start ) )
      }
      if(is.null(names(start)))
          names(start) <- c(colnames(XS), colnames(XO1), "sigma1", "rho1", colnames(XO2), "sigma2", "rho2")
                                        # add names to start values if not present
      estimation <- tobit5fit(YS, XS, YO1, XO1, YO2, XO2, start=start,
                              printLevel=printLevel, ...)
      param <- list(index=list(betaS=iBetaS,
                    betaO1=iBetaO1, sigma1=iSigma1, rho1=iRho1,
                    betaO2=iBetaO2, sigma2=iSigma2, rho2=iRho2,
                    errTerms = c( iSigma1, iSigma2, iRho1, iRho2 ),
                    outcome = c( iBetaO1, iBetaO2 ) ),
                    NXS=ncol(XS),
                    NXO1=ncol(XO1), NXO2=ncol(XO2),
                    N1=sum(YS==0), N2=sum(YS==1),
                    nObs=length(YS), nParam=length(start),
                    df=length(YS) - length(start),
                    levels=YSLevels
                           # levels[1]: selection 1; levels[2]: selection 2
                    )
   } else {
      stop("Unknown model type: '", type, "'")
   }
   ## now add the additional parameters into the resulting
   ## structure
   result <- c(estimation,
               twoStep=list(twoStep),
               start=list(start),
               param=list(param),
               call=cl,
               termsS=mtS,
               termsO=switch(as.character(type),
                             "treatment"=, "2"=mtO,
                             "5"=list(mtO1, mtO2),
                             "0"=NULL),
               ys=switch(as.character(ys), "TRUE"=list(YS), "FALSE"=NULL),
               xs=switch(as.character(xs), "TRUE"=list(XS), "FALSE"=NULL),
               yo=switch(as.character(yo),
                  "TRUE"=switch(as.character(type),
                     "2"=, "treatment"=list(YO),
                     "5"=list(YO1, YO2)),
                  "FALSE"=NULL),
               xo=switch(as.character(xo),
                  "TRUE"=switch(as.character(type),
                     "2"=, "teatment"=list(XO),
                     "5"=list(XO1, XO2)),
                  "FALSE"=NULL),
               mfs=switch(as.character(mfs), "TRUE"=list(mfS[!badRow,]), "FALSE"=NULL),
               mfo=switch(as.character(mfo),
                  "TRUE"=switch(as.character(type),
                     "2"=, "treatment"=list(mfO[!badRow,]),
                     "5"=list(mf1[!badRow,], mf2[!badRow,]),
                  "FALSE"=NULL))
               )

   result$outcomeVar <- outcomeVar

   class( result ) <- class( estimation ) 
   return(result)
}
