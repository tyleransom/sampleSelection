\name{selection-methods}
\alias{selection-methods}
\alias{logLik.selection}
\alias{nobs.selection}
\alias{nObs.selection}
\alias{print.selection}

\title{selection-methods}

\description{
   Methods for selection models
}

\usage{
\method{logLik}{selection}(object, ... )

\method{nobs}{selection}(object, ... )
             
\method{nObs}{selection}(x, ... )
             
\method{print}{selection}( x, digits = max(3, getOption("digits") - 3), ... )
}

\arguments{
   \item{object,x}{object of class \code{selection}.}
   \item{digits}{the minimum number of significant digits of the coefficients
      to be printed.}
   \item{\dots}{further arguments (currently ignored).}
}

\details{
   The \code{logLik} method returns the log likelihood value of the model.
   The \code{nobs} and \code{nObs} methods return the number of observations.
   The \code{print} method prints the call and the estimated coefficients.
   
   Furthermore, some standard methods can be applied to selection models:
   The \code{\link[lmtest]{lrtest}} method can be used to perform
   likelihood-ratio tests.
   The \code{\link[miscTools]{stdEr}} method returns the vector
   of the standard errors of the estimated parameters.
   
   The methods \code{\link{coef.selection}},
   \code{\link{fitted.selection}}
   \code{\link{model.frame.selection}},
   \code{\link{model.matrix.selection}},
   \code{\link{residuals.selection}},
   \code{\link{summary.selection}},
   and \code{\link{vcov.selection}}
   are described at seperate help pages.
}

\author{
   Arne Henningsen
}

\seealso{\code{\link{selection}}, \code{\link{summary.selection}},
   and \code{\link{probit-methods}}.}

\keyword{methods}
