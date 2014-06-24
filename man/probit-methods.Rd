\name{probit-methods}
\alias{probit-methods}
\alias{fitted.probit}
\alias{logLik.probit}
\alias{nobs.probit}
\alias{nObs.probit}
\alias{print.probit}

\title{probit-methods}

\description{
   Methods for probit models
}

\usage{
\method{fitted}{probit}(object, ... )

\method{logLik}{probit}(object, ... )

\method{nobs}{probit}(object, ... )
             
\method{nObs}{probit}(x, ... )
             
\method{print}{probit}( x, digits = max(3, getOption("digits") - 3), ... )
}

\arguments{
   \item{object,x}{object of class \code{probit}.}
   \item{digits}{the minimum number of significant digits of the coefficients
      to be printed.}
   \item{\dots}{further arguments (currently ignored).}
}

\details{
   The \code{fitted} method returns a vector of fitted values (probabilities).
   The \code{logLik} method returns the log likelihood value of the model.
   The \code{nobs} and \code{nObs} methods return the number of observations.
   The \code{print} method prints the call and the estimated coefficients.
   
   Furthermore, some standard methods can be applied to probit models:
   the \code{\link[stats]{coef}} method returns the vector
   of the estimated parameters.
   The \code{\link[stats]{df.residual}} method returns the degrees of freedom
   of the residuals.
   The \code{\link[lmtest]{lrtest}} method can be used to perform
   likelihood-ratio tests.
   The \code{\link[miscTools]{stdEr}} method returns the vector
   of the standard errors of the estimated parameters.
   The \code{\link[stats]{vcov}} method returns the variance covariance matrix
   of the estimated coefficients.
   
   The methods \code{\link{linearPredictors.probit}},
   \code{\link{model.frame.binaryChoice}},
   \code{\link{model.matrix.binaryChoice}},
   \code{\link{residuals.probit}}, and \code{\link{summary.probit}}
   are described at seperate help pages.
}

\author{
   Arne Henningsen
}

\seealso{\code{\link{probit}}, \code{\link{summary.probit}},
   and \code{\link{selection-methods}}.}

\keyword{methods}
