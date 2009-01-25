\name{coef.probit}
\alias{coef.probit}
\alias{coef.summary.probit}

\title{Extract Coefficients from Probit Models}

\description{
  These methods extract coefficients from probit models
}

\usage{
\method{coef}{probit}( object, ... )
\method{coef}{summary.probit}( object, ... )
}

\arguments{
  \item{object}{object of class \code{probit}
    or \code{summary.probit}.}
  \item{\dots}{currently not used.}
}
\value{
   \code{coef.probit} returns a vector of the estimated coefficients.

   \code{coef.summary.probit} returns a matrix of the estimated coefficients,
      their standard errors, t-values, and p-values.
}

\author{Arne Henningsen}

\seealso{\code{\link{coefficients}}, \code{\link{probit}}, and
   \code{\link{summary.probit}}}

\keyword{methods}
