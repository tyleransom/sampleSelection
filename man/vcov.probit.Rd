\name{vcov.probit}
\alias{vcov.probit}

\title{Variance Covariance Matrix of probit models}
\description{
   Extract variance-covariance matrices  of \code{\link{probit}} models.
}
\usage{
   \method{vcov}{probit}( object, ... )
}
\arguments{
   \item{object}{an object of class \code{probit} or \code{maxLik}.}
   \item{\dots}{further arguments (currently ignored).}
}
\value{
   the estimated variance covariance matrix of the coefficients.
}
\author{
   Arne Henningsen,
   Ott Toomet \email{otoomet@ut.ee}
}

\seealso{\code{\link[stat]{vcov}}, \code{\link{probit}}.}
\keyword{methods}
