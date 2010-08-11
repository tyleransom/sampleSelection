\name{nObs.probit}
\alias{nObs.probit}

\title{Number of Observations of Probit Models}
\description{
   Extract the number of observations from \code{\link{probit}} models.
}

\usage{
   \method{nObs}{probit}( x, ... )
}

\arguments{
   \item{x}{an object of class \code{probit}.}
   \item{\dots}{further arguments (currently ignored).}
}

\value{
   A numeric scalar: the number of observations.
}

\author{
   Arne Henningsen
}

\seealso{\code{\link{probit}}, \code{\link[maxLik]{nObs}}.}

\keyword{methods}
