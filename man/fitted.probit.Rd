\name{fitted.probit}
\alias{fitted.probit}

\title{Fitted values of probit models}
\description{
   Calculate fitted values of \code{\link{probit}} models.
}
\usage{
   \method{fitted}{probit}( object, ... )
}
\arguments{
   \item{object}{an object of class \code{probit}.}
   \item{\dots}{further arguments (currently ignored).}
}
\value{
   A numeric vector of the fitted values.
}
\author{
   Arne Henningsen \email{ahenningsen@agric-econ.uni-kiel.de}
}

\seealso{\code{\link[stats]{fitted}}, \code{\link{probit}}.}
\keyword{methods}
