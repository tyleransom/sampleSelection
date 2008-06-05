\name{summary.probit}
\alias{summary.probit}
\alias{print.summary.probit}

\title{Summarizing Probit Estimations}

\description{
   Print or return a summary of a probit estimation.
}

\usage{
   \method{summary}{probit}( object, ... )
   \method{print}{summary.probit}( x, ... )
}

\arguments{
   \item{object}{an object of class \code{probit}.}
   \item{x}{an object of class \code{summary.probit}.}
   \item{...}{currently not used.}
}

\value{
   The \code{summary} method returns an object of class \code{summary.probit};
   the \code{print} method prints summary results and returns
   the argument invisibly.
}

\author{Arne Henningsen \email{ahenningsen@agric-econ.uni-kiel.de}}

\seealso{\code{\link{probit}}}

\keyword{models}



