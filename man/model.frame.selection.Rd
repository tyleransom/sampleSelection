\name{model.frame.selection}
\alias{model.frame.selection}

\title{Data of Selection Models}

\description{
   Return the variables used for estimating a sample selection model
}

\usage{
\method{model.frame}{selection}(formula, ... )
}

\arguments{
   \item{formula}{object of class \code{selection}.}
   \item{\dots}{further arguments passed to other methods
      (e.g. \code{\link{model.frame}} or \code{\link{model.frame.probit}}).}
}
\value{
   A data.frame containing all variables used for the estimation.
}

\author{Arne Henningsen \email{ahenningsen@agric-econ.uni-kiel.de}}

\seealso{\code{\link{selection}},
   \code{\link{model.frame}}, and \code{\link{model.frame.probit}}}

\keyword{methods}
