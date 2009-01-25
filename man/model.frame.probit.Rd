\name{model.frame.probit}
\alias{model.frame.probit}

\title{Data of Probit Models}

\description{
   Return the variables used for estimating a probit model
}

\usage{
\method{model.frame}{probit}( formula, ... )
}

\arguments{
   \item{formula}{object of class \code{probit}.}
   \item{\dots}{further arguments passed to other methods
      (e.g. \code{\link{model.frame}}).}
}
\value{
   A data.frame containing all variables used for the estimation.
}

\author{Arne Henningsen,
   Ott Toomet \email{otoomet@ut.ee}}

\seealso{\code{\link{probit}}, and \code{\link{model.frame}}}

\keyword{methods}
