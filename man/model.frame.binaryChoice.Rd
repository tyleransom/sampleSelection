\name{model.frame.binaryChoice}
\alias{model.frame.binaryChoice}

\title{Data of Binary Choice Models}

\description{
   Return the variables used for estimating a binary choice model
}

\usage{
\method{model.frame}{binaryChoice}( formula, ... )
}

\arguments{
   \item{formula}{object of class \code{binaryChoice}.}
   \item{\dots}{further arguments passed to other methods
      (e.g. \code{\link{model.frame}}).}
}
\value{
   A data.frame containing all variables used for the estimation.
}

\author{Arne Henningsen,
   Ott Toomet \email{otoomet@ut.ee}}

\seealso{\code{\link{binaryChoice}}, \code{\link{probit}},
   \code{\link{model.frame}}, \code{\link{model.matrix.binaryChoice}},
   and \code{\link{probit-methods}}.}

\keyword{methods}
