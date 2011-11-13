\name{model.matrix.selection}
\alias{model.matrix.selection}

\title{Design Matrix of Selection Models}

\description{
   Create design matrix of sample selection models
}

\usage{
\method{model.matrix}{selection}(object, part = "outcome", ... )
}

\arguments{
   \item{object}{object of class \code{selection}.}
   \item{part}{character string indication which design matrix/matrices to extract:
      "outcome" for the design matrix/matrices of the outcome equation(s) or
      "selection" for the design matrix of the selection equation.}
   \item{\dots}{further arguments passed to other methods
      (e.g. \code{\link{model.matrix.binaryChoice}} or \code{\link{model.matrix}}).}
}
\value{
   If argument \code{part} is \code{"selection"},
   the design matrix of the selection equation is returned.

   If argument \code{part} is \code{"outcome"},
   the design matrix of the outcome equation of a tobit-2 model
   or a list that contains the design matrices of the (two) outcome equations
   of a tobit-5 model is returned.
   All values of observations (rows of the design matrix)
   that were not selected for estimating the corresponding outcome equation
   are set to \code{NA}.
}

\author{Arne Henningsen}

\seealso{\code{\link{selection}},
   \code{\link{model.matrix}}, and \code{\link{model.matrix.binaryChoice}}}

\keyword{methods}
