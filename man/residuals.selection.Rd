\name{residuals.selection}
\alias{residuals.selection}

\title{Residuals of Selection Models}

\description{
   Calculate residuals of sample selection models
}

\usage{
\method{residuals}{selection}(object, part = "outcome",
   type = "deviance", ... )
}

\arguments{
   \item{object}{object of class \code{selection}.}
   \item{part}{character string indication which residuals to extract:
      "outcome" for the fitted values of the outcome equation(s) or
      "selection" for the fitted values of the selection equation.}
   \item{type}{the type of residuals of the selection equation.
      The alternatives are: "deviance" (default), "pearson", and "response"
      (see \code{\link{residuals.probit}}).}
   \item{\dots}{further arguments passed to other methods
      (e.g. \code{\link{residuals.probit}} or \code{\link{residuals}}).}
}

\details{
   The calculation of the fitted values
   that are used to calculate the residuals
   is described in the details section of the documentation
   of \code{\link{fitted.selection}}.
}

\value{
   A numeric vector of the residuals.
}

\author{Arne Henningsen}

\seealso{\code{\link{selection}}, \code{\link{fitted.selection}},
   \code{\link{residuals}}, and \code{\link{residuals.probit}}}

\keyword{methods}
