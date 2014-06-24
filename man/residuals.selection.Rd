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
   \item{type}{the type of residuals (see section \sQuote{Details}).
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
   
   Argument \code{type} is only used for binary dependent variables,
   i.e. if argument \code{part} is equal \code{"selection"}
   or the dependent variable of the outcome model is binary.
   Hence, argument \code{type} is ignored
   if argument \code{part} is equal \code{"outcome"}
   and the dependent variable of the outcome model is numeric.
}

\value{
   A numeric vector of the residuals.
}

\author{Arne Henningsen}

\seealso{
   \code{\link{residuals}}, \code{\link{selection}},
   \code{\link{fitted.selection}}, \code{\link{residuals.probit}},
   and \code{\link{selection-methods}}.
}

\keyword{methods}
