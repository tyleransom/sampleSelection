\name{fitted.selection}
\alias{fitted.selection}

\title{Fitted Values of Selection Models}

\description{
   Calculate fitted values of sample selection models
}

\usage{
\method{fitted}{selection}(object, part = "outcome", ... )
}

\arguments{
   \item{object}{object of class \code{selection}.}
   \item{part}{character string indication which fitted values to extract:
      "outcome" for the fitted values of the outcome equation(s) or
      "selection" for the fitted values of the selection equation.}
   \item{\dots}{further arguments passed to other methods
      (e.g. \code{\link{fitted.probit}} or \code{\link{fitted}}).}
}

\details{
   If the model was estimated by the 2-step method,
   the fitted values of an outcome equation are calculated
   using all regressors of this equation
   including the inverse Mill's ratios.

   If the model was estimated by the maximum likelihood method,
   the fitted values are calculated
   using only the regressors of the respective (outcome/selection) equation.
   In the future, we might add the option to
   include expectations of the error term
   based on the regressors of the other (selection/outcome) equation.
}

\value{
   A numeric vector of the fitted values.
}

\author{Arne Henningsen}

\seealso{\code{\link{selection}}, \code{\link{residuals.selection}}
   \code{\link{fitted}}, and \code{\link{fitted.probit}}}

\keyword{methods}
