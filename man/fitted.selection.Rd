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
   the fitted values of the outcome equation are calculated
   using all regressors of this equation
   including the inverse Mill's ratios,
   i.e. the fitted values correspond to the conditional expectations
   (see \code{\link{predict.selection}}).

   If the model was estimated by the maximum likelihood method,
   the fitted values of the outcome equation are calculated
   by disregarding the selection equation,
   i.e. the fitted values correspond to the unconditional expectations
   (see \code{\link{predict.selection}}).
   
   The fitted values of the selection equation are probabilities.
   If the dependent variable of the outcome equation is binary,
   also the fitted values of the outcome equation are probabilities.
}

\value{
   A numeric vector of the fitted values.
}

\author{Arne Henningsen}

\seealso{
   \code{\link{fitted}}, \code{\link{selection}},
   \code{\link{residuals.selection}}, and \code{\link{selection-methods}}.
}

\keyword{methods}
