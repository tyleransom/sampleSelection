\name{residuals.probit}
\alias{residuals.probit}

\title{Residuals of probit models}
\description{
   Calculate residuals of \code{\link{probit}} models.
}

\usage{
   \method{residuals}{probit}( object, type = "deviance", ... )
}

\arguments{
   \item{object}{an object of class \code{probit}.}
   \item{type}{the type of residuals which should be returned.
      The alternatives are: "deviance" (default), "pearson", and "response"
      (see details).}
   \item{\dots}{further arguments (currently ignored).}
}

\details{
   The residuals are calculated with following formulas:

   Response residuals:
   \eqn{r_i = y_i - \hat{y}_i}

   Pearson residuals:
   \eqn{r_i = ( y_i - \hat{y}_i ) / \sqrt{ \hat{y}_i ( 1 - \hat{y}_i ) }}

   Deviance residuals:
   \eqn{r_i = \sqrt{ -2 \log( \hat{y}_i ) }} if \eqn{y_i = 1},
   \eqn{r_i = - \sqrt{ -2 \log( 1 - \hat{y}_i ) }} if \eqn{y_i = 0}

   Here, \eqn{r_i} is the \eqn{i}th residual,
   \eqn{y_i} is the \eqn{i}th response,
   \eqn{\hat{y}_i = \Phi( x_i' \hat{\beta} )} is the estimated probability
   that \eqn{y_i} is one,
   \eqn{\Phi} is the cumulative distribution function of the standard normal
   distribution,
   \eqn{x_i} is the vector of regressors of the \eqn{i}th observation, and
   \eqn{\hat{\beta}} is the vector of estimated coefficients.

   More details are available in Davison & Snell (1991).
}

\value{
   A numeric vector of the residuals.
}

\references{
   Davison, A. C. and Snell, E. J. (1991)
   \emph{Residuals and diagnostics.}
   In: Statistical Theory and Modelling. In Honour of Sir David Cox,
   edited by Hinkley, D. V., Reid, N. and Snell, E. J.,
   Chapman & Hall, London.
}

\author{
   Arne Henningsen
}

\seealso{\code{\link{probit}}, \code{\link[stats]{residuals}},
   \code{\link[stats]{residuals.glm}}.}

\keyword{methods}
