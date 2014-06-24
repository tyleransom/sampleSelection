\name{tobit2fit}
\alias{tobit2fit}
\alias{tobit2Bfit}
\alias{tobit5fit}

\title{Fitting Parametric Sample Selection Models}

\description{
   These functions do the actual fitting of tobit-2
   (sample selection) and tobit-5 (switching regression)
   models by Maximum Likelihood (ML) estimation.
   The arguments must be given as numeric vectors/matrices,
   initial value of parameters must be specified.
   These functions are called by \code{\link{selection}} and
   are intended for \code{sampleSelection} internal use.
   The function \code{tobit2Bfit} does the actual fitting of tobit-2
   (sample selection) models with a binary dependent variable
   of the outcome model (\code{YO}) using a double-probit specification.
}

\usage{
tobit2fit( YS, XS, YO, XO, start, weights = NULL, print.level = 0,
   maxMethod = "Newton-Raphson", ... )

tobit2Bfit( YS, XS, YO, XO, start, weights = NULL, print.level = 0,
   maxMethod = "BHHH", ... )

tobit5fit( YS, XS, YO1, XO1, YO2, XO2, start, print.level = 0,
   maxMethod = "Newton-Raphson", ... ) 
}

\arguments{
  \item{YS}{numeric 0/1 vector, where 0 denotes unobserved outcome
(tobit 2) or outcome 1 observed (tobit 5).}
  \item{XS, XO, XO1, XO2}{numeric matrix, model matrix for selection and
outcome equations.}
  \item{YO}{numeric vector, observed outcomes.  Values for unobserved
outcomes are ignored (they may or may not be NA).}
  \item{start}{numeric vector of initial values.  The order is: betaS,
betaO(1), sigma(1), rho(1), betaO2, sigma2, rho2.}
  \item{weights}{an optional vector of \sQuote{prior weights} 
    to be used in the fitting process. Should be NULL or a numeric vector.
    Weights are currently only supported in type-2 models.}
  \item{print.level}{numeric, values greater than 0 will produce
increasingly more debugging information.}
  \item{maxMethod}{character, a maximisation method supported by \code{\link[maxLik]{maxLik}}}
  \item{\dots}{Additional parameters to \code{\link[maxLik]{maxLik}}.}
}

\value{
  Object of class \code{"selection"}.  It inherits from class \code{"maxLik"} and
  includes two additional components: \code{$tobitType}, numeric
  tobit model classifier (see Amemiya, 1985), and \code{$method}, either \code{"ml"}
  or \code{"2step"}, specifying the estimation method.
}
\references{Amemiya, T. (1985) \emph{Advanced Econometrics}, Harvard
  University Press
  }
\author{Ott Toomet \email{otoomet@ut.ee}, Arne Henningsen}

\seealso{\code{\link{selection}}, \code{\link[maxLik]{maxLik}}}

\keyword{internal}
\keyword{models}
\keyword{regression}
