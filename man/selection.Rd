\name{selection}
\alias{selection}
\alias{heckit}
\alias{treatReg}
\title{Heckman-style selection and treatment effect models}
\description{
  This is the frontend for estimating Heckman-style selection
  models either with
  one or two outcomes (also known as generalized tobit models).  It
  supports binary outcomes and interval outcomes in the single-outcome case.
  It also supports normal-distribution based treatment effect models.

  For model specification and more details, see Toomet and Henningsen
  (2008) and the included vignettes \dQuote{Sample Selection Models},
  \dQuote{Interval Regression with Sample Selection}, and
  \dQuote{All-Normal Treatment Effects}.
}

\usage{
selection(selection, outcome, data = sys.frame(sys.parent()),
   weights = NULL, subset, method = "ml",
   type = NULL,
   start = NULL,
   boundaries = NULL,
   ys = FALSE, xs = FALSE, yo = FALSE, xo = FALSE,
   mfs = FALSE, mfo = FALSE,
   printLevel = print.level, print.level=0,
   ...)

heckit( selection, outcome, data = sys.frame(sys.parent()),
   method = "2step", ... )

treatReg(selection, outcome,
   data=sys.frame(sys.parent()),
   mfs=TRUE, mfo=TRUE,
   ...)
}
\arguments{
  \item{selection}{formula, the selection equation.}
  \item{outcome}{the outcome equation(s).  Either a single equation (for
    tobit 2 models), or a list of two equations (tobit 5 models).}
  \item{data}{an optional data frame, list or environment (or object
          coercible by \code{as.data.frame} to a data frame) containing the
          variables in the model.  If not found in \code{data}, the
          variables are taken from \code{environment(formula)}, typically
          the environment from which \code{selection} is called.}
  \item{weights}{an optional vector of \sQuote{prior weights} 
    to be used in the fitting process. Should be NULL or a numeric vector.
    Weights are currently only supported in type-2 models.}
  \item{subset}{an optional index vector specifying a subset of
    observations to be used in the fitting process.}
  \item{method}{how to estimate the model.  Either \code{"ml"} for Maximum
    Likelihood, \code{"2step"} for 2-step estimation,
    or \code{"model.frame"} for returning the model frame (only).}
  \item{type}{model type.  \code{NULL} will automatically detect the
   type, but one may also manually determine it.  Numeric \sQuote{2} and \sQuote{5} are
   tobit-2 and tobit-5 models respectively, and character \dQuote{treatment} is treatment
   effect model.}
  \item{start}{vector, initial values for the ML estimation.  If
    \code{start} does not have names, names are constructed based on the
    model frame.}
  \item{boundaries}{an optional vector of boundaries of the intervals
      of the dependent variable of the outcome equation
      for sample selection models with interval regression
      of the outcome equation.}
  \item{ys, yo, xs, xo, mfs, mfo}{logicals.  If true, the response (\code{y}),
    model matrix (\code{x}) or the model frame (\code{mf})
    of the selection (\code{s}) or outcome
    (\code{o}) equation(s) are returned.}
  \item{printLevel, print.level}{integer.  Various debugging information, higher
    value gives more information.  The preferred option is \sQuote{printLevel}.}
  \item{\dots}{additional parameters for the corresponding fitting
    functions \code{\link{tobit2fit}}, \code{\link{tobit5fit}},
    \code{\link{heckit2fit}}, \code{\link{heckit5fit}},
    and \code{\link{tobit2Bfit}}.}
}
\details{
   The dependent variable of of the selection equation
   (specified by argument \code{selection}) must have exactly
   two levels (e.g., 'FALSE' and 'TRUE', or '0' and '1').
   By default the levels are sorted in increasing order
   ('FALSE' is before 'TRUE', and '0' is before '1').
   If the dependent variable of the outcome equation
   (specified by argument \code{outcome}) has exactly two levels,
   this variable is modelled as a binary variable.
   If argument \code{boundaries} is specified,
   the outcome equation is estimated as interval regression model
   and the dependent variable of the outcome equation
   must be a categorical (factor) variable
   or a variable of strictly positive integer values,
   whereas the vector specified by argument \code{boundaries}
   must have one more element than the number of levels
   or the largest integer, respectively.
   In all other cases, it is assumed
   that the dependent variable of the outcome equation is continuous
   and an ordinary sample selection model is estimated. 

   For tobit-2 (sample selection) models,
   only those observations are included in the second step estimation
   (argument 'outcome'),
   where the dependent variable variable of the selection equation
   equals the second element of its levels (e.g., 'TRUE' or '1').  

   For tobit-5 (switching regression) models,
   in the second step the first outcome equation
   (first element of argument 'outcome') is estimated
   only for those observations,
   where the dependent variable of the selection equation
   equals the first element of its levels (e.g., 'FALSE' or '0').
   The second outcome equation is estimated
   only for those observations,
   where this variable equals the second element of its levels
   (e.g., 'TRUE' or '1').

  Treatment effect models are a version of tobit-5 models where the two
  outcomes are \dQuote{participation} and \dQuote{non-participation}.
\code{treatReg}
  takes an equal set of explanatory variables for both of these choices
  and assumes that the corresponding parameters are equal.  In typical
  treatment effect model the selection outcome variable (participation
  decision) is included as an explanatory variable for the outcome.
  If this is not done, \code{treatReg} amounts to estimating two
  equations with correlated error structure.

  \code{NA}-s are allowed in the data.  These are ignored if the
  corresponding outcome is unobserved, otherwise observations which
  contain \code{NA} (either in selection or outcome) are
  removed.
  
  These selection models assume a known (multivariate normal)
  distribution
  of error terms.  Because of this, the instruments (exclusion
  restrictions) are not necessary.  However, if no instruments are
  supplied, the results are based solely on the assumption on
  multivariate normality.  This may or may not be an appropriate
  assumption for a particular problem.  Note also that standard errors
  tend to be large without a strong
  exclusion restriction.
  
  If argument \code{method} is equal to \code{"ml"} (the default),
  the estimation is done by the maximum likelihood method,
  where the Newton-Raphson algorithm is used by default.
  Argument \code{maxMethod} (see \code{\link{tobit2fit}}) 
  can be used to chose other algorithms for the maximisation 
  of the (log) likelihood function.
  
  If argument \code{method} is equal to \code{"ml"} (the default)
  and argument \code{start} is \code{NULL} (the default),
  the starting values for the maximum-likelihood (ML) estimation
  of a tobit-2 or tobit-5 model
  are obtained by an initial two-step estimation of this model.
  
  The two-step estimation of interval-regression models with sample-selection
  has not yet been implemented.
  If no starting values for a maximum-likelihood (ML) estimation
  of an interval-regression model with sample-selection
  are specified (i.e., argument \code{start} is \code{NULL}, the default),
  starting values are obtained by an initial estimation of a tobit-2 model,
  where the dependent variable of the outcome equation
  is set to the mid points of the boudaries of the intervals.
  By default, the starting values are obtained
  by a maximum-likelihood (ML) estimation of the tobit-2 model,
  whereas the starting values
  for the maximum-likelihood (ML) estimation of the tobit-2 model
  are obtained by a 2-step estimation of the tobit-2 model.
  If argument \code{start} is set to \code{"2step"},
  the starting values for the maximum-likelihood (ML) estimation
  of an interval-regression model with sample-selection
  are directly obtained by a 2-step estimation of the tobit-2 model
  (i.e., without a subsequent ML estimation of the tobit-2 model).
  
  Methods that can be applied to objects returned by \code{selection()}
  are described on the help page \code{\link{selection-methods}}.
}
\value{
  'selection' returns an object of class "selection".
  If the model estimated by Maximum Likelihood (argument method = "ml"),
  this object is a list, which has all the components of a 'maxLik' object, and in
  addition the elements 'twoStep', 'start, 'param', 'termS', 'termO',
  'outcomeVar', and if requested 'ys', 'xs', 'yo', 'xo', 'mfs', and 'mfo'.
  If a tobit-2 (sample selection) model is estimated by the
  two-step method (argument method = "2step"),
  the returned object is a list with components
  'probit', 'coefficients', 'param', 'vcov', 'lm', 'sigma', 'rho',
  'invMillsRatio', and 'imrDelta'.
  If a tobit-5 (switching regression) model is estimated by the
  two-step method (argument method = "2step"),
  the returned object is a list with components
  'coefficients', 'vcov', 'probit', 'lm1', 'lm2', 'rho1', 'rho2',
  'sigma1', 'sigma2', 'termsS', 'termsO', 'param',
  and if requested 'ys', 'xs', 'yo', 'xo', 'mfs', and 'mfo'.

  \item{probit}{object of class 'probit' that contains the results of the
    1st step (probit estimation) (only for two-step estimations).}
  \item{twoStep}{(only if initial values not given)
    results of the 2-step estimation, used for initial values}
  \item{start}{initial values for ML estimation}
  \item{termsS, termsO}{terms for the selection and outcome equation}
  \item{ys, xs, yo, xo, mfs, mfo}{response, matrix and frame of the
    selection- and outcome equations (as a list of two for the latter).
    NULL, if not requested.  The
    response is represented internally as 0/1 integer vector with 0
    denoting either the unobservable outcome (tobit 2) or the first
    selection (tobit 5).}
  \item{coefficients}{estimated coefficients, the complete model.
    coefficient for the Inverse Mills ratio is treated as a parameter
    (\eqn{=\varrho \sigma}{= rho * sigma}).}
  \item{vcov}{variance covariance matrix of the estimated coefficients.}
  \item{param}{a list with following components: \code{index}, a list of
    numeric vectors: where in the \code{coef} the component are
    located;
    \code{oIntercept}, a logical: whether the outcome equation includes
    intercept;
    \code{N0, N1}, integer, number of observations with unobserved and
    observed outcomes;
    \code{nObs}, integer, number of valid observations;
    \code{nParam}, integer, number of the parameters in the model (not
    all are independent);
    \code{df}, integer, degrees of freedom.  Note this is not equal to
    \code{nObs - nParam} because of the parameters are not independent
    in all the cases;
    \code{levels} levels for the response of the selection equation.
    \code{levels[1]} corresponds to the outcome 1, \code{levels[2]} to
    the outcome 2.
  }
  \item{lm, lm1, lm2}{objects of class 'lm' that contain the results
    of the 2nd step estimation(s) of the outcome equation(s).
    Note: the standard errors of this
    estimation are biased, because they do not account for the
    estimation of \eqn{\gamma} in the 1st step estimation
    (the correct standard errors are returned by \code{summary} and they
    are contained in \code{vcov} component).}
  \item{sigma, sigma1, sigma2}{the standard error(s)
    of the error terms of the outcome equation(s).}
  \item{rho, rho1, rho2}{the estimated correlation coefficient(s)
    between the error term of the selection equation
    and the outcome equation(s).}
  \item{invMillsRatio}{the inverse Mills Ratios calculated from the results of
    the 1st step probit estimation.}
  \item{imrDelta}{the \eqn{\delta}s calculated from the inverse Mills Ratios and
    the results of the 1st step probit estimation.}
  \item{outcomeVar}{character string indicating whether the dependent variable
    of the outcome equation is \code{"continuous"} or \code{"binary"}.}
}

\references{
  Cameron, A. C. and Trivedi, P. K. (2005)
  \emph{Microeconometrics: Methods and Applications},
  Cambridge University Press.

  Greene, W. H. (2003)
  \emph{Econometric Analysis, Fifth Edition}, Prentice Hall.

  Heckman, J. (1976)
  The common structure of statistical models of
  truncation, sample selection and limited dependent
  variables and a simple estimator for such models,
  \emph{Annals of Economic and Social Measurement},
  5(4), p. 475-492.

  Johnston, J. and J. DiNardo (1997)
  \emph{Econometric Methods, Fourth Edition}, McGraw-Hill.

  Lee, L., G. Maddala and R. Trost (1980)
  Asymetric covariance matrices of two-stage probit and two-stage tobit
  methods for simultaneous equations models with selectivity.
  \emph{Econometrica}, 48, p. 491-503.
  
  Petersen, S., G. Henningsen and A. Henningsen (2017)
  \emph{Which Households Invest in Energy-Saving Home Improvements?
  Evidence From a Danish Policy Intervention}.
  Unpublished Manuscript.
  Department of Management Engineering, Technical University of Denmark.

  Toomet, O. and A. Henningsen, (2008)
  Sample Selection Models in R: Package sampleSelection.
  \emph{Journal of Statistical Software} 27(7),
  \url{http://www.jstatsoft.org/v27/i07/}

  Wooldridge, J. M. (2003)
  \emph{Introductory Econometrics: A Modern Approach, 2e}, Thomson South-Western.
}

\note{
  The 2-step estimate of 'rho' may be outside of the
  \eqn{[-1,1]}{[-1,1]} interval.  In that case the standard errors of
  invMillsRatio may be meaningless.
  }

\author{Arne Henningsen,
   Ott Toomet \email{otoomet@ut.ee}}

\seealso{
   \code{\link{summary.selection}}, \code{\link{selection-methods}},
   \code{\link{probit}}, \code{\link{lm}},
   and \code{\link{Mroz87}} and \code{\link{RandHIE}}
   for further examples.
}

\examples{
## Greene( 2003 ): example 22.8, page 786
data( Mroz87 )
Mroz87$kids  <- ( Mroz87$kids5 + Mroz87$kids618 > 0 )
# Two-step estimation
summary( heckit( lfp ~ age + I( age^2 ) + faminc + kids + educ,
   wage ~ exper + I( exper^2 ) + educ + city, Mroz87 ) )
# ML estimation
summary( selection( lfp ~ age + I( age^2 ) + faminc + kids + educ,
   wage ~ exper + I( exper^2 ) + educ + city, Mroz87 ) )

## Example using binary outcome for selection model.
## We estimate the probability of womens' education on their
## chances to get high wage (> $5/hr in 1975 USD), using PSID data
## We use education as explanatory variable
## and add age, kids, and non-work income as exclusion restrictions.
data(Mroz87)
m <- selection(lfp ~ educ + age + kids5 + kids618 + nwifeinc,
   wage >= 5 ~ educ, data = Mroz87 )
summary(m)


## example using random numbers
library( "mvtnorm" )
nObs <- 1000
sigma <- matrix( c( 1, -0.7, -0.7, 1 ), ncol = 2 )
errorTerms <- rmvnorm( nObs, c( 0, 0 ), sigma )
myData <- data.frame( no = c( 1:nObs ), x1 = rnorm( nObs ), x2 = rnorm( nObs ),
   u1 = errorTerms[ , 1 ], u2 =  errorTerms[ , 2 ] )
myData$y <- 2 + myData$x1 + myData$u1
myData$s <- ( 2 * myData$x1 + myData$x2 + myData$u2 - 0.2 ) > 0
myData$y[ !myData$s ] <- NA
myOls <- lm( y ~ x1, data = myData)
summary( myOls )
myHeckit <- heckit( s ~ x1 + x2, y ~ x1, myData, print.level = 1 )
summary( myHeckit )

## example using random numbers with IV/2SLS estimation
library( "mvtnorm" )
nObs <- 1000
sigma <- matrix( c( 1, 0.5, 0.1, 0.5, 1, -0.3, 0.1, -0.3, 1 ), ncol = 3 )
errorTerms <- rmvnorm( nObs, c( 0, 0, 0 ), sigma )
myData <- data.frame( no = c( 1:nObs ), x1 = rnorm( nObs ), x2 = rnorm( nObs ),
   u1 = errorTerms[ , 1 ], u2 = errorTerms[ , 2 ], u3 = errorTerms[ , 3 ] )
myData$w <- 1 + myData$x1 + myData$u1
myData$y <- 2 + myData$w + myData$u2
myData$s <- ( 2 * myData$x1 + myData$x2 + myData$u3 - 0.2 ) > 0
myData$y[ !myData$s ] <- NA
myHeckit <- heckit( s ~ x1 + x2, y ~ w, data = myData )
summary( myHeckit )  # biased!
myHeckitIv <- heckit( s ~ x1 + x2, y ~ w, data = myData, inst = ~ x1 )
summary( myHeckitIv ) # unbiased

## tobit-5 example
N <- 500
   library(mvtnorm)
   vc <- diag(3)
   vc[lower.tri(vc)] <- c(0.9, 0.5, 0.6)
   vc[upper.tri(vc)] <- vc[lower.tri(vc)]
   eps <- rmvnorm(N, rep(0, 3), vc)
   xs <- runif(N)
   ys <- xs + eps[,1] > 0
   xo1 <- runif(N)
   yo1 <- xo1 + eps[,2]
   xo2 <- runif(N)
   yo2 <- xo2 + eps[,3]
   a <- selection(ys~xs, list(yo1 ~ xo1, yo2 ~ xo2))
   summary(a)

## tobit2 example
   vc <- diag(2)
   vc[2,1] <- vc[1,2] <- -0.7
   eps <- rmvnorm(N, rep(0, 2), vc)
   xs <- runif(N)
   ys <- xs + eps[,1] > 0
   xo <- runif(N)
   yo <- (xo + eps[,2])*(ys > 0)
   a <- selection(ys~xs, yo ~xo)
   summary(a)

## Example for treatment regressions
## Estimate the effect of treatment on income
## selection outcome: treatment participation, logical (treatment)
## selection explanatory variables: age, education (years)
##   unemployment in 1974, 1975, race
## outcome: log real income 1978
## outcome explanatory variables: treatment, age, education, race.
## unemployment variables are treated as exclusion restriction
data(Treatment, package="Ecdat")
a <- treatReg(treat~poly(age,2) + educ + u74 + u75 + ethn,
              log(re78)~treat + poly(age,2) + educ + ethn,
              data=Treatment)
print(summary(a))
}

\keyword{models}
\keyword{regression}
