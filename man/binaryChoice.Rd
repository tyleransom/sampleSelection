\name{probit}
\alias{binaryChoice}
\alias{probit}
\title{Binary choice models.}
\description{
  Binary Choice models.  These models are estimated by
  \code{binaryChoice}, intended to be called by wrappers like
  \code{probit}. 
}
\usage{
probit(formula, ...)
binaryChoice(formula, subset, na.action, start = NULL, data = sys.frame(sys.parent()),
             x=FALSE, y = FALSE, model = FALSE, method="ML",
userLogLik=NULL,
             cdfLower, cdfUpper=function(x) 1 - cdfLower(x),
logCdfLower=NULL, logCdfUpper=NULL,
pdf, logPdf=NULL, gradPdf,
             ... )
}
\arguments{
  \item{formula}{a symbolic description of the model to be fit, in the
    form \code{response ~ explanatory variables}
    (see also details).}
  \item{subset}{an optional vector specifying a subset of observations to be
    used in the fitting process.}
  \item{na.action}{a function which indicates what should happen when the data
          contain 'NA's.  The default is set by the 'na.action' setting
          of 'options', and is 'na.fail' if that is unset.  The
          'factory-fresh' default is 'na.omit'.  Another possible value
          is 'NULL', no action.  Value 'na.exclude' can be useful.}
  \item{start}{inital value of parameters.}
  \item{data}{an optional data frame containing the variables in the
    model. If not found in data, the
    variables are taken from environment(formula), typically the
    environment from which \code{probit} is called.}
  \item{x, y, model}{logicals. If TRUE the corresponding components of
    the fit (the
    model matrix, the response, the model frame) are returned.}
  \item{method}{the method to use; for fitting,
    currently only method = "ML" (Maximum Likelihood) is supported;
    method = "model.frame" returns the model frame (the same as
    with model = TRUE, see below).}
  \item{userLogLik}{log-likelihood function.  A function of the
    parameter to be estimated, which computes
    the log likelihood.  If supplied, it will be used instead of
    \code{cdfLower} and similar parameters.  This allows user to
    fine-tune the likelihood function such as introducing robust
    approximations.  It might return the corresponding gradient and
    Hessian as approximations, see \code{\link[maxLik]{maxNR}}.
  }
  \item{cdfLower, cdfUpper, pdf, gradPdf}{function, lower and upper tail of the
    cumulative distribution function of the disturbance term,
    corresponding probability density function, and gradient of the
    density function.  These
    functions must take a numeric vector as the argument, and return
    numeric vector of the probability/gradient values.}
  \item{logCdfLower, logCdfUpper, logPdf}{logs of the corresponding
    functions.  Providing these may improve precision in extreme tail.
    If not provided, simply logs are takes of the corresponding non-log
    values.}
  \item{\dots}{further arguments for \code{binaryChoice} and
    \code{\link[maxLik]{maxLik}}.
  }
}
\details{
   The dependent variable for the binary choice models must have exactly
   two levels (e.g. '0' and '1', 'FALSE' and 'TRUE', or 'no' and 'yes').
   Internally, the first level is always coded '0' ('failure') and
   the second level as '1' ('success'), no matter of the actual value.
   However, by default the levels are ordered alphabetically and this
   makes puts '1' after '0', 'TRUE' after 'FALSE' nad 'yes' after 'no'.

   Via the distribution function parameters, \code{binaryChoice}
   supports generic latent linear index binary choice models with
   additive disturbance terms.  It is intended to be called by wrappers
   like \code{probit}.  However, it is also visible in the namespace as
   the user may want to implement her own models using another
   distribution of the disturbance term.
   
   The model is estimated using Maximum Likelihood and Newton-Raphson
   optimizer.

   \code{probit} implements an outlier-robust log-likelihood (Demidenko,
   2001).  In case of large outliers the analytic Hessian is
   singular while Fisher scoring approximation (used, for instance, by
   \code{\link[stats]{glm}}) is invertible.  Those values are not
   reliable in case of outliers.

   No attempt is made to establish the existence of the
   estimator. 
}
\value{
  An object of class "binaryChoice".  It is a list with following components:
  \item{LRT}{Likelihood ration test.  The full model is tested against
    H0: the parameters (besides constant) have no effect on the result.  This is a list
    with components
    \itemize{
    \item{LRT}{The LRT value}
    \item{df}{Degrees of freedom for LRT (= df of the model - 1)}
    }
    LRT is distributed by chi2(df) under H0.}
  \item{param}{A list with following background information:
    \itemize{
      \item{nParam}{Number of parameters of the model including constant}
      \item{nObs}{Number of the observations}
      \item{N1}{Number of observations with non-zero (true) response}
      \item{N0}{Number of observations with zero (false) response}
    }
  }
  \item{df}{Number of free parameters}
  \item{x}{if requested, the model matrix used.}
  \item{y}{if requested, the model response used.  The
   response is represented internally as 0/1 integer vector.}
  \item{model}{the model frame, only if \code{model = TRUE} or
    \code{method = "model.frame"}.}
  \item{na.action}{information returned by \code{\link{model.frame}} on
    the special handling of  NA s.}
  Other components are inherited from \code{\link[maxLik]{maxLik}}.

  \code{probit} adds class "probit" and following components to
  the "binaryChoice" object:
  \item{family}{the family object used (\code{\link{binomial}} with
    \code{link="probit"})}
}
\references{
  Demidenko, Eugene (2001) \dQuote{Computational aspects of probit
  model}, Mathematical Communications 6, 233-247
  }
\author{Ott Toomet \email{otoomet@ut.ee}}
\seealso{\code{\link[maxLik]{maxLik}} for ready-packaged likelihood maximisation
  routines and methods, \code{\link{glm}} for generalised linear models,
  including probit, \code{\link{binomial}}.
}
\examples{
## A simple MC trial: note probit assumes normal errors
x <- runif(100)
e <- 0.5*rnorm(100)
y <- x + e
summary(probit((y > 0) ~ x))
## female labour force participation probability
data(Mroz87)
Mroz87$kids <- Mroz87$kids5 > 0 | Mroz87$kids618 > 0
Mroz87$age30.39 <- Mroz87$age < 40
Mroz87$age50.60 <- Mroz87$age >= 50
summary(probit(lfp ~ kids + age30.39 + age50.60 + educ + hushrs +
               huseduc + huswage + mtr + motheduc, data=Mroz87))
}
\keyword{models}
\keyword{regression}
\keyword{nonlinear}
