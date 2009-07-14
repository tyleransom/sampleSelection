\name{probit}
\alias{probit}
\alias{print.probit}
\title{Probit binary choice model}
\description{
  This function calculates probit binary choice model
}
\usage{
probit(formula, subset, na.action, start = NULL, data = sys.frame(sys.parent()),
       x=FALSE, y = FALSE, model = FALSE, method="ML", ... )
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
  \item{\dots}{further arguments for \code{maxLik}.}
}
\details{
  The model is estimated using Maximum Likelihood method.

   The dependent variable must have exactly
   two levels (e.g. '0' and '1', 'FALSE' and 'TRUE', or 'no' and 'yes').
   \code{probit} always takes the first level as '0' and
   the second level as '1' -- no matter the actual value.
   However, by default levels are sorted in increasing order and thus,
   the second level is generally, e.g., '1' (after '0'),
   'TRUE' (after 'FALSE'), or 'yes' (after 'no').
}
\value{
  An object of class "probit".  It is a list with following components:
  \item{LRT}{Likelihood ration test.  The full model is tested against
    H0: the parameters have no effect on the result.  This is a list
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
  Other components are inherited from \code{\link[maxLik]{maxLik}}.
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
