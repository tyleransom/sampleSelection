\name{heckit2fit}
\alias{heckit2fit}
\alias{heckit5fit}
\alias{heckitTfit}

\title{2-step Heckman (heckit) estimation}

\description{
   These functions do the actual fitting of tobit-2
   (sample selection), tobit-5 (switching regression) and
   normal-disturbance treatment effect
   models by the 2-step Heckman (heckit) estimation.
   They are called by \code{\link{selection}} or
   \code{\link{heckit}} and
   they are intended for \code{sampleSelection} internal use.
}

\usage{
heckit2fit( selection, outcome, data=sys.frame(sys.parent()),
   weights = NULL, inst = NULL,
   printLevel=print.level, print.level = 0,
   maxMethod = "Newton-Raphson" )

heckit5fit( selection, outcome1, outcome2, data = sys.frame(sys.parent()),
   ys = FALSE, yo = FALSE, xs = FALSE, xo = FALSE, mfs = FALSE,
   mfo = FALSE,
   printLevel=print.level, print.level = 0, maxMethod = "Newton-Raphson", ... )

heckitTfit(selection, outcome, data=sys.frame(sys.parent()),
   ys=FALSE, yo=FALSE, xs=FALSE, xo=FALSE, mfs=FALSE, mfo=FALSE,
   printLevel=0, maxMethod="Newton-Raphson", ... )
}
\arguments{
  \item{selection}{formula for the probit estimation (1st step)
    (see \code{\link{selection}}).}
  \item{outcome}{formula to be estimated (2nd step).  In case of
    treatment effect model, it may include the response indicator from
    selection equation.}
  \item{outcome1}{formula, the first outcome equation.}
  \item{outcome2}{formula, the second outcome equation.}
  \item{data}{a data frame containing the data.}
  \item{weights}{an optional vector of \sQuote{prior weights} 
    to be used in the fitting process. Should be NULL or a numeric vector.
    Weights are currently only supported in type-2 models.}
  \item{inst}{an optional one-sided formula specifying instrumental variables
    for a 2SLS/IV estimation on the 2nd step.}
  \item{ys, yo, xs, xo, mfs, mfo}{logicals.  If true, the response (\code{y}),
    model matrix (\code{x}) or the model frame (\code{mf})
    of the selection (\code{s}) or outcome
    (\code{o}) equation(s) are returned.}
  \item{print.level}{numeric, values greater than 0 will produce
    increasingly more debugging information.}
  \item{maxMethod}{character string,
    a maximisation method supported by \code{\link[maxLik]{maxLik}}
    that is used for estimating the probit model (1st stage).}
  \item{...}{currently not used.}
}

\value{
   see \code{\link{selection}}.
}

\references{
   see \code{\link{selection}}.
}

\author{Arne Henningsen,
   Ott Toomet \email{otoomet@ut.ee}}

\seealso{\code{\link{selection}}, \code{\link{heckit}}}

\keyword{internal}
\keyword{models}
\keyword{regression}
