\name{predict.probit}
\alias{predict.probit}

\title{Predict method for fitted probit models}
\description{
   Calculate predicted values for fitted \code{\link{probit}} models.
}

\usage{
   \method{predict}{probit}( object, newdata = NULL, type = "link", ... )
}

\arguments{
   \item{object}{a fitted object of class \code{probit}.}
   \item{newdata}{optionally, a data frame in which to look for variables
      with which to predict.
      If omitted, the fitted linear predictors or the fitted response values
      are returned.}
   \item{type}{the type of prediction.
      If this argument is \code{"link"} (the default),
      the predicted linear predictors are returned.
      If this argument is \code{"response"},
      the predicted probabilities are returned.}
   \item{\dots}{further arguments (currently ignored).}
}

\value{
   A numeric vector of the predicted values.
}

\author{
   Arne Henningsen and the R Core Team (the code of \code{predict.probit}
   is partly based on the code of \code{\link[stats]{predict.lm}}
   and \code{\link[stats]{predict.glm}}).
}

\seealso{
   \code{\link{probit}}, \code{\link[stats]{predict}},
   \code{\link[stats]{predict.glm}}, \code{\link{residuals.probit}},
   and \code{\link{probit-methods}}.
}

\examples{
## female labour force participation probability
data( "Mroz87" )
m <- probit( lfp ~ kids5 + kids618 + educ + hushrs +
   huseduc + huswage + mtr + motheduc, data=Mroz87 )
predict( m )                            # equal to linearPredictors(m)
predict( m, type = "response" )         # equal to fitted(m)
predict( m, newdata = Mroz87[ 3:9, ] )  # equal to linearPredictors(m)[3:9]
predict( m, newdata = Mroz87[ 3:9, ], type = "response" ) # equal to fitted(m)[3:9]
}

\keyword{methods}
