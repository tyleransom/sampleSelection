\name{nlswork}
\docType{data}
\alias{nlswork}

\title{National Longitudinal Survey of Young Working Women}

\description{
   The \code{nlswork} data frame contains data about 4711 young working women
   who had an age of 14--26 years in 1968.
   These data are collected within the "National Longitudinal Survey"
   over the years 1968-1988 (with gaps).
   There are 28534 observations in total.
}

\usage{data(nlswork)}

\format{
   This data frame contains the following columns:
   \describe{
      \item{idcode}{NLS ID.}
      \item{year}{interview year.}
      \item{birth\_yr}{birth year.}
      \item{age}{age in current year.}
      \item{race}{1=white, 2=black, 3=other.}
      \item{msp}{1 if married, spouse present.}
      \item{nev\_mar}{1 if never married.}
      \item{grade}{current grade completed.}
      \item{collgrad}{1 if college graduate.}
      \item{not\_smsa}{1 if not SMSA.}
      \item{c\_city}{1 if central city.}
      \item{south}{1 if south.}
      \item{ind\_code}{industry of employment.}
      \item{occ\_code}{occupation.}
      \item{union}{1 if union.}
      \item{wks\_ue}{weeks unemployed last year.}
      \item{ttl\_exp}{total work experience.}
      \item{tenure}{job tenure, in years.}
      \item{hours}{usual hours worked.}
      \item{wks\_work}{weeks worked last year.}
      \item{ln\_wage}{ln(wage/GNP deflator).}
   }
}

\details{
Two different versions of this data set are available on the internet.
They are slighly different:
The variable \code{wks_work} (weeks worked last year)
is \code{101} in this version (from Stata),
but \code{NA} in the version provided by the Boston College
for the observation with \code{idcode = 1} and \code{year = 83}.
Moreover, this variable
is \code{NA} in this version (from Stata),
but \code{104} in the version provided by the Boston College
for the observation with \code{idcode = 2} and \code{year = 87}.
}

\source{
Datasets for Stata Longitudinal/Panel-Data Reference Manual, Release 10:
National Longitudinal Survey. Young Women 14-26 years of age in 1968,
\url{http://www.stata-press.com/data/r10/nlswork.dta}.
}

\references{
Boston College,
National Longitudinal Survey. Young Women 14-26 years of age in 1968,
\url{http://fmwww.bc.edu/ec-p/data/stata/nlswork.dta}.
}

\examples{
data( "nlswork" )
library( "plm" )
nlswork <- plm.data( nlswork, c( "idcode", "year" ) )
plmResult <- plm( ln_wage ~ union + age + grade + not_smsa + south + occ_code,
   data = nlswork, model = "random" )
}

\keyword{datasets}
