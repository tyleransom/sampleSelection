library( "sampleSelection" )
library( "mvtnorm" )
options( digits = 2 )

nObs <- 300
betaS <- c( 1, 1, -1 )
betaO <- c( 10, 4 )
rho <- 0.4
sigma <- 5
bound <- c(-Inf,5,15,Inf)


set.seed(123)
dat <- data.frame( x1 = rnorm( nObs ), x2 = rnorm( nObs ) )
vcovMat <- matrix( c( 1, rho*sigma, rho*sigma, sigma^2 ), nrow = 2 )
eps <- rmvnorm( nObs, sigma = vcovMat )
dat$epsS <- eps[,1]
dat$epsO <- eps[,2]
dat$yS <- with( dat, betaS[1] + betaS[2] * x1 + betaS[3] * x2 + epsS ) > 0
# table(dat$yS)
dat$yOu <- with( dat, betaO[1] + betaO[2] * x1 + epsO )
dat$yOu[ !dat$yS ] <- NA
hist( dat$yOu )
dat$yO <- cut( dat$yOu, bound )

YS <- dat$yS
XS <- cbind( 1, dat$x1, dat$x2 )
YO <- as.numeric( dat$yO )
XO <- cbind( 1, dat$x1 )


start <- c( betaS, betaO, log( sqrt( sigma ) ), atanh( rho ) )
# the correct starting value of logSigma would be: log( sigma )
names( start ) <- c( "betaS0", "betaS1", "betaS2", "betaO0", "betaO2",
   "logSigma", "atanhRho" )

res <- selection( yS ~ x1 + x2, yO ~ x1, data = dat, boundaries = bound, 
   start = start, printLevel = 1 )

print( res )
print( round( coef( res ), 2 ) )
print( round( coef( summary( res ) ), 2 ) )
print( res$start )
print( summary( res ) )


# tests with automatically generated starting values (ML estimation)
resMl <- selection( yS ~ x1 + x2, yO ~ x1, data = dat, boundaries = bound, 
   start = "ml", printLevel = 1 )
print( resMl )
print( round( coef( resMl ), 2 ) )
print( round( coef( summary( resMl ) ), 2 ) )
print( resMl$start )
print( summary( resMl ) )


# tests with automatically generated starting values (2-step estimation)
res2s <- selection( yS ~ x1 + x2, yO ~ x1, data = dat, boundaries = bound, 
   start = "2step", printLevel = 1 )
print( res2s )
print( round( coef( res2s ), 2 ) )
print( round( coef( summary( res2s ) ), 2 ) )
print( res2s$start )
print( summary( res2s ) )


# tests with incorrectly specified starting values
try( selection( yS ~ x1 + x2, yO ~ x1, data = dat, boundaries = bound, 
   start = "wrong" ) )
try( selection( yS ~ x1 + x2, yO ~ x1, data = dat, boundaries = bound, 
   start = rep( 1, 11 ) ) )

# tests with incorrectly specified boundaries
try( selection( yS ~ x1 + x2, yO ~ x1, data = dat, boundaries = 1:6, 
   start = start ) )
try( selection( yS ~ x1 + x2, yO ~ x1, data = dat, boundaries = 4:1, 
   start = start ) )


### tests with Smoke data
data(Smoke)

## tests with different specifications
bounds <- c(0,5,10,20,50,Inf)

# test with low number of variables
Smoke_spec1 <- selection( smoker ~ educ + age, 
   cigs_intervals ~ educ, data = Smoke, boundaries = bounds,
  start = "2step" )

# test with more variables
Smoke_spec2 <- selection( smoker ~ educ + age + restaurn, 
   cigs_intervals ~ educ + income + restaurn, data = Smoke, 
  boundaries = bounds, start = "2step")

# testing models against each other
library( "lmtest" )
lrtest(Smoke_spec1, Smoke_spec2)
waldtest(Smoke_spec1, Smoke_spec2)
