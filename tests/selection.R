library(sampleSelection)
library(mvtnorm)
options(digits=6)
N <- 1500
NNA <- 5
vc <- diag(3)
vc[lower.tri(vc)] <- c(0.9, 0.5, 0.6)
vc[upper.tri(vc)] <- vc[lower.tri(vc)]
set.seed(1)
## ------- Tobit-5 example ---------
eps <- rmvnorm(N, rep(0, 3), vc)
xs <- runif(N)
ys <- xs + eps[,1] > 0
xo1 <- runif(N)
yo1 <- xo1 + eps[,2]
xo2 <- runif(N)
yo2 <- xo2 + eps[,3]
## Put some NA-s into the data
ys[sample(N, NNA)] <- NA
xs[sample(N, NNA)] <- NA
xo1[sample(N, NNA)] <- NA
xo2[sample(N, NNA)] <- NA
yo1[sample(N, NNA)] <- NA
yo2[sample(N, NNA)] <- NA
testTobit5TwoStep <- selection(ys~xs, list(yo1 ~ xo1, yo2 ~ xo2), method="2step")
print( testTobit5TwoStep )
print( summary( testTobit5TwoStep ) )
print( coef( testTobit5TwoStep ), digits = 5 )
print( coef( testTobit5TwoStep, part = "outcome" ), digits = 5 )
print( coef( summary( testTobit5TwoStep ) ), digits = 5 )
print( coef( summary( testTobit5TwoStep ), part = "outcome" ), digits = 5 )
print( vcov( testTobit5TwoStep ), digits = 5 )
print( vcov( testTobit5TwoStep, part = "outcome" ), digits = 5 )
print( fitted( testTobit5TwoStep, part = "outcome" ), digits = 5 )
print( fitted( testTobit5TwoStep, part = "selection" ), digits = 5 )
print( residuals( testTobit5TwoStep, part = "outcome" ), digits = 5 )
print( residuals( testTobit5TwoStep, part = "selection", type = "response" ),
   digits = 5 )
print( model.matrix( testTobit5TwoStep, part = "outcome" ), digits = 5 )
print( model.matrix( testTobit5TwoStep, part = "selection" ), digits = 5 )
print( model.frame( testTobit5TwoStep ), digits = 5 )

testTobit5Ml <- selection(ys~xs, list(yo1 ~ xo1, yo2 ~ xo2), method="ml")
print( testTobit5Ml )
print( summary( testTobit5Ml ) )
print( coef( testTobit5Ml ), digits = 5 )
print( coef( testTobit5Ml, part = "outcome" ), digits = 5 )
print( coef( summary( testTobit5Ml ) ), digits = 5 )
print( coef( summary( testTobit5Ml ), part = "outcome" ), digits = 5 )
print( vcov( testTobit5Ml ), digits = 5 )
print( vcov( testTobit5Ml, part = "outcome" ), digits = 5 )
print( fitted( testTobit5Ml, part = "outcome" ), digits = 5 )
print( fitted( testTobit5Ml, part = "selection" ), digits = 5 )
print( residuals( testTobit5Ml, part = "outcome" ), digits = 5 )
print( residuals( testTobit5Ml, part = "selection" ), digits = 5 )
mmsTestTobit5Ml <- model.matrix( testTobit5Ml, part = "selection" )
print( mmsTestTobit5Ml, digits = 5 )
mmoTestTobit5Ml <- model.matrix( testTobit5Ml, part = "outcome" )
print( mmoTestTobit5Ml, digits = 5 )
mfTestTobit5Ml <- model.frame( testTobit5Ml )
print( mfTestTobit5Ml, digits = 5 )
# ML with model.matrices returned
testTobit5MlMm <- selection( ys ~ xs, list( yo1 ~ xo1, yo2 ~ xo2 ),
   method = "ml", xs = TRUE, xo = TRUE )
mmsTestTobit5MlMm <- model.matrix( testTobit5MlMm, part = "selection" )
attributes( mmsTestTobit5Ml )$assign <- NULL
all.equal( mmsTestTobit5Ml, mmsTestTobit5MlMm )
mmoTestTobit5MlMm <- model.matrix( testTobit5MlMm, part = "outcome" )
attributes( mmoTestTobit5Ml[[ 1 ]] )$assign <- NULL
attributes( mmoTestTobit5Ml[[ 2 ]] )$assign <- NULL
all.equal( mmoTestTobit5Ml, mmoTestTobit5MlMm )
# ML with model.frames returned
testTobit5MlMf <- selection( ys~xs, list( yo1 ~ xo1, yo2 ~ xo2 ),
   method = "ml", mfs = TRUE, mfo = TRUE )
mfTestTobit5MlMf <- model.frame( testTobit5MlMf )
all.equal( mfTestTobit5Ml, mfTestTobit5MlMf )

# return just the model.frame
selection( ys~xs, list( yo1 ~ xo1, yo2 ~ xo2 ), method="model.frame" )

# factors as dependent variable (from Achim Zeileis)
selection( ys ~ xs, list( yo1 ~ xo1, yo2 ~ xo2 ), method = "2step" )
selection( factor( ys ) ~ xs, list( yo1 ~ xo1, yo2 ~ xo2 ), method = "2step" )
selection( factor( ys, labels = c( "no", "yes" ) ) ~ xs,
   list( yo1 ~ xo1, yo2 ~ xo2 ), method = "2step" )

selection( ys ~ xs, list( yo1 ~ xo1, yo2 ~ xo2 ) )
selection( factor( ys ) ~ xs, list( yo1 ~ xo1, yo2 ~ xo2 ) )
selection( factor( ys, labels = c( "no", "yes" ) ) ~ xs,
   list( yo1 ~ xo1, yo2 ~ xo2 ) )
# return just the model.frame
selection( ys ~ xs, list( yo1 ~ xo1, yo2 ~ xo2 ), method="model.frame" )
selection( factor( ys ) ~ xs, list( yo1 ~ xo1, yo2 ~ xo2 ), method="model.frame" )
selection( factor( ys, labels = c( "no", "yes" ) ) ~ xs,
   list( yo1 ~ xo1, yo2 ~ xo2 ), method="model.frame" )

# the case without intercepts 
cat("Now run tobit5 without intercepts\n")
print(coef(selection( ys ~ xs - 1, list( yo1 ~ xo1 - 1, yo2 ~ xo2 - 1))))
# return just the model.frame
selection( ys ~ xs - 1, list( yo1 ~ xo1 - 1, yo2 ~ xo2 - 1 ),
   method = "model.frame" )

## ------- Tobit-2 exmple -----------
vc <- diag(2)
vc[2,1] <- vc[1,2] <- -0.7
eps <- rmvnorm(N, rep(0, 2), vc)
xs <- runif(N)
ys <- xs + eps[,1] > 0
xo <- runif(N)
yo <- (xo + eps[,2])*(ys > 0)
xs[sample(N, NNA)] <- NA
ys[sample(N, NNA)] <- NA
xo[sample(N, NNA)] <- NA
yo[sample(N, NNA)] <- NA
testTobit2TwoStep <- selection(ys~xs, yo ~xo, method="2step")
print( testTobit2TwoStep )
print( summary( testTobit2TwoStep ) )
print( coef( testTobit2TwoStep ), digits = 5 )
print( coef( testTobit2TwoStep, part = "outcome" ), digits = 5 )
print( coef( summary( testTobit2TwoStep ) ), digits = 5 )
print( coef( summary( testTobit2TwoStep ), part = "outcome" ), digits = 5 )
print( vcov( testTobit2TwoStep ), digits = 5 )
print( vcov( testTobit2TwoStep, part = "outcome" ), digits = 5 )
print( testTobit2TwoStep$invMillsRatio )
print( fitted( testTobit2TwoStep, part = "outcome" ), digits = 5 )
print( fitted( testTobit2TwoStep, part = "selection" ), digits = 5 )
print( residuals( testTobit2TwoStep, part = "outcome" ), digits = 5 )
print( residuals( testTobit2TwoStep, part = "selection", type = "response" ),
   digits = 5 )
print( model.matrix( testTobit2TwoStep, part = "outcome" ), digits = 5 )
print( model.matrix( testTobit2TwoStep, part = "selection" ), digits = 5 )
print( model.frame( testTobit2TwoStep ), digits = 5 )

testTobit2Ml <- selection(ys~xs, yo ~xo, method="ml")
print( testTobit2Ml )
print( summary( testTobit2Ml ) )
print( coef( testTobit2Ml ), digits = 5 )
print( coef( testTobit2Ml, part = "outcome" ), digits = 5 )
print( coef( summary( testTobit2Ml ) ), digits = 5 )
print( coef( summary( testTobit2Ml ), part = "outcome" ), digits = 5 )
print( vcov( testTobit2Ml ), digits = 5 )
print( vcov( testTobit2Ml, part = "outcome" ), digits = 5 )
print( fitted( testTobit2Ml, part = "outcome" ), digits = 5 )
print( fitted( testTobit2Ml, part = "selection" ), digits = 5 )
print( residuals( testTobit2Ml, part = "outcome" ), digits = 5 )
print( residuals( testTobit2Ml, part = "selection" ), digits = 5 )
mmsTestTobit2Ml <- model.matrix( testTobit2Ml, part = "selection" )
print( mmsTestTobit2Ml, digits = 5 )
mmoTestTobit2Ml <- model.matrix( testTobit2Ml, part = "outcome" )
print( mmsTestTobit2Ml, digits = 5 )
mfTestTobit2Ml <- model.frame( testTobit2Ml )
print( mfTestTobit2Ml, digits = 5 )
# ML with model.matrices returned
testTobit2MlMm <- selection( ys ~ xs, yo ~ xo, method = "ml", 
   xs = TRUE, xo = TRUE )
mmsTestTobit2MlMm <- model.matrix( testTobit2MlMm, part = "selection" )
attributes( mmsTestTobit2Ml )$assign <- NULL
all.equal( mmsTestTobit2Ml, mmsTestTobit2MlMm )
mmoTestTobit2MlMm <- model.matrix( testTobit2MlMm, part = "outcome" )
attributes( mmoTestTobit2Ml )$assign <- NULL
all.equal( mmoTestTobit2Ml, mmoTestTobit2MlMm )
# ML with model.frames returned
testTobit2MlMf <- selection(ys~xs, yo ~xo, method="ml", mfs = TRUE, mfo = TRUE)
mfTestTobit2MlMf <- model.frame( testTobit2MlMf )
all.equal( mfTestTobit2Ml, mfTestTobit2MlMf )

# return just the model.frame
selection( ys~xs, yo ~xo, method = "model.frame" )

# factors as dependent variable (from Achim Zeileis)
selection( ys ~ xs, yo ~ xo, method = "2step" )
selection( factor( ys ) ~ xs, yo ~ xo, method = "2step" )
selection( factor( ys, labels = c( "no", "yes" ) ) ~ xs, yo ~ xo,
   method = "2step" )
selection( ys ~ xs, yo ~ xo )
selection( factor( ys ) ~ xs, yo ~ xo )
selection( factor( ys, labels = c( "no", "yes" ) ) ~ xs, yo ~ xo )
# return just the model.frame
selection( ys ~ xs, yo ~ xo, method = "model.frame" )
selection( factor( ys ) ~ xs, yo ~ xo, method = "model.frame" )
selection( factor( ys, labels = c( "no", "yes" ) ) ~ xs, yo ~ xo,
   method = "model.frame" )

# the case without intercepts (by Lucas Salazar)
cat("Now run tobit2 without intercepts\n")
print(coef(selection( ys ~ xs - 1, yo ~ xo - 1)))
# return just the model.frame
selection( ys ~ xs - 1, yo ~ xo - 1, method = "model.frame" )

# NA-s in data frames (Nelson Villoria)
vc <- diag(2)
vc[2,1] <- vc[1,2] <- -0.8
eps <- rmvnorm(N, rep(0, 2), vc)
xs <- runif(N)
ys <- xs + eps[,1] > 0
xo <- runif(N)
yo <- (xo + eps[,2])*(ys > 0)
xs[sample(N, NNA)] <- NA
ys[sample(N, NNA)] <- NA
xo[sample(N, NNA)] <- NA
yo[sample(N, NNA)] <- NA
data <- data.frame(ys, xs, yo, xo)
rm(eps, xs, ys, xo, yo)
testTobit2ML <- selection(ys~xs, yo ~xo, data=data, method="ml")
print(summary(testTobit2ML))

## Raphael Abiry: does 'start' argument work?
init <- coef(testTobit2ML)
testTobit2ML <- selection(ys~xs, yo ~xo, data=data, method="ml", start=init)
print(summary(testTobit2ML))
                           # Note: should be only 1 iteration
