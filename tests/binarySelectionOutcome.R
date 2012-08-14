
options(digits=5)

## Leeman Lucas (and many others): binary outcome

set.seed(0)
N <- 500
rho <- 0.7
library(mvtnorm)
eps <- rmvnorm(N, c(0,0), matrix(c(1,rho,rho,1), 2, 2))
xs <- runif(N)
ysX <- 3*xs + eps[,1]
ys <- ysX > 0
xo <- runif(N)
yoX <- -1 + 2*xo + eps[,2]
yo <- factor((yoX > 0)*(ys > 0))
                           # binary outcome, only observable if ys>0
print(table(ys, yo, exclude=NULL))
library(sampleSelection)
ss <- selection(ys~xs, yo ~xo)
print(summary(ss))

# binary outcome NA if unobserved
yo[ !ys ] <- NA
print(table(ys, yo, exclude=NULL))
ssN <- selection(ys~xs, yo ~xo)
print(summary(ssN))
all.equal(ss,ssN)

# binary outcome logical
yo <- yoX > 0 & ys
print(table(ys, yo, exclude=NULL))
ssL <- selection(ys~xs, yo ~xo)
print(summary(ssL))
all.equal(ss,ssL)

# binary outcome logical and NA if unobserved
yo[ !ys ] <- NA
print(table(ys, yo, exclude=NULL))
ssLN <- selection(ys~xs, yo ~xo)
print(summary(ssLN))
all.equal(ssL,ssLN)
