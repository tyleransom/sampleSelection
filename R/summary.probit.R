summary.probit <- function(object, ...) {
   ## summary for probit -- adds Likelihood Ratio Test to summary.maxLik
   summaryML <- maxLik:::summary.maxLik(object, ...)
   pchi2 <- pchisq(object$LRT$LRT, object$LRT$df, lower.tail=FALSE)
   a <- c(summaryML,
          LRT=list(c(object$LRT, pchi2=pchi2)),
          nParam=nParam(object),
          nObs=nObs(object),
          N0=object$param$N0,
          N1=object$param$N1,
          df=object$df,
          levels=list(object$param$levels))
   class(a) <- c("summary.probit", class(summaryML))
   a
}
