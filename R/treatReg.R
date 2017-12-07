treatReg <- function(selection, outcome,
                     data=sys.frame(sys.parent()),
                     mfs=TRUE, mfo=TRUE,
                      ...) {
   ## Heckman-style treatment effect models
   ## selection:   formula
   ##              LHS: must be convertable to two-level factor (e.g. 0-1, 1-2, "A"-"B")
   ##              RHS: ordinary formula as in lm()
   ## outcome:     formula
   ##              should include selection outcome
   ## ys, xs, yo, xo, mfs, mfo: whether to return the response, model matrix or
   ##              the model frame of outcome and selection equation(s)
   ## First the consistency checks
   ## ...          additional arguments for tobit2fit and tobit5fit
   res <- selection(selection, outcome, data=data,
                    type="treatment",
                    mfs=mfs, mfo=mfo,
                    ...)
   return(res)
}
