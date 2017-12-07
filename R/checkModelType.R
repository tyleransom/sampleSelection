### check whether arguments fit the supplied/guessed model type

checkModelType <- function(type, selection, outcome) {
   chr <- function(formula) {
      ## convert 'formula' objects into nice text
      as.character(formula)
   }
   ## general checks
   if(!(type %in% c(2,5,"treatment"))) {
      stop("'type' in 'selection()' must be either '2', '5', or ",
           "'teatment'\n", "Currently ", type)
   }
   ##
   if(!inherits( selection, "formula" )) {
      stop( "argument 'selection' must be a formula in function 'selection()'" )
   }
   if( length( selection ) != 3 ) {
      stop( "argument 'selection' must be a 2-sided formula in function 'selection()'" )
   }
   ##
   if(inherits(outcome, "formula")) {
      if( length( outcome ) != 3 ) {
         stop( "argument 'outcome' must be a 2-sided formula in function 'selection()'" )
      }
      if(type == 5) {
         stop( "type '5' selection models need dual outcomes as\n",
              "a list of two formulas")
      }
   }
   ##
   else if(inherits(outcome, "list")) {
      if(length(outcome) == 1) {
         if(!(type %in% c(2, "treatment"))) {
            stop( "type '5' selection models need dual outcomes as\n",
                 "a list of two formulas")
         }
      }
      else if(length(outcome) == 2) {
         if(type != 5) {
            stop("type-2 and treatment models require a single outcome\n",
                 "in function 'selection()'.  Currently a list of length ",
                 length(outcome))
         }
         if(inherits(outcome[[1]], "formula")) {
            if( length( outcome[[1]] ) != 3 ) {
               stop("argument 'outcome[[1]]' must be a 2-sided formula in\n",
                    "in 'selection(", chr(outcome), chr(selection), ")'")
            }
         }
         else
            stop( "argument 'outcome[[1]]' must be a formula in function 'selection()'" )
         if(inherits(outcome[[2]], "formula")) {
            if( length( outcome[[2]] ) != 3 ) {
               stop("argument 'outcome[[2]]' must be a 2-sided formula in function 'selection()'" )
            }
         }
         else
            stop( "argument 'outcome[[2]]' must be a formula in function 'selection()'" )
      }
      else
         stop("argument 'outcome' must contain 1 or 2 components in function 'selection()'")
   }
   else
      stop("argument 'outcome' must be either a formula or a list of two formulas in function 'selection()'" )
   return(type)
   
}
