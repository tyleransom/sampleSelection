### Detect the tobit model type based on how selection and
### outcome formulas are set up.
### returns the model type

detectModelType <- function(selection, outcome) {
   type <- 0
   if(inherits(outcome, "formula")) {
      type <- 2
   } else if(inherits(outcome, "list")) {
      if(length(outcome) == 1) {
         outcome <- outcome[[1]]
         type <- 2
      } else {
         type <- 5
      }
   } else {
      stop( "argument 'outcome' must be a formula or a list of formulas")
   }
   ## Now distinguish between 2 and treatment
   if(type == 2) {
      ys <- as.character(selection)[2]
      if(grepl(paste("\\<", ys, "\\>", sep=""),
               as.character(outcome)[3]))
         type <- "treatment"
   }
   return(type)
}
