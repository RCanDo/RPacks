## ---------------------------------------------------------------------------------------------------------------------���
## FUNCTIONS HERE          {SumMod}
##  panova()
##  panova.default()
##
## DEPENDENCIES
##
## ---------------------------------------------------------------------------------------------------------------------���

## ---------------------------------------------------------------------------------------------------------------------���
panova <- function(model,...){UseMethod("panova")}

## ---------------------------------------------------------------------------------------------------------------------���
panova.default <- function(model,...){

   if(!is.null(model$panova)){
      result <- model$panova
   }else{
      warning("This object has neither 'panova' entry nor defined procedure to derive such object.")
      result <- NULL
   }
   class(result) <- union(class(result),"panova")
   result

}##----END----##

## ---------------------------------------------------------------------------------------------------------------------���
