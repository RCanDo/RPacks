########################################################################################################################—•°
## FUNCTIONS HERE    {EfficiencyCurves}
##  ##coalesce()
##  efficiency()
##
## DEPENDENCIES
##
## TODO
##
########################################################################################################################—•°


########################################################################################################################—•°
#coalesce = function(...){  ## to be moved to MD package
#ll = list(...)
#Find(Negate(is.null),ll)
#}
##   coalesce(NULL,"c",2,"a")
##   coalesce(NULL,2,"a")
##   coalesce(NULL,NULL,2,"a")

########################################################################################################################—•°
efficiency = function(x,...){UseMethod("efficiency")}
########################################################################################################################—•°
efficiency.default = function(x,margin=2,cum=FALSE){
###################################################################################################—•°
## Calculates efficiency for a vector x in a sense of x/sum(x),
## i.e. each entry is thought as a number of successes from the total number of trials equal to sum(x).
## This is obviously the same as normalisation of the vector (in the norm sum(x)).
## If cum = TRUE then cumulated efficiency is calculated i.e. cumsum(x)/sum(x), hence the last element
## of the result is always 1.
##
## If x is a matrix then this function is applied to each column (default) separately.
## In general the basic function for vector, say f(), is passed to
##    apply(X=x,MARGIN=margin,FUN=f)  thus we may calculate efficiency
## in the direction (dimension) indicated by 'margin' as in apply().
## The difference is that in case of 2-dimensions, length(dim(x)) = 2, and margin=1
## (we need to get efficiencies for rows)
## efficiency() additionaly transpose the result to be consistent with input (unlike apply()).
## In case of more dimensions result is returned as for apply() — dimensions rearranged (see ?apply).
###################################################################################################—•°
   f <- function(y){if(cum){cumsum(y)/sum(y)}else{y/sum(y)}}
   if(is.null(dim(x))){
      f(x)
   }else{
      x <- apply(x,margin,FUN=f)
      if( length(dim(x))==2 & margin==1 ){ t(x) }else{ x }
   }
}
###################################################################################################—•°
## EXAMPLES ##
dummy = function(){
efficiency(2)
efficiency(1:4)
efficiency(1:4,cum=TRUE)
mm <- matrix(1:12,ncol=3) ; colnames(mm) <- paste0("g",1:3) ; rownames(mm) <- paste0("t",1:4)
efficiency(mm)
efficiency(mm,cum=TRUE)
efficiency(mm,cum=TRUE,margin=2)  ## the same
efficiency(mm,cum=TRUE,margin=1)
efficiency(mm,margin=1)
}
rm(dummy)
########################################################################################################################—•°
efficiency.smooth_gam = function(sg,cum=FALSE){
###################################################################################################—•°
## Calculates efficiency for a vector in a sense of x/sum(x)
###################################################################################################—•°
   succ <- if(cum){
      if(is.null(sg$successes_cum)){ apply(sg$successes,2,cumsum)
      }else{ sg$successe_cum }
   }else{ sg$successes }
   succ / sg$trials
}
## EXAMPLES ##
## see EXAMPLES for smooth_gam().
########################################################################################################################—•°

#########################################################################################################################—•°
## moeved to SumMod package
#########################################################################################################################—•°
#models_table = function(x,...){UseMethod("models_table")}
#########################################################################################################################—•°
#models_table.smooth_gam = function(sg){
#   cbind(  coeff = t(sapply(sg$models_list,'[[',"coefficients"))
#      , rank = sapply(sg$models_list,'[[',"rank")
#      , nl.df = sapply(sg$models_list,'[[',"nl.df")
#      , df.residul = sapply(sg$models_list,'[[',"df.residual")
#      , df.null = sapply(sg$models_list,'[[',"df.null")
#      , deviance = sapply(sg$models_list,'[[',"deviance")
#      , null.deviance = sapply(sg$models_list,'[[',"null.deviance")
#      , aic = sapply(sg$models_list,'[[',"aic")
#      , nl.chisq = sapply(sg$models_list,'[[',"nl.chisq")
#      , iter = sapply(sg$models_list,'[[',"iter")
#      )
#}
#########################################################################################################################—•°
