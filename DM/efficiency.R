## ---------------------------------------------------------------------------------------------------------------------—•°
## efficiency() generic and methods.
## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {DM}
##  efficiency()
##  efficiency.default()
##  efficiency.smooth_gam()
##
## DEPENDENCIES
##
## TODO
## •
##
## DESCRIPTION
##  The aim, the idea, problems, solutions... briefly!, concerning the whole set of functions
##
## -------------------------------------------------------------------------------------------------—•°
## AUTHOR: Arkadiusz Kasprzyk; akasp@...
## ver. 1.0
## PATH: D:\ROBOCZY\...
## start: 2017-03-01    last: 2017-09-26
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
#coalesce = function(...){  ## to be moved to DM package
#ll = list(...)
#Find(Negate(is.null),ll)
#}
##   coalesce(NULL,"c",2,"a")
##   coalesce(NULL,2,"a")
##   coalesce(NULL,NULL,2,"a")

## ---------------------------------------------------------------------------------------------------------------------—•°
efficiency = function(x,...){UseMethod("efficiency")}
## ---------------------------------------------------------------------------------------------------------------------—•°
efficiency.default = function(x,margin=2,cum=FALSE){
## ------------------------------------------------------------------------------------------------—•°
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
## ------------------------------------------------------------------------------------------------—•°
   f <- function(y){if(cum){cumsum(y)/sum(y)}else{y/sum(y)}}
   if(is.null(dim(x))){
      f(x)
   }else{
      x <- apply(x,margin,FUN=f)
      if( length(dim(x))==2 & margin==1 ){ t(x) }else{ x }
   }
}
## ------------------------------------------------------------------------------------------------—•°
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
## ---------------------------------------------------------------------------------------------------------------------—•°
efficiency.smooth_gam = function(sg,cum=FALSE){
## ------------------------------------------------------------------------------------------------—•°
## efficiency method for "smooth_gam".
## It uses matrix of efficiencies from the object.
## If cum=TRUE but
## ------------------------------------------------------------------------------------------------—•°
   if(cum){
      if(is.null(sg$efficiencies_cum)){

         cnams <- colnames(sg$successes)
         times <- rownames(sg$successes)

         if(!all(apply(sg$trials,2,function(x){min(x)==max(x)}))){  ## not all equal in a column
            effs <- sg$successes / sg$trials
            sg$trials <- apply(sg$trials,2,function(x)rep(max(x),nrow(sg$trials)))
            sg$successes <- round(sg$trials*effs)
         }
         sg$successes <- apply(sg$successes,2,cumsum)

         if(!"net_curve"%in%cnams){
            sg$successes <- addmargins(sg$successes,2)
               sg$trials <- addmargins(sg$trials,2)
                cnams <- c(cnams,"Sum")
         }

         effs <- sg$successes / sg$trials

         if(!is.null(sg$weights_groups)){
            ngr <- length(sg$weights_groups)
            effs <- cbind(effs,effs[,1:ngr]%*%cbind(sg$weights_groups))
            cnams <- c(cnams,"weighted_curve")
         }

         dimnames(effs) <- list(time=times,group=cnams)

         effs

      }else{
         sg$efficiencies_cum
      }
   }else{
      sg$efficiencies
   }
}
## EXAMPLES ##
## see EXAMPLES for smooth_gam().
## ---------------------------------------------------------------------------------------------------------------------—•°

