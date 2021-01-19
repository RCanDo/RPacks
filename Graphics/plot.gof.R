## ---------------------------------------------------------------------------------------------------------------------—•°
## Goodness Of Fit for models' objects
## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {package}
##  fun1( x , y , ... )
##  fun2( ... )
##
## DEPENDENCIES
##  funa()        {package} file.R     [defalt {package} is the same; default file.R is the same as function name]
##  funb()
##
## REMARKS/WARNINGS
##
## TODO
## •
##
## DESCRIPTION
##  Extracts the most important things from model object to asses goodness of fit.
##
## -------------------------------------------------------------------------------------------------—•°
## AUTHOR: Arkadiusz Kasprzyk; rcando@int.pl
## ---------------------------------------------------------------------------------------------------------------------—•°

plot.gof <- function(gof, xlab="y", ylab=c("fitted", "residuals"),
    title=c("fitted vs true", "residuls vs true"), coldef="grey", ...){
## -------------------------------------------------------------------------------------------------—•°
##  gof    obj of class "gof" (Goodness Of Fit)
## -------------------------------------------------------------------------------------------------—•°

par(mfrow=c(2, 1))
marginsMy(); coldef(coldef)

plot(gof$y, gof$fitted, xlab=xlab, ylab=ylab[1], ...)
title(title[1])  ## not very good
abline(0, 1, col="red")

plot(gof$y, gof$residuals, xlab=xlab, ylab=ylab[2], ...)
title(title[2])  ## not very good
abline(0, 0, col="black")
}  ##----END----##


## ---------------------------------------------------------------------------------------------------------------------—•°
## EXAMPLES ------------------------------------------------------------------------------------------------------------—•°
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
dummy = function(){
## This is dummy function — it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## — in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
## -------------------------------------------------------------------------------------------------—•°
## RELOADER — before it works you need to source("RCanDo.R"); it's best to use {package_name}.R within pack's dir.
 loadPacksAK("{package_name}")
## -------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
}; rm(dummy)