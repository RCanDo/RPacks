## ---------------------------------------------------------------------------------------------------------------------—•°
## Goodness Of Fit for models' objects
## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {package}
##  gof( ... )
##  gof.gam( ... )
##  gof.Gam()
##  summary.gof.gam()
##  print.summary.gof.gam()
##
## DEPENDENCIES
##  whileif()
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

gof <- function(x, ...){ UseMethod("gof") }

## ---------------------------------------------------------------------------------------------------------------------—•°
gof.gam = function(model, ...)
{
## -------------------------------------------------------------------------------------------------—•°
## Goodness of fit for gam model.
## "gam" is a result of mgcv package.
## "Gam" is a result of gam package. See gof.Gam() below.
##    Arguments
##  model       "gam" model -- from gam or mgcv package
##
##    Result
##  List of class `c("gof.gam", "gof")` with entries:
##  y                   Y variable; in case of binomial model it is the 1st column of the Y
##                      passed to the model, i.e. nr of "successes";
##  fitted              fitted values (in original space i.e. where Y is given)
##  residuals           residuals from the model (in original space)
##  n_obs               number od observations
##  n_vars              number of all explanatory variables used for model
##  n_smooth            number of explanatory variables for which smooth term is non-zero
##  df.null             degrees of freedom for null model i.e. n-1
##  df.residual         degrees of freedom for residuals
##  df.model            df.null - df.residuals
##  deviance_explained  how much of the deviance was explained in %
##  MAE                 Mean Absolute Error
##  adjR2               adjusted R2
##  R2                  R2
##
##    Description/Comments/Remarks
## This is primarily for Y in the binomial form, i.e. Y_i = [succ_i, fails_i] !!!
## But is generalised for all types of Y.
## -------------------------------------------------------------------------------------------------—•°

Yname <- as.character(model$formula)[2]
Y <- model$model[[Yname]]

## -----------------------------------------------------------------------------—•°
## the basic "problem"

if(model$family$family=="binomial"){
    maxY <- max(apply(Y, 1, sum))
    Y <- Y[,1]
    Yhat <- model$fitted.values * maxY     ##                        #???
}else{
    maxY <- max(Y)
    Yhat <- model$fitted.values
}
resids <- Y - Yhat

## -----------------------------------------------------------------------------—•°

n <- length(Y)
df.null <- model$df.null
df.residual <- model$df.residual

n_vars <- length(colnames(model$model)) - 1
n_smooth <- length(model$smooth)

## -----------------------------------------------------------------------------—•°

TSS <- sum((Y-mean(Y))^2)
MSS <- sum((Yhat-mean(Yhat))^2)
RSS <- sum((resids-mean(resids))^2)

MAE <- sum(abs(resids))/n

VTSS <- TSS / df.null
VMSS <- MSS / (df.null - df.residual)
VRSS <- RSS / df.residual

adjR2 <- 1 - VRSS / VTSS
R2 <- 1 - RSS / TSS

deviance_explained = 1 - model$deviance / model$null.deviance

## -----------------------------------------------------------------------------—•°

result <- structure( list(
    y = Y, fitted = Yhat, residuals = resids,
    n_obs = n, df.null = df.null, df.residual = df.residual, df.model = df.null - df.residual,
    n_vars = n_vars, n_smooth = n_smooth,
    deviance_explained = deviance_explained,
    MAE = MAE,
    adjR2 = adjR2,
    R2 = R2),
    class=c("gof.gam", "gof")
    )

return(result)

}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°

summary.gof.gam <- function(gg, round=4, ...){

nams <- c( "adjR2", "R2", "deviance_explained", "MAE",  "n", "n_vars", "n_smooth",
    "df.null", "df.residual", "df.model" )

result <- round(unlist(gg[nams]), round)
class(result) <- c("summary.gof.gam", "summary")
result

}  ##----END----##

## -------------------------------------------------------------------------------------------------—•°
print.summary.gof.gam <- function(sgg, ...){
for(n in names(sgg)){
    cat(n, " : ", sgg[n],"\n", sep="")
}
}

## ---------------------------------------------------------------------------------------------------------------------—•°
gof.Gam = function(model, ...){
## Gam is a result of gam package.
gof <- gof.gam(model)
gof$n_smooth <- length(colnames(model$smooth))
gof
}

## ---------------------------------------------------------------------------------------------------------------------—•°

#print.summary.gof.gam.binomial <- function(ggb, ...){
#}

## ---------------------------------------------------------------------------------------------------------------------—•°


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
