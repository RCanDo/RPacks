## ---------------------------------------------------------------------------------------------------------------------—•°
## Goodness Of Fit measures for models' objects
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
##  Extracts the most important things from model object i.e. Y, Yhat, residuals,
##  to assess goodness of fit measures.
##
## -------------------------------------------------------------------------------------------------—•°
## AUTHOR: Arkadiusz Kasprzyk; rcando@int.pl
## ---------------------------------------------------------------------------------------------------------------------—•°

gof <- function(x, ...){ UseMethod("gof") }

## ---------------------------------------------------------------------------------------------------------------------—•°
gof.gam = function(model)
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
##  RMSE                Root Mean Squared Error $\sqrt{\sum_i |y_i - yhat_i|^2 / n}$ -- everybody knows;
##  MAE                 Mean Absolute Error  $\sum_i |y_i - yhat_i| / n$ -- scale sensitive
##  MAEp                Mean Absolute Error % i.e. percentage change  $\frac{\sum_i |y_i - yhat_i|}{ \sum_i y_i }$ -- good, scale agnostic
##  MAPE                Mean Absolute Percentage Error $\sum_i \frac{|y_i - yhat_i|}{y_i} / n$ -- not good! overshoots meand
##  Max                 Maximum absolute error $\max_i(|y_i|)$
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
    sumY <- apply(Y, 1, sum)
    if(length(unique(sumY)) > 1){
        Yhat <- model$fitted.values
        Y <- Y[,1]/sumY
    }else{
        maxY <- sumY[1]                                                  #!!!
        Y <- Y[,1]
        Yhat <- model$fitted.values * maxY     ##                        #???
    }
}else{
    maxY <- max(Y)
    Yhat <- model$fitted.values
}
resids <- Y - Yhat

## -----------------------------------------------------------------------------—•°

n.obs <- length(Y)
df.null <- model$df.null
df.residual <- model$df.residual

n.vars <- length(colnames(model$model)) - 1
n.smooth <- length(model$smooth)

## -----------------------------------------------------------------------------—•°

TSS <- sum((Y-mean(Y))^2)
MSS <- sum((Yhat-mean(Yhat))^2)
RSS <- sum((resids-mean(resids))^2)

RMSE <- sqrt(TSS / n.obs)
MAE <- mean(abs(resids))
MAEp <- MAE / mean(Y)
MAPE <- mean(abs(resids)/Y)
Max <- max(abs(resids))

VTSS <- TSS / df.null                   ## Total
VMSS <- MSS / (df.null - df.residual)   ## Model
VRSS <- RSS / df.residual               ## Residual

adjR2 <- 1 - VRSS / VTSS
R2 <- 1 - RSS / TSS

deviance_explained = 1 - model$deviance / model$null.deviance

## -----------------------------------------------------------------------------—•°

result <- structure( list(
    y = Y, fitted = Yhat, residuals = resids,
    n.obs = n.obs, df.null = df.null, df.residual = df.residual, df.model = df.null - df.residual,
    n.vars = n.vars, n.smooth = n.smooth,
    ## Goodness Of Fit measures ("metrics")
    deviance_explained = deviance_explained,
    RMSE = RMSE, MAE = MAE, MAEp = MAEp, MAPE = MAPE, Max = Max,
    adjR2 = adjR2, R2 = R2
    ),
    class=c("gof.gam", "gof")
    )

return(result)

}  ##----END----##

## -------------------------------------------------------------------------------------------------—•°
gof.Gam = function(model){
## ---------------------------------------------------------
## "Gam" is a result of gam package.
## Returns "gof.gam" -- the same as `gof.gam()` method.
## ---------------------------------------------------------
gof <- gof.gam(model)
gof$n_smooth <- length(colnames(model$smooth))
gof
}

## ---------------------------------------------------------------------------------------------------------------------—•°

summary.gof.gam <- function(gg, round=3){
##
nams <- c( "adjR2", "R2", "deviance_explained",
    "Max", "MAE", "MAEp", "MAPE", "RMSE",
    "n.obs", "n.vars", "n.smooth",
    "df.null", "df.model", "df.residual" )

result <- round(unlist(gg[nams]), round)
class(result) <- c("summary.gof.gam", "summary.gof", "summary", "list")  ## but it's not a list !!!
result
}  ##----END----##

## -------------------------------------------------------------------------------------------------—•°
#summary.gof.Gam <- function(gg, round=3){    #! not necessary -- there is no "gof.Gam" only always "gof.gam"
#summary.gof.gam(gg, round)
#}

## -------------------------------------------------------------------------------------------------—•°
print.summary.gof <- function(sg){
for(n in names(sg)){
    cat(n, " : ", sg[n], "\n", sep="")
}
}

## -------------------------------------------------------------------------------------------------—•°
print.summary.gof.gam <- function(sgg){
for(n in names(sgg)){
    cat(n, " : ", sgg[n], "\n", sep="")
}
}

## -------------------------------------------------------------------------------------------------—•°
summary.gof.list <- function(...){
ll <- list(...)
stopifnot(all(sapply(ll, inherits, "summary.gof")))
structure(ll, class=c("summary.gof.list", "list"))
}

## -------------------------------------------------------------------------------------------------—•°
summary.gof.gam.list <- function(...){
ll <- list(...)
stopifnot(all(sapply(ll, inherits, "summary.gof.gam")))
structure(ll, class=c("list", "summary.gof.gam.list", "summary.gof.list"))
}

## -------------------------------------------------------------------------------------------------—•°
summary.summary.gof.list <- function(sgl){
sgl.df <- as.data.frame(t(sapply(lapply(sgl, '['), unlist)))  ## tricky staff found by trials and errors
class(sgl.df) <- union(class(sgl.df), "summary.gof.table")
sgl.df
}

## -------------------------------------------------------------------------------------------------—•°
summary.summary.gof.gam.list <- function(sggl){
## the same
sggl.df <- as.data.frame(t(sapply(lapply(sggl, '['), unlist)))  ## tricky staff found by trials and errors
class(sggl.df) <- union(class(sggl.df), "summary.gof.gam.table")
sggl.df
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
