## ---------------------------------------------------------------------------------------------------------------------—•°
## GAM utils.
## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {dm}
##  success_failure_matrix()
##  univariate_gam_binomial_r2()
##  gof_binomial()        #! change it !!!
##
## DEPENDENCIES
##  whileif()
##
## REMARKS/WARNINGS
##  See other nodes and list of nodes:
##  vars.node.R
##  data.node.R
##  model.node.R
##
## TODO
## •
##
## DESCRIPTION
##  Helper functions for use with GAM models.
## ---------------------------------------------------------------------------------------------------------------------—•°

success_failure_matrix <- function(Y, ceil=NULL, as.integer=NULL){
## -----------------------------------------------------------------------------—•°
## necessary tranformation of Y for GAM modelling with  family=binomial(link=...)
##  Y                 integers or floats; by integers we mean  `round(Y) == Y`
##  ceil=NULL         relevant only when as.integer=TRUE; if NULL takes value of max(Y);
##                    ...
##  as.integer=NULL   logical: treat Y as integers (round them) or not;
##                    if NULL then takes value of `all(round(Y)==Y)`;
## -----------------------------------------------------------------------------—•°
as.integer <- whileif(as.integer, ifnull=(all(round(Y)==Y) || any(Y>1)))
if(as.integer){
    Y <- pmax(Y, 0)
    Y <- round(Y)
    ceil <- if(is.null(ceil)) max(Y) else ceil
    Y <- cbind(succs = Y, fails = ceil - Y)
}else{
    library(fractional)
    Y <- fractional(Y)
    succs = numerators(Y)
    trials = denominators(Y)
    Y <- cbind(succs = succs, fails = trials - succs)
}
Y
}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°

univariate_gam_binomial_r2 <- function(X, Y,
    transform=FALSE,
    s=TRUE,
    uniques=7,
    ...){
## -----------------------------------------------------------------------------—•°
## ! for use with  mgcv  package !
## Returns only r2 for assessing how single X is good in explaining Y.
## Y must be already transformed if transform=FALSE;
##  X                  candidate predictor variable for target Y;
##  Y                  target variable;
##  transform = FALSE  should be Y transformed to successes/failures matrix?
##                     default is FALSE i.e. it is assumed that Y is already transformned;
##  s = TRUE           should s(X) be applied? see also:
##  uniques = 7        how many unique values X should have to apply s(X)
##  ...                passed to success_failure_matrix() if relevant, i.e. `transform=TRUE`
##
## -----------------------------------------------------------------------------—•°
Y <- if(transform) success_failure_matrix(Y, ...) else Y
result <- tryCatch({
        if(s && (length(unique(X)) >= uniques) ){
            model <- mgcv::gam(Y ~ s(X), family=binomial(link=logit))
        }else{
            model <- mgcv::gam(Y ~ X, family=binomial(link=logit))
        }
        return(summary(model)$r.sq)
    },
    error = function(e){ print(e); return(NA)}
    )
result
}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°


## ---------------------------------------------------------------------------------------------------------------------—•°
## EXAMPLES ############################################################################################################—•°
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
dummy = function(){
## This is dummy function - it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## - in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
## -------------------------------------------------------------------------------------------------—•°
 ## RELOADER - before it works you need to source("RCanDo.R"); it's best to use DM.R within pack's dir.
 loadPacksAK("DM")
## -------------------------------------------------------------------------------------------------—•°
set.seed(123)
datfram <- data.frame(
    yy = ((1:30) + sample(0:10, 30, replace=T)),
    xx = 1:30
    )
datfram
plot(yy ~ xx, data=datfram)

Yb <- success_failure_matrix(datfram$yy)

univariate_gam_binomial_r2(datfram$xx, Yb)
univariate_gam_binomial_r2(datfram$xx, Yb, s=F)
univariate_gam_binomial_r2(datfram$xx, datfram$yy, transform=T)
univariate_gam_binomial_r2(datfram$xx, datfram$yy, transform=T, s=F)

## -------------------------------------------------------------------------------------------------—•°
df1 <- datfram
df1$xx[25:30] <- NA
clear(df1, full=T)

df2 <- df1
df2$yb <- Yb
dim(df2)
names(df2)
df2$yb

clear(df2, full=T)
column <- "yb"
df2[!is.na(df2[column]),]

dim(df2[["xx"]])
df2[column][3,]

df2[[column]][t(rbind(c(1, 3, 5, 7), c(1, 2, 1, 2)))] <- NA
df2

which(is.na(df2[[column]]), arr.ind=T)

apply(df2[[column]], 1, function(x)as.logical(sum(is.na(x))))
clear(df2, full=T)

clear(df2, NAs=1L)

length(unique(df2$yb))
## ---------------------------------------------------------------------------------------------------------------------—•°
}; rm(dummy)
