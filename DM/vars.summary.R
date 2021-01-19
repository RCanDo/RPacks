## ---------------------------------------------------------------------------------------------------------------------•°
## FUNCTIONS HERE
##  vars.table()
##
## DEPENDENCIES
##  
## ---------------------------------------------------------------------------------------------------------------------•°
agg.table <- function(datfram,
    summary=list(
        'nas'=quote(sum(is.na(x))),
        oks=quote(sum(!is.na(x))),
        'oks/all'=quote(sum(!is.na(x))/length(x)),
        class=quote(class(x))
        ),
    sort=NULL, decreasing=TRUE){
## -------------------------------------------------------------------------------------------------—•°
##
## -------------------------------------------------------------------------------------------------—•°
sum.df <- with( datfram,
{
 as.data.frame(lapply(summary, function(fun)sapply(datfram, function(x)eval(fun))))
}
)
sum.df$id <- 1:nrow(sum.df)
if(!is.null(sort)){
    sum.df <- sum.df[order(sum.df[,sort], decreasing=decreasing),]
}
sum.df

}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°


## ---------------------------------------------------------------------------------------------------------------------—•°
## EXAMPLES ############################################################################################################—•°
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
dummy = function(){
## This is dummy function — it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## — in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
## -------------------------------------------------------------------------------------------------—•°

 ## RELOADER ? before it works you need to source("PacksAK.R"); it's best to use DM.R within pack's dir.
 loadPacksAK("DM")

## -------------------------------------------------------------------------------------------------—•°

vars.table(data.4, sort="oks")


fun <- function(x){
    df0 <- data.frame(y=y, x=x)
    summary(lm(y ~ x, data=df0))$r.squared
}

fun2 <- function(x){
    summary(lm(ING_Sugar ~ x))$r.squared
}

data.4.n <- capply(data.4, class = c("numeric"))
nulls.table(data.4.n)
data.4.n <- subtable(data.4.n, full="ING_Sugar", OKs=1)
nulls.table(data.4.n)
y = data.4$ING_Sugar

fun <- function(x){
    sum(x, na.rm=T)
}

vars.table(data.4.n, summary=list(r=quote(fun(x))))

## ---------------------------------------------------------------------------------------------------------------------•°
}; rm(dummy)