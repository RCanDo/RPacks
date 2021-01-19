## ---------------------------------------------------------------------------------------------------------------------—•°
## Turning OH encoded variable (many columns) into a factor (one column)
## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {DM}
##  oh_to_factor_0()
##  oh_to_factor()
##
## DEPENDENCIES
##  ...()        DM::whileif.R
##
## REMARKS/WARNINGS
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
## PATH: ./DM/subtable.R
## start: 2020-12-23    last: 2020-12-23
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
is.onehot <- function(datfram){
## -------------------------------------------------------------------------------------------------—•°
all(datfram == 0 | datfram == 1) &&
all(apply(datfram, 1, sum) == 1)
}
## ---------------------------------------------------------------------------------------------------------------------—•°


## ---------------------------------------------------------------------------------------------------------------------—•°
onehot_to_factor <- function(
    datfram,
    append=FALSE,  ## NULL, F/T, "", character -- name of the new factor appended to `datfram`
    levels=NULL,
    as.factor=TRUE
){
## -------------------------------------------------------------------------------------------------—•°
## Assumptions on `datfram`
## 1. no NAs
## 2. sum of rows is always 1
## 3. only 0 and 1
## -------------------------------------------------------------------------------------------------—•°

levels <- if(is.null(levels)) names(datfram) else levels
if(length(levels) != ncol(datfram)){
    stop("`levels` must have exactly the same nr of elements as there are columns of `datfram`")
}

if(!is.onehot(datfram)){
    stop("`datfram` must be true one-hot encoded variable, i.e. each row must sum to exactly 1,
  each value must be 0 or 1, and cannot be NA values.")
}

{
    w <- which(datfram==1, arr.ind=TRUE)
    w <- w[order(w[, 1]), ]
    res <- levels[w[, 2]]
}
res <- if(as.factor) factor(res) else res

append <- whileif(append, ifnull="", iffalse="", iftrue="f1")    ## ??
if(append != ""){
    res <- cbind(datfram, append=res)
}

}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°


## ---------------------------------------------------------------------------------------------------------------------—•°
onehot_to_factor_ <- function(
    datfram,
    columns,
    append=FALSE,  ## NULL, F/T, "", character -- name of the new factor appended to `datfram`
    levels=NULL,
    as.factor=TRUE
){
## -------------------------------------------------------------------------------------------------—•°

}  ##----END----##
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
 loadPacksAK("DM")
## -------------------------------------------------------------------------------------------------—•°

dat <- data.frame(
a = c(1,0,0,1,0),
b = c(0,1,1,0,0),
c = c(0,0,0,0,1)
)
dat

is.onehot(dat)

w <- which(dat==1, arr.ind=T)
w
w[order(w[,1]), 2]

names(dat)[w[order(w[,1]), 2]]
## -------------------------------------------------------------------------------------------------—•°

any(apply(dat, 1, sum) != 1)

## -------------------------------------------------------------------------------------------------—•°
any(apply(data.1[grep("^CB_", names(data.1), value=TRUE)], 1, sum) != 1)

## -------------------------------------------------------------------------------------------------—•°
df1 <- data.1[grep("^CB_", names(data.1), value=TRUE)]
names(df1) <- paste("CB_", 1:length(grep("^CB_", names(data.1), value=TRUE)))

which(df1==1, arr.ind=T)

subtable(df1, full="")   ## ???

## ---------------------------------------------------------------------------------------------------------------------—•°
}; rm(dummy)
