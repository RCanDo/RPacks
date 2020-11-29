## ---------------------------------------------------------------------------------------------------------------------—•°
## t method for arrays.
## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {DM}
##  t.array( x )
##
## DEPENDENCIES
##  unattr()
##
## REMARKS/WARNINGS
##
## TODO
## •
##
## DESCRIPTION
## There was no t method for arrays.
## -------------------------------------------------------------------------------------------------—•°
## AUTHOR: Arkadiusz Kasprzyk; akasp@...
## ver. 1.0
## PATH: D:\ROBOCZY\PK_Analitycy\R\PacksAK\DM\t.array.R
## start: 2017-08-20    last: 2017-08-20
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
t <- function(x,...) UseMethod("t")
## generic redefinced to allow more arguments
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
t.array <- function(x,swap=c(1,2)){
## ---------------------------------------------------------------------------------------------------------------------—•°
## There is no standard t method for arrays; moreover t(array) returns error.
## Here t.array() is defined in a sense that it makes transposition of all pages of array
## i.e. simply swaps rows and columns. In general case you may pass a series of dimensions which will be rearranged
## in reverse order; hence this is special case of aperm().
##
##    Arguments
##  x             array
##  swap=c(1,2)   dimensions to swap; vector of unique integers all of which must be in 1:length(dim(x));
##
##    Result
##  x          array with rows and columns swaped when 'swap' is default; in general dimensions given in swap
##             are rearranged in reversed order i.e. t.array() is a special case of aperm(x,perm) where
##                   dims = 1:length(dim(x));   dims[swap] <- rev(swap);   x <- aperm(x,dims)
##
##    Description/Comments/Remarks
## Retains attributes as t() does.
##
## ---------------------------------------------------------------------------------------------------------------------—•°

ndim <- length(dim(x))
dims <- seq_len(ndim)

stopifnot( length(swap) <= ndim , all(swap%in%dims) , max(table(swap))==1 )

attrs <- attributes(x)
attrs$dim <- NULL
attrs$dimnames <- NULL
x <- unattr(x)

dims[swap] <- rev(swap)
x <- aperm(x,dims)

attributes(x)[names(attrs)] <- attrs

x

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
## -------------------------------------------------------------------------------------------—•°
## RELOADER — before it works you need to source("PacksAK.R"); it's best to use {package_name}.R within pack's dir.
 loadPacksAK("DM")
## -------------------------------------------------------------------------------------------—•°

mm <- array(1:100,dim=c(10,5,2))
mm
t(mm)
t(mm,swap=c(1,3))
t(mm,c(2,3))

mm <- array(1:120,dim=c(5,4,3,2))
mm
t(mm)
t(mm,1:4)
t(mm,c(1,3))

};rm(dummy)
