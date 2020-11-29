## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {DM}
##  allnames()
##
## DEPENDENCIES
##
## TODO
## •
##
## ---------------------------------------------------------------------------------------------------------------------—•°

allnames <- function(x,dnams,...){
## ------------------------------------------------------------------------------------------------—•°
## Shorthand for:
##    ll <- list(names_1, names_2, ...)
##    names(ll) <- dnams
##    dimnames(x) <- ll
## where names_1, names_2, ... are names for rows, columns, ... of the x which in general is an atomic vector
## of  length = length(names_1)*length(names_2)*...  which may or may not have dimensions already defined (an array).
##
##    Arguments
##  x       atomic vector of any type;
##  dnams   character vector with names for dimensions;
##  ...     character vectors with names dimensions elements; numbers of vectors must be equal to length(dnams);
##
##    Result
##
##    Remark
## If  names_1, names_2, ..., names_k  are passed in place of ... then
##    length(x) = length(names_1)*length(names_2)*...*length(names_k)
##
##    Comment
## Avoid using it within a user defined functions as it is only shorthand intended for interactive use.
## It's anly little shorter then setting names via standard functions.
##
## ------------------------------------------------------------------------------------------------—•°
   ll <- list(...)
   names(ll) <- dnams
   if(is.null(dim(x))) dim(x)<-sapply(ll,length)
   dimnames(x) <- ll
   x
}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°

dummy = function(){

(x <- array(1:60,dim=c(5,4,3)))
(x <- allnames(x,LETTERS[1:3],letters[1:5],letters[11:14],letters[21:23]))

## is the same as

(x <- array(1:60,dim=c(5,4,3)))
ll <- list(letters[1:5],letters[11:14],letters[21:23])
names(ll) <- LETTERS[1:3]
dimnames(x) <- ll
x

## but in the first case you may not assign result
(x <- array(1:60,dim=c(5,4,3)))
allnames(x,LETTERS[1:3],letters[1:5],letters[11:14],letters[21:23])
x

## what may be useful when testing visual aspects of names but may be also inconvienient.

};rm(dummy)
