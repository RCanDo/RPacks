########################################################################################################################—•°
## head method for arrays.
########################################################################################################################—•°
## FUNCTIONS HERE    {DM}
##  head.array( x , n=6 )
##
## DEPENDENCIES
##
## REMARKS/WARNINGS
##
## TODO
## •
##
## DESCRIPTION
## There was no head method for arrays.
####################################################################################################—•°
## AUTHOR: Arkadiusz Kasprzyk; akasp@...
## ver. 1.0
## PATH: D:\ROBOCZY\...
## start: 2017-08-15    last: 2017-08-15
########################################################################################################################—•°

#########################################################################################################################—•°
#head.matrix <- function(x,n=6L){
#########################################################################################################################—•°
### Standard head() gives only n elements of array if it has more then 2 diensions.
### This function gives n rows across all pages (dimensions further then 1, 2, i.e. rows and columns)
### of arrays regardless of number of dimensions.
###
###    Arguments
###  x          array
###  n = 6      integer; number of first rows to display; if negative then number of first rows to omit.
###
###    Result
###  x          array truncated to the first n rows if n>0 or without first n rows if n<0.
###
###    Description/Comments/Remarks
### It is slow as it uses eval(parse(text=txt)).
###
#########################################################################################################################—•°
#
#    stopifnot(length(n) == 1L)
#    n <- if (n < 0L) max(nrow(x) + n, 0L) else min(n, nrow(x))
#    x[seq_len(n), , drop = FALSE]
#
#
#d <- length(dim(x))
#
#attrs <- attributes(x)
#
#if(d==0){
#
#   attrs$names <- NULL
#
#   nr <- lenght(x)
#   rows <- seq_len(min(nr,abs(n)))*sign(n)
#   x <- x[rows]
#
#}else{
#
#   attrs$dim <- NULL
#   attrs$dimnames <- NULL
#
#   nr <- dim(x)[1]
#   rows <- seq_len(min(nr,abs(n)))*sign(n)
#
#   form <- paste("x[rows",paste(rep(",",d),collapse=""),"drop=FALSE]",sep="")
#   x <- eval(parse(text=form))
#}
#
#attributes(x)[names(attrs)] <- attrs
#x
#
#}  ##----END----##
#########################################################################################################################—•°

########################################################################################################################—•°
head.array <- function(x,n=6L){
########################################################################################################################—•°
## Standard head() gives only n elements of array if it has more then 2 dimensions.
## This function gives n rows across all pages (dimensions further then 1, 2, i.e. rows and columns)
## of arrays regardless of number of dimensions.
##
##    Arguments
##  x          array
##  n = 6      integer; number of first rows to display; if negative then number of last rows to omit.
##
##    Result
##  x          array truncated to the first n rows if n>0 or without last n rows if n<0.
##
##    Description/Comments/Remarks
## It is slow as it uses eval(parse(text=txt)).
## Does not retain attributes, as standard head() doesn't. This is bad thing...
##
########################################################################################################################—•°

stopifnot(length(n) == 1L)

d <- length(dim(x))

#attrs <- attributes(x)

if(d==0){

#   attrs$names <- NULL

   nr <- lenght(x)
   ###rows <- seq_len(min(nr,abs(n)))*sign(n)   ### another option for negative n — excluding first abs(n) rows.
   n <- if (n < 0L) max(nr + n, 0L) else min(n, nr)   ## this option is consistent with head.matrix()
   rows <- seq_len(n)
   x <- x[rows]

}else{

#   attrs$dim <- NULL
#   attrs$dimnames <- NULL

   nr <- dim(x)[1]
   ###rows <- seq_len(min(nr,abs(n)))*sign(n)   ### another option for negative n — excluding first abs(n) rows.
   n <- if (n < 0L) max(nr + n, 0L) else min(n, nr)   ## this option is consistent with head.matrix()
   rows <- seq_len(n)

   form <- paste("x[rows",paste(rep(",",d),collapse=""),"drop=FALSE]",sep="")
   x <- eval(parse(text=form))
}

#attributes(x)[names(attrs)] <- attrs
x

}  ##----END----##
########################################################################################################################—•°

########################################################################################################################—•°
## EXAMPLES ############################################################################################################—•°
########################################################################################################################—•°

########################################################################################################################—•°
dummy = function(){
## This is dummy function — it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## — in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
##############################################################################################—•°
## RELOADER — before it works you need to source("PacksAK.R"); it's best to use {package_name}.R within pack's dir.
 loadPacksAK("DM")
##############################################################################################—•°


mx3 <- 1:100
dim(mx3) <- c(10,5,2)
mx3

head(mx3)
head(mx3,3)
head(mx3,-3)
head(mx3,0)

## OK!!!

mx4 <- 1:100
dim(mx4) <- c(5,5,2,2)
mx4

head(mx4,3)
head(mx4,-3)
head(mx4,0)

##
messages(mx4) <- "Qq"
mx4

head(mx4,3)
head(mx4,-3)
## no Message...

dimnames(mx4) <- list(x=letters[1:5],y=letters[11:15],P=letters[16:17],R=letters[18:19])
mx4
head(mx4,3)

########################################################################################################################—•°
};rm(dummy)