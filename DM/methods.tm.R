## ---------------------------------------------------------------------------------------------------------------------���
## Methods for the class "tm".
## ---------------------------------------------------------------------------------------------------------------------���
## FUNCTIONS HERE    {DM}
##  head.tm()
##
## DEPENDENCIES
##  whileif()        whileif.R
##  head.attr()      tm.attributes.R
##
## TODO
##
## -------------------------------------------------------------------------------------------------���
## AUTHOR: Arkadiusz Kasprzyk; akasp@...
## ver. 1.0
## start: 2017-0m-dd    last: 2017-08-06
## ---------------------------------------------------------------------------------------------------------------------���

## ---------------------------------------------------------------------------------------------------------------------���
head.tm <- function( x , head = NULL , ... ){
## ------------------------------------------------------------------------------------------------���
##
## ------------------------------------------------------------------------------------------------���

head = whileif(head, iftrue=NULL, ifnull=head.attr(x), ifnull=6, iftrue=6, iffalse=NULL)
#margins = whileif(margins(x), ifnull=0)

if(!is.null(head)){

   attrs <- attributes(x)
   x <- unattr(x)
   if(is.null(dim(x))){
      attrs$names <- NULL
   }else{
      attrs$dim <- NULL
      attrs$dimnames <- NULL
   }

   x <- head(x, n=head)

   attributes(x)[names(attrs)] <- attrs
}

x

}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------���

## ---------------------------------------------------------------------------------------------------------------------���
## EXAMPLES ############################################################################################################���
## ---------------------------------------------------------------------------------------------------------------------���

## ---------------------------------------------------------------------------------------------------------------------���
dummy = function(){
## This is dummy function � it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## � in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
## ------------------------------------------------------------------------------------------------���
## RELOADER � before it works you need to source("PacksAK.R"); it's best to use DM.R within pack's dir.
 loadPacksAK("DM")
## ------------------------------------------------------------------------------------------------���


hx <- sample(100)
head.attr(hx) <- 10
hx
class(hx)
head(hx)
head(hx, 6)
head(hx,-2)

hm <- matrix(sample(36), 9)
dimnames(hm) <- list(X=letters[1:9], Y=letters[21:24])
hm
head(hm)

margins(hm) <- 1:2
hm
head(hm)
head(hm, 4)
head(hm, 10)

transpose(hm) <- T
hm
head(hm)
head(hm, 2)

t(hm)
hm[]

head.attr(hm) <- 4
hm
head(hm)

## -------------------------------------

hm <- matrix(sample(36), 9)
dimnames(hm) <- list(X=letters[1:9], Y=letters[21:24])
hm
head(hm)

head.attr(hm) <- 3
hm
head(hm)
head(hm, 5)

margins(hm) <- 1:2
hm
head(hm)
head(hm, 4)
head(hm, 10)


head.attr(hm) <- T
hm
head(hm)

transpose(hm) <- T
hm
head(hm)
head(hm, 2)

t(hm)
hm[]

hmt <- t(hm)
hmt[]
hmt
head(hmt)
head(hmt, 2)

transpose(hmt) <- F
hmt[]
hmt
head(hmt)
head(hmt, 2)

## ---------------------------------------------------------------------------------------------------------------------���
}; rm(dummy)

