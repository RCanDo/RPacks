########################################################################################################################—•°
## FUNCTIONS HERE    {DM}
##  whileif()
##  whileif0()
##
## DEPENDENCIES
##
## TODO
## • to move funlist to some environment in order to add possibilty of defining custom conditions except predefined
##   "iftrue", "iffalse", "ifnull", "ifna", "ifnan", "ifinf":
##   if(!exists(envir::funlist)){
##       funlist <- list({as already defined})
##   }else{
##       get(funlist,envir="envir")
##   }
##
########################################################################################################################—•°


########################################################################################################################—•°
iflogical <- function(x,...){ whileif(x,...) }     ## synonym for whileif()
########################################################################################################################—•°
iflogical0 <- function(x,...){ whileif0(x,...) }   ## synonym for whileif0()
########################################################################################################################—•°


########################################################################################################################—•°
whileif <- function(x,...){
###################################################################################################—•°
##    Arguments
##  x       atomic vector of any type.
##  ...     any number of arguments of any type but all must be named with one of
##          "iftrue", "iffalse", "ifnull", "ifna", "ifnan", "ifinf", "ifempty";
##          names may be repeated. Arguments in place of ... are called "conditions".
##          If these arguments are not named then default names are given:
##          "iftrue", "iffalse", "ifnull", "ifna" — in that order and recycled if necessary.
##          "ifnan", "ifinf", "ifempty" are rare add-ons and may be passed only as named arguments.
##          It is also possible to encapsulate all conditions into a list
##          BUT then all arguments following this list are ignored.
##          Such a list cannot be assigned to any name within a function call (see Remarks).
##
##    Remarks
## Arguments in place of ... are called "conditions".
##
## This function is generalisation of whileif0() where arguments following the first (conditions) are fixed in order.
## Read help on whileif0() to get the idea of a function.
## Notice that in  whileif0()  there are no "ifnan", "ifinf" and "ifempty" which
## are possible only in whileif() and only as named arguments.
## The cost of generalisation is that conditions must all be named and there are no defaults for them.
##
## On the other hand in whileif() it is possible to pass conditions in a list.
## If you wish to do so you must not assign this list to any name within a function call
## and it must be the first argument in place of ... (i.e. the second of the function).
## All arguments following will be ignored.
## E.g. if the list is like
##    ll <- list( iftrue=1, iffalse=0, ifnull=-1, ifna=0 )
## then
##    whileif(x,ll)
## will work while
##    whileif(x,name=ll)
## will not. E.g.
##    whileif(TRUE,ll)
## will return 1,
##    whileif(TRUE,iftrue=ll)
## will return the whole ll,
##    whileif(TRUE,name=ll)
## will throw an ERROR if "name" is none of "iftrue", "iffalse", "ifnull", "ifna", "ifnan", "ifinf", "ifempty".
##
## The above implies that if you wish to have a list as a result of some condition then all conditions
## (arguments in place of ...) must be named or the list should not be a result of the first condition.
##
## If "ifnan" is passed then "ifna" do not recognise NaN values as NA what is different from standard is.na() where
## is.na(NaN) returns TRUE.
## It allows distinguish between NaN and other NAs without taking care for an order of conditions.
## The standard behaviour of is.na() is preserved when "ifnan" is not passed.
##
## The following table gives reference on behaviour of standard is.{na/nan/infinite}() functions
## (dots means trivial TRUEs):
##
##   function        input
##                  NA    NaN   Inf      0/0   1/0-1/0     1/0   log(0)
##   is.na           .     T     F        T      T          F     F
##   is.nan          F     .     F        T      T          F     F
##   is.infinite     F     F     .        F      F          T     T
##
###################################################################################################—•°
ll <- list(...)   ## ll <- list(iftrue=1,iffalse=0,ifnull=NULL,ifna=NA)  ## e.g.

if(length(ll)==0){
   x
}else{

   nams <- names(ll)

   ## if the first element of ... is unnamed and is a list we take only this element as a list of conditions
   ## and ignore the following elements of ...
   if( ( is.null(nams) || nams[1]=="" ) && is.list(ll[[1]]) ){
      ll <- ll[[1]]
      nams <- names(ll)
   }

   if(is.null(nams)){
      nams <- names(ll) <- rep(c("iftrue","iffalse","ifnull","ifna"),length=length(ll))
      warning("Arguments following the first are not named so default names were given:
\"iftrue\", \"iffalse\", \"ifnull\", \"ifna\" — in that order and recycled if necessary.")
   }
   if(any(!nams%in%c("iftrue","iffalse","ifnull","ifna","ifnan","ifinf","ifempty"))){
      stop( "Arguments passed to whileif() must be all (but first) named with one of
\"iftrue\", \"iffalse\", \"ifnull\", \"ifna\", \"ifnan\", \"ifinf\", \"ifempty\".
The first argument should be unnamed or have name \"x\"." )
   }

   notnanin <- ! "ifnan" %in% nams

   funlist <- list(
        iftrue    = function(a,b) if( !is.null(a) && length(a)>0 && is.logical(a[[1]]) && !is.na(a[[1]]) && a[[1]] ) b else a
      , iffalse   = function(a,b) if( !is.null(a) && length(a)>0 && is.logical(a[[1]]) && !is.na(a[[1]]) && !a[[1]] ) b else a
      , ifnull    = function(a,b) if( is.null(a) ) b else a
      , ifna      = function(a,b) if( !is.null(a) && length(a)>0 && (!is.nan(a[[1]])||notnanin) && is.na(a[[1]]) ) b else a
                                                                    ## always TRUE if "ifnan" is not passed
      ## additional, only by hand
      , ifnan     = function(a,b) if( !is.null(a) && length(a)>0 && is.nan(a[[1]]) ) b else a
      , ifinf     = function(a,b) if( !is.null(a) && length(a)>0 && is.infinite(a[[1]]) ) b else a
      , ifempty   = function(a,b) if( !is.null(a) && length(a)==0 ) b else a
   )

   funcall <- function(k,...){
      funlist[[nams[k]]](...,ll[[k]])
   }

   Reduce(funcall, rev(as.list(1:length(ll))) , x , right=TRUE )     ## right=FALSE  doesn't work so rev() is used

}
}  ##----END----##
########################################################################################################################—•°

########################################################################################################################—•°
whileif0 <- function(x,iftrue=TRUE,iffalse=FALSE,ifnull=NULL,ifna=NA){
###################################################################################################—•°
##    Arguments
##  x
##  iftrue        result when x is TRUE.
##  iffalse       result when x is FALSE.
##  ifnull        result when x is NULL.
##  ifna          result when x is NA.
##
##    Result
## If the first element of 'x' (vector — atomic or list) i.e. x[[1]] is not logical and not NULL and not NA then 'x'.
## If x[[1]] is TRUE then 'iftrue', if x[[1]] is FALSE then 'iffalse',
## if x[[1]] is NULL then 'ifnull', if x[[1]] is NA then 'ifna'.
## Result of earlier condition checking is piped to the next. See comment below.
##
##    Remarks
## Notice that 'iffalse' is applied before 'ifnull' which in turn is applied before 'ifna' so that if you run e.g.
## > whileif0(x,iftrue,iffalse=NULL,ifnull=0)
## you'll get 0 for FALSE, not NULL;
## or if you run
## > whileif0(x,iftrue,iffalse=NULL,ifnull=FALSE)
## you'll get FALSE for FALSE, not NULL.
## Thus setting iffalse=NULL is equivalent to "treat FALSE as NULL and apply 'ifnull'".
## Similarily
## > whileif0(TRUE,FALSE,iffalse=NULL,ifnull=NA,ifna=0)
## returns 0 as TRUE gives FALSE but FALSE gives NULL but NULL gives NA but NA is turned into 0.
## This implies that you cannot simply switch x from NULL to FALSE and FALSE to NULL
## — you need e.g. the following trick:
## > whileif0(x,iffalse=NA,ifnull=FALSE,ifna=NULL)
##
## whileif0() is a special case of whileif() (as paste0() is as special case of paste())
## where you may pass any number of arguments (but only from set  "iftrue", "iffalse", "ifnull", "ifna" plus additional
## "ifnan", "ifinf") in any order.
## This however, requires writing their names, and they obviously have no defaults.
##
###################################################################################################—•°

#if(!is.atomic(x)){
#   stop("x is not atomic but whileif0() works only on atomic vectors.")
#}

#        iftrue    = function(a,b) if(!is.null(x) && is.logical(x[[1]]) && !is.na(x[[1]]) && x[[1]]) iftrue else x
#      , iffalse   = function(a,b) if(!is.null(x) && is.logical(x[[1]]) && !is.na(x[[1]]) && !x[[1]]) iffalse else x
#      , ifnull    = function(a,b) if(is.null(x)) ifnull else x
#      , ifna      = function(a,b) if(!is.null(x) && is.na(x[[1]])) ifna else x

x <- if(!is.null(x) && is.logical(x[[1]]) && !is.na(x[[1]]) && x[[1]])  iftrue  else x
x <- if(!is.null(x) && is.logical(x[[1]]) && !is.na(x[[1]]) && !x[[1]]) iffalse else x
x <- if(is.null(x)) ifnull else x
x <- if(!is.null(x) && is.na(x[[1]])) ifna else x
x

}  ##----END----##
########################################################################################################################—•°

########################################################################################################################—•°
## EXAMPLES ############################################################################################################—•°
########################################################################################################################—•°

########################################################################################################################—•°
dummy = function(){
## This is dummy function - it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## — in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
##############################################################################################—•°

 ## RELOADER —— before it works you need to source("PacksAK.R"); it's best to use EfficiencyCurves.R within pack's dir.
 loadPacksAK("DM")

##############################################################################################—•°
## whileif()

## trivial case
whileif(1)
whileif(FALSE)
whileif(NULL)
whileif(NA)

whileif(1,ifnull=FALSE)
whileif(NULL,ifnull=FALSE)
whileif(NULL,ifnull=FALSE,ifna=0)
whileif(NA,ifnull=FALSE,ifna=0)

## in case when x is vvector (atomic or list) its first element is considered while checking conditions ...
whileif(c(NA,1),ifnull=FALSE,ifna=0)
whileif(c(NA,1),ifnull=FALSE,ifna=0:2)
whileif(1:2,ifnull=FALSE,ifna=0)      ## ... but the whole x is returned when no condition is satisfied for the first element
   #
whileif(c(T,F),iftrue="ok",iffalse=0)
whileif(c(F,T),iftrue="ok",iffalse=0)
whileif(c(NA,F,T),iftrue="ok",iffalse=0) ## the whole list is returned when no condition is satisfied for the first element

## similar for list()
whileif(list(NA,1),ifnull=FALSE,ifna=0)
whileif(list(NA,1),ifnull=FALSE,ifna=0:2)
whileif(list(1,2),ifnull=FALSE,ifna=0)   ## the whole list is returned when no condition is satisfied for the first element
   #
whileif(list(T,F),iftrue="ok",iffalse=0)
whileif(list(F,T),iftrue="ok",iffalse=0)
whileif(list(NA,F,T),iftrue="ok",iffalse=0) ## the whole list is returned when no condition is satisfied for the first element

whileif(1:3,iftrue=1,iffalse=0,ifnull=NULL,ifna=NA)
whileif(T,iftrue=1,iffalse=0,ifnull=NULL,ifna=NA)
whileif(F,iftrue=1,iffalse=0,ifnull=NULL,ifna=NA)
whileif(NULL,iftrue=1,iffalse=0,ifnull=NULL,ifna=NA)
whileif(NULL,iftrue=1,iffalse=0,ifnull=-1,ifna=NA)
whileif(NA,iftrue=1,iffalse=0,ifnull=NULL,ifna=NA)
whileif(NA,iftrue=1,iffalse=0,ifnull=NULL,ifna=0)

## simpler
whileif(T,iftrue=1,iffalse=0)
whileif(F,iftrue=1,iffalse=0)
whileif(NULL,iftrue=1,iffalse=0)
whileif(NA,iftrue=1,iffalse=0)
whileif(3,iftrue=1,iffalse=0)
whileif("3",iftrue=1,iffalse=0)
whileif(list(),iftrue=1,iffalse=0)

## more simple using default order of arguments
whileif(T,1,0)    ## warning appears
suppressWarnings(whileif(T,1,0))
whileif(F,1,0)
whileif(NULL,1,0)
whileif(NA,1,0)
whileif(3,1,0)
whileif("3",1,0)
whileif(list(),1,0)

## conditions may be passed through a list
 l1 <- list(iftrue=1,iffalse=0,ifnull=-1,ifna=0)
whileif(1:3,l1)
whileif(T,l1)
whileif(F,l1)
whileif(NULL,l1)
whileif(NA,l1)
## however, if the list of conditions is passed via name
whileif(T,name=l1)  ## ERROR
## an error occurs;
## or if it is a legal condition name, then the list passed via this name is treated as an output of a condition
## i.e. whole list is retuned if the condition is satisfied
whileif(T,iftrue=l1)
whileif(F,iftrue=l1,iffalse="f")

## This may be used as a tool for establishing varying set of conditions
 l2 <- list(iftrue="t",iffalse="f",ifnull="null",ifna="na")
 (y = sample(0:1,10,replace=T))
whileif( sum(y)<5, whileif(is.numeric(y), iftrue=l1, iffalse=l2) )
 (y = sample(c(T,F),10,replace=T))
whileif( sum(y)<5, whileif(is.numeric(y), iftrue=l1, iffalse=l2) )

## an order is important as a result of earlier condition checking is passed to the next
whileif(F,iffalse=NULL,ifnull=NA,ifna="a")
whileif(F,iffalse=NULL,ifna="a",ifnull=NA)

whileif(F,iffalse=NULL,iftrue=0,ifna="a",ifnull=NA)
whileif(NA,iffalse=TRUE,iftrue=0,ifna="a",ifnull=NA)
whileif(NULL,iffalse=TRUE,iftrue=0,ifna="a",ifnull=NA)

whileif(F,iffalse=TRUE,iftrue=FALSE,iffalse=TRUE,iftrue=NA)
whileif(F,iffalse=TRUE,iftrue=FALSE,iffalse=NA,ifna=0)

## tree-like reasoning
whileif( TRUE , iftrue  = whileif( 1 , ifnull=NULL )       ## should not return FALSE
                , iffalse = whileif( NULL , ifnull="n") )

whileif( FALSE, iftrue  = whileif( 1 , ifnull=NULL )       ## should not return FALSE
                , iffalse = whileif( NULL , ifnull="n") )

## other things
whileif( rnorm(1)<0 , iffalse = 1 , iftrue = NA )

## only first element of logical vector is considered
x <- rnorm(10)
whileif( x<0 , iffalse = x[x>0] , iftrue = x[x<0] )   ## all positive el. of x if the first is positive
                                                      ## or all negative if the first is negative

###########################################################—•°
##  ifna() gives result for NaN if there is no ifnan() passed
whileif( 0/0 , ifna="na" )
##  but if ifnan() is passed then ifna() ignores NaN
whileif( 0/0 , ifna="na" , ifnan="nan" )
###########################################################—•°

whileif( log(0) , ifna="ok" )    ## log(0) is infinity not NA
whileif( log(0) , ifna="ok" , ifinf="infinity" )


###################################################################################################—•°
## whileif0()

whileif0(list(1),1,0)

whileif0(1)         ## x returned if it is not logical
whileif0(1:3)
whileif0(letters[1:3])

whileif0(NA)        ## default for NA is NA
whileif0(NA,ifna="na")

whileif0(T)         ## default  iftrue = TRUE
whileif0(T,1)
whileif0(T,1:3)
whileif0(T,FALSE)
whileif0(T,NULL)
whileif0(T,NA)

whileif0(F,1:3)     ## default  ifalse = FALSE
whileif0(F,1:3,0)
whileif0(F,,"f")
whileif0(F,,NULL)
whileif0(F,,NA)

whileif0(1,0)
whileif0(TRUE,0)
whileif0(FALSE,0)
whileif0(NULL,0)
whileif0(NA,0)


whileif0(FALSE,0,"f")
whileif0(NULL,0,,"n")
whileif0(NA,0,,,"na")

whileif0(NA_real_,0,,,"na")
whileif0(NA_integer_,0,,,"na")
whileif0(NA_character_,0,,,"na")
whileif0(NA_complex_,0,,,"na")

whileif0(FALSE,TRUE,FALSE)
whileif0(TRUE,TRUE,FALSE)

whileif0(FALSE,1,NULL,0)
whileif0(FALSE,1,NULL,FALSE)

whileif0(NULL,1,NULL,NA,0)

whileif0(TRUE,FALSE,NULL,NA,0)   ## 0

whileif0(TRUE,NULL,NULL,NA,0)

whileif0(z<-TRUE,NULL,NULL,NA,z)
whileif0(z<-FALSE,NULL,NULL,NA,z)

whileif0(z<-FALSE,z,NULL,NA,z)

##...

}
rm(dummy)
########################################################################################################################—•°