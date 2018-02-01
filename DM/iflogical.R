########################################################################################################################—•°
## FUNCTIONS HERE    {DM}
##  iflogical()
##  iflogical0()
##
## DEPENDENCIES
##
## TODO
## •
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
##          "iftrue", "iffalse", "ifnull", "ifna", "ifnan", "ifinf";
##          names may be repeated.
##          If these arguments are not named then default names are given:
##          "iftrue", "iffalse", "ifnull", "ifna" — in that order and recycled if necessary.
##          "ifnan", "ifinf" are rare add-ons and may be passed only as named arguments.
##          It is also possible to encapsulate all these arguments into a list BUT then all arguments follwing this list
##          are ignored.
##
##    Comment
## This function is generalisation of whileif0() where arguments following the first are fixed.
## Read help on whileif0() to get the idea. Notice that in  whileif0()  there are no "ifnan" and "ifinf" which
## are possible only in whileif() and only as named arguments.
## The cost of generalisation is that arguments following the first must all be named and there's no defaults for them.
##
## If "ifnan" is passed then "ifna" do not recognise NaN values as NA what is different from standard is.na() where
## is.na(NaN) returns TRUE. This standard behaviour is preserved when "ifnan" is not passed.
##
###################################################################################################—•°
ll <- list(...)   ## ll <- list(iftrue=1,iffalse=0,ifnull=NULL,ifna=NA)  ## e.g.

if(length(ll)==0){
   x
}else{

   if(is.list(ll[[1]])){ ll <- ll[[1]] }

   nams <- names(ll)
   if(is.null(nams)){
      names(ll) <- rep(c("iftrue","iffalse","ifnull","ifna"),length=length(ll))
      warning("Arguments following the first are not named so default names were given:
   \"iftrue\", \"iffalse\", \"ifnull\", \"ifna\" — in that order and recycled if necessary.")
   }
   if(any(!nams%in%c("iftrue","iffalse","ifnull","ifna","ifnan","ifinf"))){
      stop( "Arguments passed to whileif() must be all (but first) named with one of
\"iftrue\", \"iffalse\", \"ifnull\", \"ifna\", \"ifnan\", \"ifinf\".
The first argument should be unnamed or have name \"x\"." )
   }

   notnanin <- ! "ifnan" %in% nams

   funlist <- list(
        iftrue    = function(a,b) if(!is.null(a) && is.logical(a) && !is.na(a[1]) && a[1]) b else a
      , iffalse   = function(a,b) if(!is.null(a) && is.logical(a) && !is.na(a[1]) && !a[1]) b else a
      , ifnull    = function(a,b) if(is.null(a)) b else a
      , ifna      = function(a,b) if(!is.null(a) && (!is.nan(a[1])||notnanin) ## always TRUE if "ifnan" is not passed
                                                 && is.na(a[1])) b else a
      ## additional, only by hand
      , ifnan     = function(a,b) if(!is.null(a) && is.nan(a[1])) b else a
      , ifinf     = function(a,b) if(!is.null(a) && is.infinite(a[1])) b else a
   )

##                  NA    NaN   Inf      0/0   1/0-1/0     1/0   log(0)
##   is.na           .     T     F        T      T          F     F
##   is.nan          F     .     F        T      T          F     F
##   is.infinite     F     F     .        F      F          T     T


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
##  iftrue        result when x=TRUE.
##  iffalse       result when x=FALSE.
##  ifnull        result when x is NULL.
##  ifna          result when x is NA.
##
##    Result
## If 'x' is not logical and not NULL and not NA then 'x'.
## If 'x' is TRUE then 'iftrue', if 'x' is FALSE then 'iffalse',
## if 'x' is NULL then 'ifnull', if 'x' is NA then 'ifna'.
## Result of earlier condition checking is piped to the next. See comment below.
##
##    Comments
## Notice that 'iffalse' is applied before 'ifnull' which in turn is applied before 'ifna' so that if you run e.g.
## > whileif0(x,iftrue,iffalse=NULL,ifnull=0)
## you'll get 0 for FALSE, not NULL;
## or if you run
## > whileif0(x,iftrue,iffalse=NULL,ifnull=FALSE)
## you'll get FALSE for FALSE, not NULL.
## Thus setting iffalse=NULL is equivalent to "treat FALSE as NULL and apply 'ifnull'".
## This also implies that you cannot switch x from NULL to FALSE and FALSE to NULL at the same time using this function.
## The same rules concern 'ifna'.
## E.g.
## > whileif0(TRUE,FALSE,iffalse=NULL,ifnull=NA,ifna=0)
## returns 0 as TRUE gives FALSE but FALSE gives NULL but NULL gives NA but NA is turned into 0.
##
## whileif0() is a special case of whileif() (as paste0() is as special case of paste())
## where you may pass any number of arguments (but only from set  "iftrue", "iffalse", "ifnull", "ifna" plus additional
## "ifnan", "ifinf") in any order.
## This however, requires writing their names, and they obviously have no defaults.
##
###################################################################################################—•°
if(!is.atomic(x)){
   stop("x is not atomic but whileif0() works only on atomic vectors.")
}

y <- if(is.logical(x)){
         if(length(x)>1) warning("x is logical but length(x)>1 and only first element of x is considered.")
         if(is.na(x[1])) x else if(x[1]) iftrue else iffalse
     }else{
        x
     }
y <- if(!is.null(y) && is.logical(y) && !is.na(y[1]) && !y[1]) iffalse else y  ## in case iftrue = FALSE;    is.logical(NULL) -> FALSE  that's OK!
y <- if(is.null(y)) ifnull else y
y <- if(!is.null(y) && is.na(y[1])) ifna else y
y

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

##################################################################################3
## whileif()

whileif(1)
whileif(FALSE)
whileif(NULL)
whileif(NA)

whileif(1,ifnull=FALSE)
whileif(NULL,ifnull=FALSE)
whileif(NULL,ifnull=FALSE,ifna=0)
whileif(NA,ifnull=FALSE,ifna=0)

whileif(1:3,iftrue=1,iffalse=0,ifnull=NULL,ifna=NA)
whileif(T,iftrue=1,iffalse=0,ifnull=NULL,ifna=NA)
whileif(F,iftrue=1,iffalse=0,ifnull=NULL,ifna=NA)
whileif(NULL,iftrue=1,iffalse=0,ifnull=NULL,ifna=NA)
whileif(NULL,iftrue=1,iffalse=0,ifnull=-1,ifna=NA)
whileif(NA,iftrue=1,iffalse=0,ifnull=NULL,ifna=NA)
whileif(NA,iftrue=1,iffalse=0,ifnull=NULL,ifna=0)

## arguments may be passed through a list
 ll <- list(iftrue=1,iffalse=0,ifnull=-1,ifna=0)
whileif(1:3,ll)
whileif(T,ll)
whileif(F,ll)
whileif(NULL,ll)
whileif(NA,ll)

## an order is important as result of earlier condition checking is passed to next
whileif(F,iftrue=0,iffalse=NULL,ifnull="n",ifna="a")
whileif(F,iftrue=0,iffalse=NULL,ifnull=NA,ifna="a")

whileif(F,iffalse=NULL,iftrue=0,ifna="a",ifnull=NA)
whileif(NA,iffalse=TRUE,iftrue=0,ifna="a",ifnull=NA)
whileif(NULL,iffalse=TRUE,iftrue=0,ifna="a",ifnull=NA)

whileif(F,iffalse=TRUE,iftrue=FALSE,iffalse=TRUE,iftrue=NA)
whileif(F,iffalse=TRUE,iftrue=FALSE,iffalse=NA,ifna=0)

## tree-like reasoning

whileif( TRUE , iftrue  = whileif( 1 , ifnull=NULL)       ## should not return FALSE
                , iffalse = whileif( NULL , ifnull="n") )

whileif( FALSE, iftrue  = whileif( 1 , ifnull=NULL)       ## should not return FALSE
                , iffalse = whileif( NULL , ifnull="n") )

## other things
whileif( rnorm(1)<0 , iffalse = 1 , iftrue = NA )

## only first element of logical vector is considered
x <- rnorm(10)
whileif( x<0 , iffalse = x[x>0] , iftrue = x[x<0] )

###########################################################
##  ifna() gives result for NaN if there is no ifnan() passed
whileif( 0/0 , ifna="na" )
##  but if ifnan() is passed then ifna() ignores NaN
whileif( 0/0 , ifna="na" , ifnan="nan" )
###########################################################

whileif( log(0) , ifna="ok" )
whileif( log(0) , ifna="ok" , ifinf="infinity" )


###############################################################################
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