## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {DM}
##  print.tm()
##  printtm()
##  printtm.default()
##
##
## DEPENDENCIES
##  coalesce()       {DM}  infix.R
##  whileif()        {DM}  .
##  unattr()         {DM}  attributes.R
##
## TODO
##
## ---------------------------------------------------------------------------------------------------------------------—•°


## ---------------------------------------------------------------------------------------------------------------------—•°
print.tm <- function(x,...){   class(x)<-setdiff(class(x), "tm") ; printtm(x,...) }
## ------------------------------------------------------------------------------------------------—•°
## Class "tm" is auxiliary and is not defined explicitly.
## It is given to objects with attributes "margins", "round", "transpose"
## for these attributes take effect when printing object.
## It is designed for atomic vectors of class "matrix" or any other having non-null dimensionality
## but vectors without dimension are also accepted.
## ------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
printtm = function(x,...){UseMethod("printtm")}
## ------------------------------------------------------------------------------------------------—•°
## Methods of this generic should not be defined for lists (except data.frames).
## For list-like classes define rather print.class() with the usage of printtm() if you need.
## ------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
printtm.default = function( x , transpose=NULL , margins=NULL , round=NULL , round.attr=TRUE
                          , messages=coalesce(getOption("messages"), TRUE) , comment=TRUE
                          , attributes=FALSE , ... ){
## ------------------------------------------------------------------------------------------------—•°
## Prints object together with its comment (if comment=TRUE) and messages (if messages=TRUE).
## More importantly, if dim(x) is not null then printtm() adds margins which are indicated by margins(x) attribute
## (defined in DM::tm.attributes.R) if only margins(x) is not null.
## Otherwise, if dim(x) is null then print(x) is called.
## In general this function should not be used for objects without dimensions as it introduces unnecessary
## level of complexity, while simply referring back to print().
## "tm" in "printtm" stand for "transposition" and "margin". However, notice that transposition is not default while
## margins are.
##
##    Arguments
## x           object of any type/class;
## transpose   NULL (default) or logical; if NULL then transpose attribute of x is applied;
##             if TRUE then x will be printed as t(x) but only if dim(x) is not null.
## margins     logical; if NULL (default) then margins are added but only those which are indicated by margins(x) attr.
##             If TRUE then all margins are added.
##             If FALSE then no margins are added even if margins(x) attr. is not NULL.
## round       NULL (default), logical or numeric.
##             If round.attr=TRUE and round.attr(x) is not NULL then value of 'round' is neglected.
##             If round.attr=TRUE and round.attr(x) is NULL then only numeric value of 'round' works
##             while 'round' logical or NULL does not give any result.
##             If round.attr=FALSE then numeric value of 'round' works, round=TRUE takes round.attr(x) if it is not NULL,
##             and round=FALSE or NULL gives nothing.
## round.attr  if TRUE (default) and object has non-NULL "round" attribute then its value will be used
##             regardless of 'round' value (NULL, logical or numeric);
##             if FALSE then "round" attribute of an object is not considered at all except when round=TRUE;
## messages    logical; if TRUE then messages(x) will be printed; default is coalesce(getOption("messages"), TRUE).
## comment     logical; if TRUE (default) then comment(x) will be printed;
## attributes  if FALSE (default) then no attributes will be printed;
##             if TRUE then attributes will be normaly printed;
##             however attributes which make object inheriting from class "tm"
##             i.e. "head", "round", "transpose", "margins" and also "messages" will never be printed.
## ...         passed to print()
##
## ------------------------------------------------------------------------------------------------—•°
   #if( !is.numeric(x) || !is.null(round)&&is.logical(round)&&!round ){round<-NULL}
   #round.attr <- if(!is.null(round.attr(x))&&round.attr){round.attr(x)}else{NULL}

   class(x)<-setdiff(class(x), "tm")

   transpose <- coalesce(transpose, transpose(x), FALSE)
   margins   <- is.numeric(x)*whileif(margins, iftrue=1:length(dim(x)), ifnull=margins(x), ifnull=FALSE)

   round <- if( is.numeric(x) ){

               whileif( round.attr , iffalse = whileif(round, iftrue=round.attr(x), iffalse=NULL)  ## NULL or number
                                   , iftrue  = whileif(round.attr(x), ifnull=round, iftrue=NULL, iffalse=NULL)  ## NULL or number
               )
            }else{ NULL }

   messages.x <- whileif(messages, iffalse=NULL, iftrue=messages(x))
   comment.x  <- whileif(comment, iftrue=comment(x), iffalse=NULL)

   if(!is.null(dim(x))){
      x <- switch( as.character(10*transpose+(margins[1]>0))   ## margins is attr. defined in  DM::attributes.R
                                                  ##  —— it shows which margin is sensible to be calculated, e.g. via addmargins()
               , "11" = t(addmargins(x, margins))  ## now x has lost all attributes  but dim and dimnames
               , "10" = t(x)
               ,  "1" = addmargins(x, margins)     ## now x has lost all attributes  but dim and dimnames
               ,  "0" = x
           )
   }else{
      x <- switch( as.character(1*(margins[1]>0))
               , "1" = c(x, "sum"=sum(x))
               , "0" = x
           )
   }


   what <- if( attributes ){ c("head", "round", "transpose", "margins", "messages")  ## remove only these
                           }else{ NULL }                                         ## remove all;  see how unattr() works
   x <- unattr(x, what)

   if( !is.null(round) ) print( round(x, round) , ... ) else print( x , ... )


   if( !is.null(messages.x) ){ cat("\n") ; print(messages.x, call=FALSE) }
   if( !is.null(comment.x)  ){ cat("\n") ; cat(comment.x, "\n") }
}
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
tm = function(x,...){UseMethod("tm")}
## ------------------------------------------------------------------------------------------------—•°
## Methods of this generic should not be defined for lists (except data.frames).
## For list-like classes define rather print.class() with the usage of printtm() if you need.
## ------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
tm.default = function( x , transpose=NULL , margins=NULL , round=NULL , round.attr=TRUE
                         , messages=coalesce(getOption("messages"), TRUE) , comment=TRUE
                         , attributes=FALSE , ... ){
## ------------------------------------------------------------------------------------------------—•°
   ## Prints object together with its comment (if comment=TRUE) and messages (if messages=TRUE).
   ## More importantly, if dim(x) is not null then printtm() adds margins which are indicated by margins(x) attribute
   ## (defined in DM::tm.attributes.R) if only margins(x) is not null.
   ## Otherwise, if dim(x) is null then print(x) is called.
   ## In general this function should not be used for objects without dimensions as it introduces unnecessary
   ## level of complexity, while simply referring back to print().
   ## "tm" in "printtm" stand for "transposition" and "margin". However, notice that transposition is not default while
   ## margins are.
##
##    Arguments
## x           object of any type/class;
## transpose   NULL (default) or logical; if NULL then transpose attribute of x is applied;
##             if TRUE then x will be printed as t(x) but only if dim(x) is not null.
## margins     logical; if NULL (default) then margins are added but only those which are indicated by margins(x) attr.
##             If TRUE then all margins are added.
##             If FALSE then no margins are added even if margins(x) attr. is not NULL.
## round       NULL (default), logical or numeric.
##             If round.attr=TRUE and round.attr(x) is not NULL then value of 'round' is neglected.
##             If round.attr=TRUE and round.attr(x) is NULL then only numeric value of 'round' works
##             while 'round' logical or NULL does not give any result.
##             If round.attr=FALSE then numeric value of 'round' works, round=TRUE takes round.attr(x) if it is not NULL,
##             and round=FALSE or NULL gives nothing.
## round.attr  if TRUE (default) and object has non-NULL "round" attribute then its value will be used
##             regardless of 'round' value (NULL, logical or numeric);
##             if FALSE then "round" attribute of an object is not considered at all except when round=TRUE;
## messages    logical; if TRUE then messages(x) will be printed; default is coalesce(getOption("messages"), TRUE).
## comment     logical; if TRUE (default) then comment(x) will be printed;
## attributes  if FALSE (default) then no attributes will be printed;
##             if TRUE then attributes will be normaly printed;
##             however attributes which make object inheriting from class "tm"
##             i.e. "head", "round", "transpose", "margins" and also "messages" will never be printed.
## ...         passed to print()
##
## ------------------------------------------------------------------------------------------------—•°
   #if( !is.numeric(x) || !is.null(round)&&is.logical(round)&&!round ){round<-NULL}
   #round.attr <- if(!is.null(round.attr(x))&&round.attr){round.attr(x)}else{NULL}

   class(x)<-setdiff(class(x), "tm")

   transpose <- coalesce(transpose, transpose(x), FALSE)
   margins   <- is.numeric(x)*whileif(margins, iftrue=1:length(dim(x)), ifnull=margins(x), ifnull=FALSE)

   round <- if( is.numeric(x) ){

               whileif( round.attr , iffalse = whileif(round, iftrue=round.attr(x), iffalse=NULL)  ## NULL or number
                                   , iftrue  = whileif(round.attr(x), ifnull=round, iftrue=NULL, iffalse=NULL)  ## NULL or number
               )
            }else{ NULL }

   messages.x <- whileif(messages, iffalse=NULL, iftrue=messages(x))
   comment.x  <- whileif(comment, iftrue=comment(x), iffalse=NULL)

   if(!is.null(dim(x))){
      x <- switch( as.character(10*transpose+(margins[1]>0))   ## margins is attr. defined in  DM::attributes.R
                                                  ##  —— it shows which margin is sensible to be calculated, e.g. via addmargins()
               , "11" = t(addmargins(x, margins))  ## now x has lost all attributes  but dim and dimnames
               , "10" = t(x)
               ,  "1" = addmargins(x, margins)     ## now x has lost all attributes  but dim and dimnames
               ,  "0" = x
           )
   }else{
      x <- switch( as.character(1*(margins[1]>0))
               , "1" = c(x, "sum"=sum(x))
               , "0" = x
           )
   }


   what <- if( attributes ){ c("head", "round", "transpose", "margins", "messages")  ## remove only these
                           }else{ NULL }                                         ## remove all;  see how unattr() works
   x <- unattr(x, what)

  # if( !is.null(round) ) print( round(x, round) , ... ) else print( x , ... )


   if( !is.null(messages.x) ){ messages(x) <- messages.x }
   if( !is.null(comment.x)  ){ comment(x) <- comment.x }

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
## RELOADER — before it works you need to source("RCanDo.R"); it's best to use {package_name}.R within pack's dir.
 loadPacksAK("DM")
## -------------------------------------------------------------------------------------------—•°

(mm = matrix(sample(1:12), nrow=3))
comment(mm) <- "Just an example"
margins(mm) <- 1:2
attributes(mm)

printtm(mm)
printtm(as.table(mm))
printtm(mm, comment=FALSE)
printtm(as.table(mm), comment=FALSE)

colnames(mm) <- letters[1:4]
rownames(mm) <- letters[21:23]
mm
mm[]    ## object as it is stripped off any formatting
print(mm)
print(mm, comment=FALSE)
attributes(mm)
   names(dimnames(mm)) <- c("X", "Y")
mm

message.new(mm) <- Message("someMessage", "This message cannot be forgotten.")
mm

getOption("messages")
options(messages = FALSE)
mm
print(mm, messages=T)

options(messages = T)
mm

margins(mm) <- 1
printtm(mm)
printtm(as.table(mm))

printtm(as.table(mm), TRUE)    ## transpose=T

## atomic vector
x = 1:5
margins(x) <- 1
printtm(x)
x
names(x) <- letters[1:5]
printtm(x)
x
t(x)
t(t(x))

margins(x) <- 2
x
t(x)
t(t(x))

(mm <- matrix(runif(12), nrow=3))
round.attr(mm) <-3
attributes(mm)

printtm(mm)
printtm(mm, transpose=TRUE)

printtm(mm, round.attr=FALSE)
printtm(mm, round.attr=FALSE, round=5)
printtm(mm, round.attr=FALSE, round=TRUE)   ## round.attr(x) is switched off but 'round' is only TRUE (no number)
                                          ## so rounding is performed on basis of round.attr(x)
printtm(mm, round=FALSE)    ## round.attr not switched off hence value of round is neglected
printtm(mm, round=TRUE)     ## "
printtm(mm, round=5)        ## "

## note that if round.attr is given then object inherits from "tm" thus print.tm() method can be used via print() generic
print(mm)
print(mm, round=5, round.attr=FALSE)

round.attr(mm) <- NULL
attributes(mm)
class(mm)

print(mm)
printtm(mm, round=3)
printtm(mm, round=TRUE)     ## round logical informes if to use round.attr(x) but it is NULL so no rounding is performed
printtm(mm, round=NULL)     ## " where round=NULL work like TRUE
printtm(mm, round=FALSE)

## ---------------------------------------------------------------------------------------------------------------------—•°
}; rm(dummy)
