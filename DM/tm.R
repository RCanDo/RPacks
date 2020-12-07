## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {DM}
##  tm()
##  tm.default()
##
## DEPENDENCIES
##  coalesce()       {DM}  infix.R
##  whileif()        {DM}  .
##  unattr()         {DM}  attributes.R
##
## TODO
##
## COMMENT
##
## -----------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
tm = function(x,...){UseMethod("tm")}
## ------------------------------------------------------------------------------------------------—•°
## Methods of this generic should not be defined for lists (except data.frames).
## For list-like classes define rather print.class() with the usage of printtm() if you need.
## ------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
tm.default = function( x , transpose=NULL , margins=NULL , round=NULL , round.attr=TRUE , head=NULL
                         , messages=coalesce(getOption("messages"), TRUE) , comment=TRUE
                         , attributes=FALSE , ... ){
## ------------------------------------------------------------------------------------------------—•°
   ## Prints object together with its comment (if comment=TRUE) and messages (if messages=TRUE).
   ## More importantly, if dim(x) is not null then printtm() adds margins which are indicated by margins(x) attribute
   ## (defined in DM::attributes.R) if only margins(x) is not null.
   ## Otherwise, if dim(x) is null then print(x) is called.
   ## In general this function should not be used for objects without dimensions as it introduces unnecessary
   ## level of complexity, while simply referring back to print().
   ## "tm" in "printtm" stands for "transposition" and "margin". However, notice that transposition is not default while
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
##             if FALSE then "round" attribute of an object is not considered at all except when round=TRUE.
## head        NULL (default), logical or numeric; how many rows to print (or elements, if x is not a matrix or array).
## messages    logical; if TRUE then messages(x) will be printed; default is coalesce(getOption("messages"), TRUE).
## comment     logical; if TRUE (default) then comment(x) will be printed;
## attributes  if FALSE (default) then no attributes will be printed;
##             if TRUE then attributes will be normaly printed;
##             however attributes which make object inheriting from class "tm"
##             i.e. "head", "round", "transpose", "margins" and also "messages" will never be printed.
## ...         passed to print()
##
## ------------------------------------------------------------------------------------------------—•°

   messages.x <- whileif(messages, iftrue=messages(x), iffalse=NULL)
   comment.x  <- whileif(comment, iftrue=comment(x), iffalse=NULL)

## ----------------------------------------------------------------------------—•°
## tm_core — could be defined separately, but it is used only twice: in tm() and print.tm()
## and separation creates some problems...

   transpose <- coalesce(transpose, transpose(x), FALSE)
   margins   <- is.numeric(x)*whileif(margins, iftrue=1:length(dim(x)), ifnull=margins(x), ifnull=FALSE)
   head      <- whileif(head, ifnull=FALSE, iftrue=head.attr(x), ifnull=6, iffalse=NULL)

   round <- if( is.numeric(x) ){

               whileif( round.attr , iffalse = whileif(round, iftrue=round.attr(x), iffalse=NULL)  ## NULL or number
                                   , iftrue  = whileif(round.attr(x), ifnull=round, iftrue=NULL, iffalse=NULL)  ## NULL or number
               )
            }else{ NULL }

   class(x)<-setdiff(class(x), "tm")

   if(!is.null(head)){
      x <- head(x, head)
   }


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
                           }else{ NULL }                                         ## remove all but basic attributes i.e. getOption("class.attributes")$basic
                                                                                 ## see how unattr() works
   x <- unattr(x, what)

   x <- if( !is.null(round) ) round(x, round) else x

## ----------------------------------------------------------------------------—•°

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
## ------------------------------------------------------------------------------------------------—•°
## RELOADER — before it works you need to source("PacksAK.R"); it's best to use DM.R within pack's dir.
 loadPacksAK("DM")
## ------------------------------------------------------------------------------------------------—•°



## ---------------------------------------------------------------------------------------------------------------------—•°
}; rm(dummy)
