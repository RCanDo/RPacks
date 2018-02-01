########################################################################################################################—•°
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
##  tm()             {DM}  .
##
## TODO
##
########################################################################################################################—•°


########################################################################################################################—•°
print.tm <- function(x,...){   class(x)<-setdiff(class(x),"tm") ; printtm(x,...) }
###################################################################################################—•°
## Class "tm" is auxiliary and is not defined explicitly.
## It is given to objects with attributes "margins", "round", "transpose", "head"
## for these attributes take effect when printing object.
## It is designed for atomic vectors of class "matrix" or any other having non-null dimensionality
## but vectors without dimension are also accepted.
###################################################################################################—•°

########################################################################################################################—•°
printtm = function(x,...){UseMethod("printtm")}
###################################################################################################—•°
## Methods of this generic should not be defined for lists (except data.frames).
## For list-like classes define rather print.class() with the usage of printtm() if you need.
###################################################################################################—•°

########################################################################################################################—•°
printtm.default = function( x , transpose=NULL , margins=NULL , round=NULL , round.attr=TRUE , head=NULL
                          , messages=coalesce(getOption("messages"),TRUE) , comment=FALSE
                          , attributes=FALSE , ... ){
###################################################################################################—•°
## Prints object together with its comment (if comment=TRUE) and messages (if messages=TRUE).
## More importantly, if dim(x) is not NULL then printtm() adds margins which are indicated by margins(x) attribute
## (defined in tm.attributes.R) if only margins(x) is not null.
## Otherwise, if dim(x) is NULL then print(x) is called.
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
##             If round.attr=FALSE then numeric value of 'round' works,
##             round=TRUE takes round.attr(x) if it is not NULL, and round=FALSE or NULL gives nothing.
## round.attr  if TRUE (default) and object has non-NULL "round" attribute then its value will be used
##             regardless of 'round' value (NULL, logical or numeric);
##             if FALSE then "round" attribute of an object is not considered at all except when round=TRUE;
## head        NULL (default), logical or numeric; how many rows to print (or elements,
##             if x is neither matrix nor array).
## messages    logical; if TRUE then messages(x) will be printed; default is coalesce(getOption("messages"),TRUE).
## comment     logical; if TRUE (default) then comment(x) will be printed;
## attributes  if FALSE (default) then no attributes will be printed;
##             if TRUE then attributes will be normaly printed;
##             however attributes which make object inheriting from class "tm"
##             i.e. "head", "round", "transpose", "margins" and also "messages" will never be printed.
## ...         passed to print()
##
###################################################################################################—•°

   messages.x <- whileif(messages,iftrue=messages(x),iffalse=NULL)
   comment.x  <- whileif(comment,iftrue=comment(x),iffalse=NULL)

###############################################################################—•°
#   x <- tm_core( x , transpose=transpose , margins=margins , round=round , round.attr=round.attr
#                          , messages=messages , comment=comment
#                          , attributes=attributes , ... )
## actually not defined

###############################################################################—•°
## tm_core — could be defined separately, but it is used only twice


   transpose <- coalesce(transpose,transpose(x),FALSE)
   margins   <- is.numeric(x)*whileif(margins,iftrue=1:length(dim(x)),ifnull=margins(x),ifnull=FALSE)
   head      <- whileif(head,ifnull=FALSE,iftrue=head.attr(x),ifnull=6,iffalse=NULL)

   round <- if( is.numeric(x) ){

               whileif( round.attr , iffalse = whileif(round,iftrue=round.attr(x),iffalse=NULL)  ## NULL or number
                                   , iftrue  = whileif(round.attr(x),ifnull=round,iftrue=NULL,iffalse=NULL)  ## NULL or number
               )
            }else{ NULL }

   class(x)<-setdiff(class(x),"tm")

   if(!is.null(head)){
      x <- head(x,head)
   }

   if(!is.null(dim(x))){

      x <- switch( as.character(10*transpose+(margins[1]>0))   ## margins is attr. defined in  DM::tm.attributes.R
                                                  ##  — it shows which margin is sensible to be calculated, e.g. via addmargins()
               , "11" = t(addmargins(x,margins))  ## now x has lost all attributes  but dim and dimnames
               , "10" = t(x)
               ,  "1" = addmargins(x,margins)     ## now x has lost all attributes  but dim and dimnames
               ,  "0" = x
           )
   }else{
      x <- switch( as.character(1*(margins[1]>0))
               , "1" = c(x,"sum"=sum(x))
               , "0" = x
           )
   }

   what <- if( attributes ){ c("head","round","transpose","margins","messages")  ## remove only these
                           }else{ NULL }                                         ## remove all but basic attributes i.e. getOption("class.attributes")$basic
                                                                                 ## see how unattr() works
   x <- unattr(x,what)

   x <- if( !is.null(round) ) round(x,round) else x

###############################################################################—•°

   print(x)

   if( !is.null(messages.x) ){ cat("\n") ; print(messages.x,call=FALSE) }
   if( !is.null(comment.x)  ){ cat("\n") ; cat(comment.x,"\n") }
}
########################################################################################################################—•°

########################################################################################################################—•°
## EXAMPLES
dummy=function(){

    ## RELOADER —— before it works you need to source("PacksAK.R"); it's best to use EfficiencyCurves.R within pack's dir.
    loadPacksAK("DM")

(mm = matrix(sample(1:12),nrow=3))
comment(mm) <- "Just an example"


#######################################—•°
## margins attribute
margins(mm) <- 1:2
attributes(mm)          ## notice "tm" class;
##! REMARK: As soon as you give one of attributes "margins", "transpose", "round" or "head"
## then object is endowed with "tm" class.

printtm(mm)
print(mm)               ## mm inherits from "tm" thus print.tm() method is used which in turn calls printtm()
mm                      ## the same - the simplest call - proper printing method is used
printtm(mm,comment=TRUE)
print(mm,comment=TRUE)

tm(mm)                  ## returns object with all "tm" attributes applied (not only displayed); see more on tm.R

##
printtm(as.table(mm))   ## as.table(mm) is not of "tm" class ...
print(as.table(mm))     ## ... hence print() does not call print.tm() method but rather print.table()
printtm(as.table(mm),comment=TRUE)
print(as.table(mm),comment=TRUE)    ## no comments BUT as.table(mm) has no comments at all (look 2 lines up)

colnames(mm) <- letters[1:4]
rownames(mm) <- letters[21:23]
mm
mm[]    ## object as it is, stripped off any formatting
print(mm)
print(mm,comment=TRUE)
attributes(mm)
   names(dimnames(mm)) <- c("X","Y")
mm

message.new(mm) <- Message("someMessage","This message cannot be forgotten.")
mm
print(mm)
printtm(mm)
print(mm,comment=T)

getOption("messages")
options(messages = FALSE)
mm
print(mm,messages=T)

options(messages = T)
mm

margins(mm) <- 1
print(mm)
print(as.table(mm))
## BUT
attributes(as.table(mm))
printtm(as.table(mm))

printtm(as.table(mm),TRUE)    ## transpose=T

#######################################—•°
## atomic vector
x = 1:5
margins(x) <- 1
printtm(x)
x
names(x) <- letters[1:5]
printtm(x)
x
t(x)
## BUT
attributes(t(x)) ## hence
t(t(x))

margins(x) <- 2
x
t(x)
t(t(x))

#######################################—•°
## transpose attribute
mm
transpose(mm)
transpose(mm) <- T
mm

x
transpose(x)
transpose(x) <- T
x         ## no effect,
## BUT

t(x)  ## whereas
attributes(t(x))
margins(x)
## what may seem strange BUT
## if you remove transpose
transpose(x) <- F
t(x)


#######################################—•°
## round.attr
(mm <- matrix(runif(12),nrow=3))
round.attr(mm) <-3
attributes(mm)

printtm(mm)
printtm(mm,transpose=TRUE)

printtm(mm,round.attr=FALSE)
printtm(mm,round.attr=FALSE,round=5)
printtm(mm,round.attr=FALSE,round=TRUE)   ## round.attr(x) is switched off but 'round' is only TRUE (no number)
                                          ## so rounding is performed on basis of round.attr(x)
printtm(mm,round=FALSE)    ## round.attr not switched off hence value of round is neglected
printtm(mm,round=TRUE)     ## "
printtm(mm,round=5)        ## "

## note that if round.attr is given then object inherits from "tm" thus print.tm() method can be used via print() generic
print(mm)
mm
print(mm,round=5,round.attr=FALSE)

round.attr(mm) <- NULL
attributes(mm)
class(mm)   ## no more of "tm" class
mm          ## just ordinary matrix

print(mm)
printtm(mm,round=3)
printtm(mm,round=TRUE)     ## round logical informes if to use round.attr(x) but it is NULL so no rounding is performed
printtm(mm,round=NULL)     ## " where round=NULL work like TRUE
printtm(mm,round=FALSE)

#######################################—•°
## head attribute

mm <- matrix(1:100,nrow=10)
mm
head(mm)
head.attr(mm)
head.attr(mm) <- 3
attributes(mm)

mm       ## head.attr doesn't work without direct need thus it must be called somehow
## e.g. by the head() which calls head.tm() method which uses head.attr of x to set head parameter:
head(mm)
## but you may overwrite it:
head(mm,4)
head(mm,-4)  ## without 4 last rows

## you may also use print
print(mm)             ## head.attr not called as...
print(mm,head=NULL)   ## ...default for head is NULL which is equivalent to FALSE
print(mm,head=T)   ## uses head.attr
print(mm,head=2)


## head works also for more dimensions
mm <- array(1:100,dim=c(10,5,2))
mm
head(mm)    ## if DM is installed then head.array() defined there is in use which works differrently from standard
## head() method for arrays; DM's head.array() prints first n rows for all pages of array, i.e. dimensions further then
## rows and columns. In case of standard head() you obtain only first n elements of array, unlike for matrices where you
## get first n rows.
## BUT DM's head.array() is slow as it uses  eval(parse(text=txt))!

head.attr(mm) <- 3
head(mm)

#######################################—•°
## margin, transpose and head together

attributes(mm)
message.new(mm) <- Message("newMess","Never say never!")
mm

mm
margins(mm) <- c(1,3)
mm

transpose(mm) <- T
mm

head.attr(mm) <- 4
mm
print(mm,head=T)

print(mm,margins=T,transpose=F,head=T)
print(mm,margins=1,transpose=T,head=T)
print(mm,margins=1,transpose=T,head=2)

########################################################################################################################—•°
};rm(dummy)
