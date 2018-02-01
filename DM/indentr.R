########################################################################################################################—•°
## Pretty printing of lists, alternative for str() and other.
########################################################################################################################—•°
## FUNCTIONS HERE    {DM}
##  indentr()
##
## DEPENDENCIES
##  indent()
##  compact()
##  uncompact()           compact.R       ?
##  messages()       {DM} conditions.R
##  unattr()              attributes.R    ?
##
##
## REMARKS/WARNINGS
##
## TODO
## •
##
## DESCRIPTION
##  The aim, the idea, problems, solutions... briefly!, concerning the whole set of functions
##
####################################################################################################—•°
## AUTHOR: Arkadiusz Kasprzyk; akasp@...
## ver. 1.0
## PATH: D:\ROBOCZY\...
## start: 2017-03-01    last: 2017-09-26
########################################################################################################################—•°

########################################################################################################################—•°
indentr = function( ll, level=0, ind="   "
                  , leaves=c( getOption("class.groups")$implicite , getOption("class.groups")$common
                            ,"lm","glm","gam","arguments","condlist")
                  , add=NULL
                  , rm=NULL
                  , messages=coalesce(getOption("messages"),TRUE)
                  , info=TRUE
                  , as.text=FALSE
                  , compact=NULL
                  , vsep=0
                  , omit=NULL, delete = NULL
                  , attributes=FALSE
                  , ...
                  ){
###################################################################################################—•°
## Prints lists recursively (indentr) with names of entries coerced to their numeric position (if names are empty)
## and each level indented 3 spaces (default) more. Indentation may be set arbitrary.
##    Arguments
##  ll            a list or atomic vector
##  level = 0     level of the whole list (tree) to bigin printing with;
##                default 0 means that we the outermost list's entries ("roots") are printed with no indentation;
##                printing of deeper levels is performed recursively each time with level increased by one.
##                'level' is directly translated on the length of indentation which is ind*level.
##                See help for indent() to which 'level' is passed in place of 'times' parameter.
##  ind = "   "   indentation used, which may be any character string with special characters involved like
##                "\t", "\n" and other. See help for indent() to which 'ind' is passed. Default are 3 spaces.
##  leaves        vector of names of classes objects of which are treated as a leaves. Default are:
##                   "integer", "character", "numeric", "double", "complex", "logical", "factor",
##                   "matrix", "array", "table", "data.frame", "condlist",
##                   "lm", "glm", "gam"
##  add = NULL    additional classes to be treated as leaves. This is added to the 'leaves'.
##  rm = NULL     classes to be removed from 'leaves' thus objects of these classes will be penetrated by the function,
##                i.e. indentr() will be applied to their elements.
##  messages = coalesce(getOption("messages"),TRUE)
##                if object has "messages" attribute (see messages() in DM package) then it is properly printed below
##                an object, i.e. below all elements of this object. If FALSE then messages are not printed.
##  info = TRUE   each list's element is printed with their name atop and not indented yet
##                (indented one 'ind' less then the content of an element)
##                and with information on the class/typeof and length/dim of an element in the acute parenthesis, e.g.
##                   <data.frame, dim: 3 4>
##                This information is not printed if info = FALSE.
##  as.text = FALSE  if TRUE then character vectors are printed via cat( ,sep=" ") thus there will be no quotations
##                   and all special characters like \t or \n are applied
##                   (what may be sometimes annoying but sometimes very useful).
##  compact = NULL   argument passed to compact() which is then applied to the whole list i.e. list is coerced to
##                   object of class "compact" with parameters passed via 'compact' and then is printed as such.
##                   See examples of how it works also in help on compact().
##  vsep = 0   number of lines separating each item of the list. Notice that if vsep>0 then:
##             • messages are separated;
##             • in general if object inherits from leaves then separation does not work on its content and
##               it won't be separated from its name too (header, always printed one level higher thus indented less);
##             • header (name of an object and info about it) is separated from the content if object is a list which
##               does not inherit from leaves (one of the class mentioned there).
##  omit = NULL   character vector of NAMES of objects (list's entries) the content of which will not be printed,
##                however their name (and info if info = TRUE) will be printed normaly;
##  delete = NULL    character vector of NAMES of objects (list's entries) which will not be printer at all
##                   as if they were deleted from the list; even name (and info) of the object will not be printed.
##  attributes = FALSE  If FALSE (default) then no attributes will be printed (except maybe "class").
##             If TRUE then attributes will be normaly printed,
##             except "messages" (there is separate method in action for printing "messages")
##             and attributes which define class what influences their printing properties
##             if print.{class}() method is defined.
##             Currently only class "tm" is dependent on attributes,
##             namely "head", "round", "transpose" and "margins", i.e. if any of this attributes is present
##             then object is automaticaly given class "tm", and if there is none of them then "tm" class is
##             removed.  This is encoded in package DM via functions
##                fix.classes(), tm.class(), is.tm() and unattr().
##             Defining attributes are remembered in a options("class.attributes") which may be accessed
##             via  getOption("class.attributes")$class_name, e.g. getOption("class.attributes")$tm .
##
##    Remarks
## • indentr(x) on non-list object works exactly the same way as indent(), with the exception that default
##   repetition of indentation is 1 in indent() and 0 in indentr() (thus nothing will happen by default for the latter).
## • Writing print methods (for any class) avoid using
##     indentr(x,add="class",as.text=FALSE,...)   or   indentr(x,add="class",as.text=TRUE,...) and x is not "character"
##   where x inherits from the "class";
##   print(x) is used then what causes infininte loop and the error of stack overflow occurs.
##   If you wish to use indentr() within print method remember to pass an object unclassed i.e. write
##    indentr(unclass(x),...).
##
## • "\n" at the end of strings are immediately replaced with "" i.e. are ignored;
## • "\n" in the middle of a string results with new line indented according to level;
## • elements of a string vector are printed in one line separated with " ".
###################################################################################################—•°

leaves <- setdiff(union(leaves,add),rm)
leaves <- union(getOption("class.groups")$language,leaves)

   indent1 <- paste( rep(ind,level) , collapse="" )
   separate1 <- paste0( rep(paste0(indent1,"\n"),vsep) , collapse="")

   indent2 <- paste( rep(ind,level+1) , collapse="" )
   separate2 <- paste0( rep(paste0(indent2,"\n"),vsep) , collapse="")

messages.ll <- messages(ll)  ## see (*) near the end of the body

if(is.null(ll)){
   ## NOTHING
}else if( !any(union(class(ll),typeof(ll))%in%leaves) && length(ll)>0 ){  ## ll is not a leaf  &  not empty

   #      cat(paste0(rep("\n",vsep),collapse=""))  ## it doesn't work here... WHY???!!!

   ## missing names
   if(is.null(names(ll))){
      names(ll) <- paste0("[[",1:length(ll),"]]")
   }else{
      empties <- which(names(ll)=="")
      names(ll)[empties] <- paste0("[[",empties,"]]")
   }

   ## recurrence
   if(info){     ## prints info about class  and  dim
      for(k in 1:length(ll)){
         nam <- names(ll)[k]
         if(nam %in% delete){
            ## NOTHING
         }else{

            if(!is.null(dim(ll[[k]]))){
               dimll <- paste0(", dim: ",paste(dim(ll[[k]]),collapse=" "))
            }else{
               dimll <- paste0(", ",length(ll[[k]]))
            }

            #if(k==1) cat( separate1 ) else      ## commenting it results in the first el. of the list joined with the name of the list
            cat( separate2 )

            cat( indent1
               , paste0( nam , "  <" , paste(class(ll[[k]]),collapse=", ") , dimll , ">" )
               , "\n"
               , sep=""
               )

            if(nam %in% omit){
               indentr(NULL,level+1,ind,vsep=vsep) #cat( separate )  ## NOTHING
            }else{
               if(!is.null(compact)){ compact[2] <- 0 }
               indentr(ll[[k]], level+1, ind, leaves, messages=messages, info=info, as.text=as.text, compact=compact
                      , vsep=vsep, omit=omit, delete=delete, attributes=attributes, ...)
#               if( messages && !is.null(messages(ll[[k]])) ){
#                  cat(separate, indent, sep="") ; indent( messages(ll[[k]]), times=level+1, ind=ind )
#               }
            }

         }
      }
   }else{         ## do not print info about class
      for(k in 1:length(ll)){
         nam <- names(ll)[k]
         if(nam %in% delete){
            ## NOTHING
         }else{

            #if(k==1) cat( separate1 ) else
            cat( separate2 )

            cat( indent1
               , nam
               , "\n"
               , sep=""
               )

            if(nam %in% omit){
               indentr(NULL,level+1,ind,vsep=vsep) #cat( separate )  ## NOTHING
            }else{
               indentr(ll[[k]], level+1, ind, leaves, messages=messages, info=info, as.text=as.text, compact=compact
                      , vsep=vsep, omit=omit, delete=delete, attributes=attributes, ...)
#               if( messages && !is.null(messages(ll[[k]])) ){
#                  cat(separate, indent, sep="") ; indent( messages(ll[[k]]), times=level+1, ind=ind )
#               }
            }

         }
      }
   }

}else{
   attrs.all <- names(attributes(ll))
   attrs.mustbe <- union( unlist(getOption("class.attributes")[c("tm","compact")]) , "class" )                  ##
   attr.rm <- if( attributes ){ "messages" }else{ setdiff(attrs.all, attrs.mustbe) }    ## (*)
   ll <- unattr( ll, attr.rm )  ## fix.classes() works here
   if(is.null(compact)){
      indent(ll, times=level, ind=ind, as.text=as.text, messages=FALSE, ...)
   }else{
      indent(x=compact(ll,compact), times=level, ind=ind, as.text=as.text, messages=FALSE, ...)
      uncompact(ll)  ## for functions — they are not copyable thus they are turned to "compact"
                     ## even if they're primitive from base env. This is the source of some problems later...
   }
}

if( messages && !is.null(messages.ll) ){                                        ## (*)
   #cat( rep("\n",vsep) , sep="") ;
   cat( separate2 )
   indent( messages.ll, times=level, ind=ind, call=FALSE )
}

#cat( separate1 )

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
###################################################################################################—•°

 ## RELOADER — before it works you need to source("PacksAK.R"); it's best to use DM.R within pack's dir.
 loadPacksAK("DM")

###############################################################################—•°
## See examples for indent() first.
###############################################################################—•°
mm <- matrix(1:9,3)
indentr(mm)           ## indented with 0 times 3 spaces i.e. nothing will happen by default
indentr(mm,1)         ## indented with times 3 spaces
indentr(mm,5)         ## indented with 5 times 3 spaces
indentr(mm,1,"## ")   ## indented with 1 times "## "
indentr(mm,2,"## ")   ## strange; it's better to write
indentr(mm,1,"##    ")  ## or
indent(indentr(mm,2,"  "),ind="##")   ## see help on indent()


xx<-1:10;yy<-sample(10);mod<-lm(yy~xx)
## let's add some messages to elements of mod
message.new(mod) <- Message("message00","Message for the whole mod.")
message.new(mod) <- Message("message01","Another message for the whole mod.")
message.new(mod$qr) <- Message("message1","Message for the mod$qr.")
message.new(mod$qr$qr) <- Message("message20","Message for the mod$qr$qr.")
message.new(mod$qr$qr) <- Message("message21","Another message for the mod$qr$qr.")
message.new(mod$call) <- Message("messsageForCall","Look how I was called!")
message.new(mod$terms) <- Message("messsageForTerms","Look at my terms!")

indentr(mod)  ## mod is of class "lm" which is mentioned as a leaf, hence is diplayed via printing method for "lm"
indentr(mod,messages=F)          ## no messages
## you may remove "lm" from the leaves list and then indentr() will go deeper into it as it is a list
indentr(mod,rm="lm")             ##
indentr(mod,rm="lm",messages=F)  ##

## indentr() by default does not diplay attributes
indentr(mod,rm="lm")
indentr(mod,rm="lm",attributes=FALSE)  ## default
## if you wish to see attributes write
indentr(mod,rm="lm",attributes=TRUE)   ## "messages" attribute is not printed as it has separate method for printing
indentr(mod,rm="lm",attributes=TRUE,messages=FALSE)

indentr(mod,rm="lm",vsep=1)        ## numbers of lines seperating list's elements
indentr(mod,rm="lm",vsep=1,attributes=TRUE)
indentr(mod,rm="lm",vsep=2)

   mod$qr$qr

indentr(mod,rm="lm",info=FALSE)    ## no info about class and dim for every element
indentr(mod,rm="lm",info=FALSE,messages=FALSE)

indentr(mod,3)
indentr(mod,1,ind="#  ")           ## you may copy and paste into a script as a comment
indentr(mod,rm="lm",ind="#  ")     ## but this is not good!

## use indent() which does not make recurrence, combined with indentr()
indent(indentr(mod,rm="lm"),ind="#  ")          ## see help on indent()
indent(indentr(mod,rm="lm",vsep=1),ind="#  ")   ## you may copy and paste...
indent(indentr(mod,rm="lm",vsep=1,attributes=T),ind="#  ")
indent(indentr(mod,rm="lm",vsep=1,info=FALSE),ind="#  ")

## indentation with some marker may help to orient in a structure
indentr(mod,rm="lm",ind=".  ")
indentr(mod,rm="lm",ind="|  ")
indentr(mod,rm="lm",ind="|  ",attributes=T)
indentr(mod,rm="lm",ind=".  ",vsep=1)
indentr(mod,rm="lm",ind=".  ",vsep=2)
indentr(mod,rm="lm",ind="|  ",vsep=1)
indentr(mod,rm="lm",ind="|  ",vsep=2)
indent(indentr(mod,rm="lm",ind="|  ",vsep=1),ind="#  ")

indentr(mod,rm="lm",ind="|  ",vsep=1,attributes=T)

## You may wish not to see content of some elements
indentr(mod,rm="lm",omit="terms")            ## only name of nod$terms printed
indentr(mod,rm="lm",omit=c("terms","model"))
indentr(mod,rm="lm",omit=c("terms","model"),vsep=1,attributes=T)
indentr(mod,rm="lm",omit=c("terms","model"),ind="|  ",vsep=1,attributes=T)

## You may wish not to see some elements at all (as if they were deleted from list)
indentr(mod,rm="lm",delete="terms")          ## mod$terms not visible at all
indentr(mod,rm="lm",delete=c("terms","model"))
indentr(mod,rm="lm",delete=c("terms","model"),ind="|  ",vsep=1,attributes=T)

## mixture of both
indentr(mod,rm="lm",omit=c("qr","call"),delete=c("terms","model"))
indentr(mod,rm="lm",omit=c("qr","call"),delete=c("terms","model"),vsep=1)

indentr(mod,rm="lm",omit=c("qr","call"),delete=c("terms","model"),vsep=1,ind="|  ",attributes=T)

###############################################################################—•°
## list of messages
ll0 <- list(b = c("message0\n","message1\nqq","message2","m3") , "a"
            ,c = c("message1\n","message2","m3"),d = c("message1","message2"),"empty_list"=list())
ll0
indentr(ll0)
indentr(ll0,info=FALSE)
indentr(ll0,as.text=TRUE)
indentr(ll0,info=FALSE,as.text=TRUE)      ## the best way to print nested messages
                                          ## BUT only when each message in a single vector

###########################################################—•°
## See examples for compact() first.
###########################################################—•°
indentr(ll0,compact=TRUE)                 ##    info about class and dim ;
                                          ## TRUE is coerced to 1 what means only the first element is displayed
indentr(ll0,compact=TRUE, info=FALSE)     ## compact mode works always as if as.text=TRUE, i.e. all character vectors
                                          ## are diplayed as text, and all \n, \t, etc. takes effect.

indentr(ll0,compact=TRUE,info=FALSE)      ## NO info about class and dim
indentr(ll0,compact=c(0,0),info=FALSE)    ## the whole vectors with no info about length/dim
   indentr(ll0,compact=c(0,0),info=TRUE)
indentr(ll0,compact=c(10,0),info=FALSE)
indentr(ll0,compact=c(10,1),info=FALSE)
   indentr(ll0,compact=c(10,1),info=TRUE) ## 'info' option of indentr() always prevails
indentr(ll0,compact=c(10,2),info=FALSE)
   indentr(ll0,compact=c(10,2),info=TRUE)
##!  Hence 'info' prevails over 'side' of 'compact'!
##  If info TRUE (default!) then 'side' of compact parameter (the second) is set to 0.

## repeated names
infos <- list("info" = "First info about sth.", "info" = "Second info about sth."
             , "info" = list( "Third info about sth.", "sub-info 4" = "Fourth info about sth." ) )
indentr(infos)
indentr(infos,compact=c(0,0),info=FALSE)      ## NO info about class and dim

indentr(infos,compact=c(0,0),info=FALSE,vsep=1)

#####

indentr(mod,compact=TRUE)        ## in a compact mode indentr() dwelves into a list, even if it inherits from leaves;
indentr(mod,compact=TRUE,ind="|  ") ## but indentation is not setable
indentr(mod,compact=TRUE,vsep=1)    ## vsep is also not setable
## and only the outermost messages are printed

## BUT if you remove class from leaves then
indentr(mod,rm="lm",ind="|  ",compact=T)           ## indentation is setable
indentr(mod,rm="lm",ind="|  ",vsep=1,compact=T)           ## separation also works
## and all messages are visible

indentr(mod,rm="lm",compact=T)      ## internal messages are also visible
indentr(mod,rm="lm",compact=5)      ## number of elements to see for each atomic vector
indentr(mod,rm="lm",compact=c(5,0)) ## 0 means "do not displaty info about length/dim of a vector"
indentr(mod,rm="lm",compact=c(5,1)) ## 1 means "display info at the same line (at the end)"
## but if doesn't work until you turn off 'info' of indentr():
indentr(mod,rm="lm",compact=c(5,1),info=F)
indentr(mod,rm="lm",compact=c(5,2),info=F) ## 2 means "display info below"

indentr(mod,rm="lm",compact=c(5,1),info=F,attributes=T)   ## attributes cannot be seen in compact mode

indentr(mod,rm="lm",ind="|  ",compact=c(5,2))
indentr(mod,rm="lm",ind="|  ",compact=c(5,2),info=F)
indentr(mod,rm="lm",ind="|  ",vsep=1,compact=c(5,2),info=F)

indentr(mod,rm="lm",ind="|  ",vsep=1,compact=c(5,2),info=F,digits=4)     ## only 4 significant digits


###############################################################################—•°
ll <- list(
   list("a",b = c("message0\n","message1\nqq","message2","m3"),c = c("message1\n","message2","m3"),d = c("message1","message2"))
, two = 1:3
, letters[1:3]
, four = list( ab = list( 3:6 , zz=LETTERS[3:4]) , mm = mm , "nothing" , "3"= 33 , model = mod)
)

indentr(ll)
indentr(ll,vsep=1)

indentr(ll,ind="|  ")
indentr(ll,ind=".  ")
indentr(ll,ind="|  ",vsep=1)
indentr(ll,ind="|  ",vsep=1,omit="model")
indentr(ll,ind="|  ",vsep=1,delete="model")
indentr(ll,ind="|  ",vsep=2)
indentr(ll,ind="|  ",vsep=2,rm="lm")
indentr(ll,ind=".  ",vsep=1)

## removing from 'leaves' list
indentr(ll,rm=c("lm"))         ## indentr()  gets deeper into "lm" object (which is not atomic!)
indentr(ll,ind="  |",vsep=2,rm="lm")
indentr(ll,rm=c("lm","data.frame"))   ## indentr()  gets deeper into "lm" and "data.frame" objects
indentr(ll,rm=c("lm","data.frame"),omit=c("qr","terms"),ind="  |",vsep=1)

## using with 'compact'
indentr(ll,rm=c("lm","data.frame"), compact=5)
indentr(ll,rm=c("lm","data.frame"), compact=5, info=FALSE)
indentr(ll,rm=c("lm","data.frame"), compact=5, info=FALSE, omit=c("qr","terms"), ind="  |",vsep=1)

indentr(ll,ind="  |",rm=c("lm","data.frame"))

indentr(ll,compact=5)
indentr(ll,ind="  |",rm=c("lm","data.frame"))

## make some inner element compact
ll$four$mm <- compact(ll$four$mm,c(4,2))
indentr(ll)
indentr(ll,vsep=1)
##
ll$four$model$model <- compact(ll$four$model$model,c(4,0))
indentr(ll,vsep=1,rm="lm")


indentr(quote(sin(x^2)))
class(quote(sin(x^2)))
indentr(expression(sin(x^2)))

indentr({x<-1;sin(x)})
indentr(quote({x<-1;sin(x)}))
class(quote({x<-1;sin(x)}))


########################################
cdl <- structure( list(
   "a" = list( variables = "b", conditions = quote(x<a) , substitute = quote(0) )
  ,"b" = list( variables = "c", conditions = quote(x>=a) , substitute = quote(0) )
)
, class="condlist")

cdl

indentr(cdl)

}
########################################################################################################################—•°
rm(dummy)