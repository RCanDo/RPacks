## ---------------------------------------------------------------------------------------------------------------------—•°
## System of creating and passing 'conditions', i.e. messages, warnings, notes and errors.
## Remembering messages/warnings/... within an attribute "messages".
## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {DM}
## basic objects creators:
##  condition(), is.condition()
##  Error(), Warning(), Message(), Note()
##
##  messages(), messages<-, print.messages()
##  newMessage(), message.new()
##
##  notes(), notes<-, print.notes()
##
## DEPENDENCIES
##
##
## TODO
## •
##
## DESCRIPTION
## • SCHEME OF USE interactively.
## > x                     ## some object
## > newMessage(x) <- Message( messClass , text )     ## adding new message
## > messages(x)           ## printing all messages, the same as
## > attr(x,"messages")
## > print(messages(x))    ## there is print method for printing messages which has second argument:
## > print(messages(x),call=FALSE)     ## recommended to use within other print methods.
##
## • SCHEME OF USE within functions.
##   0. (old, not necessary now) Result of function created at the end (more intuitive).
##  function(...){
##    messages <- structure( list() , class="messages" )
##    ...
##    msg1 <- Message( messClass1 , text1 )    ## may be also  Warning() or Note() or Error() all having the same args.
##    messages <- c( messages , list(msg1) )
##    ...
##    msg2 <- Message( messClass2 , text2 )
##    messages <- c( messages , list(msg2) )
##    ...
##    result <- ...   ## main result of the function
##    messages(result) <- messages
##    result
##  }
##
##   1. (new, simpler) Result of function created at the end (more intuitive).
##  function(...){
##    messages <- structure( list() , class="messages" )
##    ...
##    newMessage(messages) <- Message( messClass1 , text1 ) ## may be also  Warning() or Note() or Error() ...
##    ...
##    newMessage(messages) <- Message( messClass2 , text2 )
##    ...
##    result <- ...   ## main result of the function
##    messages(result) <- messages
##    result
##  }
##
##   2. Result of function created at the beginning (simpler as identical to interactive use).
##  function(...){
##    result <- structure( ... , class="someClass")       ## create result structure at the beginning of the body
##      ## then you may use
##    newMessage(result) <- Message( messClass1 , text1 ) ## may be also  Warning() or Note() or Error() ...
##    ...
##    newMessage(result) <- Message( messClass2 , text2 )
##    ...
##    result
##  }
##
##
## REMARKS/WARNINGS
## • List of messages (having class "messages") may be named or unnamed; these names are ignored while printing messages
##   and class of each message (condition) is printed in place of them.
##
## -------------------------------------------------------------------------------------------------—•°
## AUTHOR: Arkadiusz Kasprzyk; akasp@...
## ver. 1.0
## PATH: D:\ROBOCZY\...
## start: 2017-03-01    last: 2017-09-26
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
condition <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = union(subclass, "condition"),
    list(message = message, call = call),     ## This is the proper structure of object of class "condition"
    ...
  )
}
is.condition <- function(x) inherits(x, "condition")
## ---------------------------------------------------------------------------------------------------------------------—•°

Error   <- function(subclass,message,call=sys.call(-1),...){ condition(subclass=union(subclass,"error"),  message=gsub('(\n| |\t)+$','',gsub('^(\n| |\t)+','',message)),call,...) }
Warning <- function(subclass,message,call=sys.call(-1),...){ condition(subclass=union(subclass,"warning"),message=gsub('(\n| |\t)+$','',gsub('^(\n| |\t)+','',message)),call,...) }
Message <- function(subclass,message,call=sys.call(-1),...){ condition(subclass=union(subclass,"message"),message=gsub('(\n| |\t)+$','',gsub('^(\n| |\t)+','',message)),call,...) }
Note    <- function(subclass,message,call=sys.call(-1),...){ condition(subclass=union(subclass,"note"),   message=gsub('(\n| |\t)+$','',gsub('^(\n| |\t)+','',message)),call,...) }

## ---------------------------------------------------------------------------------------------------------------------—•°


## ---------------------------------------------------------------------------------------------------------------------—•°
messages = function(x){ attr(x,"messages") }
#> margins(aa)
## ---------------------------------------------------------------------------------------------------------------------—•°
`messages<-`<- function(x,value){
   if(!is.null(value))  class(value) <- union("messages",class(value))
   attr(x,"messages")<-value
   x
}
## ---------------------------------------------------------------------------------------------------------------------—•°
print.messages = function(m,call=TRUE,...){
## ------------------------------------------------------------------------------------------------—•°
##  m       object of class "messages" which is a list of messages all having their own class;
##          usually it is an attribute "messages" of another object.
##  call    each message has entry "call" where the environment from which it is called is rememebered;
##          it will be printed if call=TRUE (default).
## ------------------------------------------------------------------------------------------------—•°
if(length(m)>0){
   names(m) <- unlist(lapply(m,function(x)class(x)[1]))   ## message classes are used as names of messages list
   if(!call){ m <- lapply(m,function(x){x$message}) }     ## getting rid of "call" entry of every message
   cat("Messages","\n")
   indentr(unclass(m), level=1, ind="  ", as.text=TRUE, info=FALSE, ...)
}else{
   cat("No messages.\n")
}
}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°
#> messages(aa)<-"qq"

## ---------------------------------------------------------------------------------------------------------------------—•°
`newMessage<-`<- function(x,value){
## ------------------------------------------------------------------------------------------------—•°
if(!inherits(value,"condition")) warning("This message is custom i.e. not regular with a structure defined by condition().
Create messages using one of Error(), Warning(), Message(), Note() from DM package.")
   ##
if(inherits(x,"messages")){
   x <- structure(c(x,list(value)),class="messages")
}else{
   if(is.null(messages(x))) messages(x) <- structure(list(),class="messages")
   messages(x) <- c(messages(x),list(value))
}
   ##
x
}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°
`message.new<-`<- function(x,value){   ## synonym for  newMessage<-
## ------------------------------------------------------------------------------------------------—•°
if(!inherits(value,"condition")) warning("This message is custom i.e. not regular with a structure defined by condition().
Create messages using one of Error(), Warning(), Message(), Note() from DM package.")
   ##
if(inherits(x,"messages")){
   x <- c(x,list(value))
}else{
   if(is.null(messages(x))) messages(x) <- structure(list(),class="messages")
   messages(x) <- c(messages(x),list(value))
}
   ##
x
}  ##----END----##


## ---------------------------------------------------------------------------------------------------------------------—•°-- EXAMPLES
dummy = function(){
    ## RELOADER —— before it works you need to source("PacksAK.R"); it's best to use EfficiencyCurves.R within pack's dir.
    loadPacksAK("DM")

(mm = matrix(sample(1:12),nrow=3))
(mm = matrix(sample(1:12),nrow=4))

messages(mm) <- "This message cannot be forgotten."
mm    ## just ordinary text, no info about call
messages(mm)

## Use Message() to create more professional look, with info about call
messages(mm) <- Message("someMessage","This message cannot be forgotten.")
mm    ## ugly print
messages(mm)
conditionCall(messages(mm))      ## called from .GlobalEnv so call is empty but if called from within function then
                                 ## info about it would be stored

## By design "messages" is a list of messages (created by one of Error(), Warning(), Message(), Note())
## having class "messages". This list may have length == 1
messages(mm) <- structure(list(Message("someMessage","This message cannot be forgotten.")),class="messages")
mm
messages(mm)
messages(mm)[]

messages(mm) <- NULL
## more convienient way
newMessage(mm) <- Message("someMessage","This message cannot be forgotten.")
## or synomym
# message.new(mm) <- Message("someMessage","This message cannot be forgotten.")
mm
messages(mm)
newMessage(mm) <- Message("anotherMessage","This message is even more important.")
mm

## ---------------------------------------------------------------------------------------------------------------------—•°
};rm(dummy)


## ---------------------------------------------------------------------------------------------------------------------—•°
notes = function(x){ attr(x,"notes") }
#> margins(aa)
## ---------------------------------------------------------------------------------------------------------------------—•°
`notes<-`<- function(x,value){
   if(!is.null(value))  class(value) <- union("notes",class(value))
   attr(x,"notes")<-value
   x
}
#> notes(aa)<-"qq"
## ---------------------------------------------------------------------------------------------------------------------—•°
print.notes = function(m,...){
## ------------------------------------------------------------------------------------------------—•°
if(length(m)>0){
   names(m) <- unlist(lapply(m,function(x)class(x)[1]))
   if(!call){ m <- lapply(m,function(x){x$message}) }
   cat("Notes","\n")
   indentr(unclass(m),as.text=TRUE,info=FALSE,...)
}else{
   cat("No notes.\n")
}
}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°
