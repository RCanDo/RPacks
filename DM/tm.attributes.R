## ---------------------------------------------------------------------------------------------------------------------—•°
## Attributes for class "tm".
## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {DM}
##  margins()
##  margins<-
##  head.attr()
##  head.attr<-
##  round.attr()
##  round.attr<-
##  transpose()
##  transpose<-
##
## DEPENDENCIES
##  whileif()
##  Uses options set up in  {DM} options.R
##    getOption("class.attributes")
##    getOption("class.groups")
##
## TODO
## •
##
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
tm.class <- function(x){
## ------------------------------------------------------------------------------------------------—•°
## Checks if object has attributes which allow it to inherit from "tm" class.
## If so then adds "tm" to its class attribute.
## If not then removes "tm" from its class attribute.
## ------------------------------------------------------------------------------------------------—•°
   if( any( getOption("class.attributes")$tm %in% names(attributes(x)) ) ){
      class(x) <- union("tm",class(x))
   }else{
      class(x) <- setdiff(class(x),"tm")
   }
   if(all(class(x)%in%unlist(getOption("class.groups")[c("language","implicite")]))){class(x)<-NULL}
   x
}
## ---------------------------------------------------------------------------------------------------------------------—•°
is.tm <- function(x,fix=TRUE){
## ------------------------------------------------------------------------------------------------—•°
## Checks if x has any of the attributes which clasifies it as inheriting from "tm" class.
## If so then returns TRUE and adds "tm" to its class attribute.
## If not returns FALSE and removes "tm" from its class attribute.
## Thus it returns logical but has side effect of (un)setting class "tm" of the 'x'.
## This side effect may be turned off by setting  fix=FALSE  — function will then return
## only result of checking existence of tm-attributes while not fixing the class of x.
## IN other words, the basic task of the function is to check conditions for x to inherit from "tm"
## while fixing this class is optional but default.
## ------------------------------------------------------------------------------------------------—•°

   nam.x <- deparse(substitute(x))

   check <- any( getOption("class.attributes")$tm %in% names(attributes(x)) )

   if(fix){  ## fixing class "tm"
      if(check){
         class(x) <- union("tm",class(x))
      }else{
         class(x) <- setdiff(class(x),"tm")
      }
      if(all(class(x)%in%unlist(getOption("class.groups")[c("language","implicite")]))){class(x)<-NULL}

      ## side effect
      assign( nam.x , x , pos = parent.frame() )
   }
   ## result
   check
}
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
`tm.attr<-` <- function(x,value){
      attr(x,"margins")<-value$margins
      attr(x,"head")<-value$head
      attr(x,"round")<-value$round
      attr(x,"transpose")<-value$transpose
      tm.class(x)
}
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
margins = function(x){ attr(x,"margins") }
#> margins(aa)
## ---------------------------------------------------------------------------------------------------------------------—•°
`margins<-`<- function(x,value){
      attr(x,"margins")<-value
      tm.class(x)
}
## ---------------------------------------------------------------------------------------------------------------------—•°


## ---------------------------------------------------------------------------------------------------------------------—•°
head.attr = function(x){ attr(x,"head") }
#> margins(aa)
## ---------------------------------------------------------------------------------------------------------------------—•°
`head.attr<-`<- function(x,value){
      attr(x,"head")<-value
      tm.class(x)
}
## ---------------------------------------------------------------------------------------------------------------------—•°


## ---------------------------------------------------------------------------------------------------------------------—•°
round.attr = function(x){ attr(x,"round") }
#> margins(aa)
## ---------------------------------------------------------------------------------------------------------------------—•°
`round.attr<-`<- function(x,value){
      attr(x,"round")<-value
      tm.class(x)
}
## ---------------------------------------------------------------------------------------------------------------------—•°


## ---------------------------------------------------------------------------------------------------------------------—•°
transpose = function(x){ attr(x,"transpose") }
#> margins(aa)
## ---------------------------------------------------------------------------------------------------------------------—•°
`transpose<-`<- function(x,value){
      attr(x,"transpose")<-value
      tm.class(x)
}
## ---------------------------------------------------------------------------------------------------------------------—•°
