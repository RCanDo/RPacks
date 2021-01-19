## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {DM}
##  reclass()
##  reclassr()
##
## DEPENDENCIES
##
## TODO
##
## COMMENT
##  Reclassing object - changing class by adding or replacing value to class attribute;
##  reclass(obj) for simple object (atomic);
##  reclassr(ll) for recurrent reclassing of lists and it's elements.
## ---------------------------------------------------------------------------------------------------------------------—•°


## ---------------------------------------------------------------------------------------------------------------------—•°

reclass <- function(obj, class_old, class_new, add=TRUE, except=""){
## -------------------------------------------------------------------------------------------------—•°
## -------------------------------------------------------------------------------------------------—•°
if(inherits(obj, class_old) && !inherits(obj, except)){
    if(add){
        class(obj) <- union(class_new, class(obj))
    }else{
        class(obj) <- class_new
    }
}
return(obj)
}

## ---------------------------------------------------------------------------------------------------------------------—•°
reclassr <- function(ll, class_old, class_new, add=TRUE, except=""){
## -------------------------------------------------------------------------------------------------—•°
## -------------------------------------------------------------------------------------------------—•°
if(is.list(ll)){
    for(i in 1:length(ll)){
        ll[[i]] <- reclassr(ll[[i]], class_old, class_new, add, except)
    }
    if("list" %in% class_old){
        ll <- reclass(ll, class_old, class_new, add, except)
    }
}else{
    ll <- reclass(ll, class_old, class_new, add, except)
}
return(ll)
}

## ---------------------------------------------------------------------------------------------------------------------—•°


## ---------------------------------------------------------------------------------------------------------------------—•°
## EXAMPLES ------------------------------------------------------------------------------------------------------------—•°
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
dummy = function(){
## This is dummy function — it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## — in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
## -------------------------------------------------------------------------------------------------—•°
## RELOADER — before it works you need to source("RCanDo.R"); it's best to use {package_name}.R within pack's dir.
 loadPacksAK("DM")
 source(FILE$utils)
## -------------------------------------------------------------------------------------------------—•°
ll <- list( a='a',
            b=1:3,
            c= structure(
                list( p=data.frame(matrix(1:12, 3, 4)),
                      q=list(xx=1:3,
                             yy=sample(3),
                             zz=letters[1:3]
                             ),
                      r=sample(4),
                      s=LETTERS[1:4]
                    ),
                class=c("list", "pqr"))
          )
indentr(ll)

class(reclass(ll, "list", "qq"))
indentr(reclass(ll, "list", "qq"))
class(reclass(ll$c, "list", "qq"))
class(reclass(ll$c$p, "list", "qq"))
class(reclass(ll$c$p, c("list", "data.frame"), "qq"))

class(reclassr(ll, "list", "qq"))
indentr(reclassr(ll, "list", "qq"))

## notice that
inherits(data.frame(matrix(1:12, 3, 4)), "list")    # FALSE      ???
is.list(data.frame(matrix(1:12, 3, 4)))             # TRUE       !!!

indentr(reclassr(ll, c("list", "data.frame"), "qq"))

indentr(reclassr(ll, "character", "qq"))
indentr(reclassr(ll, "integer", "qq"))
indentr(reclassr(ll, "integer", "qq"), rm="data.frame", compact=5)

indentr(reclassr(ll, c("integer", "character"), "qq"))
indentr(reclassr(ll, c("integer", "character"), "qq", except="integer"))

indentr(reclassr(ll, "character", c("qq", "ryq")))

## ---------------------------------------------------------------------------------------------------------------------—•°
}; rm(dummy)
