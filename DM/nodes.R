## ---------------------------------------------------------------------------------------------------------------------—•°
## nodes & lists of nodes
## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {dm}
##  node()
##
## DEPENDENCIES
##  funa()        {package} file.R     [defalt {package} is the same; default file.R is the same as function name]
##  funb()
##
## REMARKS/WARNINGS
##  See other nodes and list of nodes:
##  vars.node.R
##  data.node.R
##  model.node.R
##
## TODO
## •
##
## DESCRIPTION
## In general x.list and x.node are NOT synonyms (for all the classes below in this module):
## as by design x.node is a node of x.list.
## BUT
## config.list, config.node, par.list, par.node  ARE all synonyms !!!
## ---------------------------------------------------------------------------------------------------------------------—•°


par.node <- function(...){
structure(list(...), class=c("par.node", "list"))
}

print.par.node <- function(x){
#indentr(pl, compact=TRUE)
for(n in names(x)){
    if(is.atomic(x[[n]])){
        if(length(x[[n]])<=1){
            out_n <- if(is.character(x[[n]])) paste0("\"", x[[n]],"\"") else as.character(x[[n]])
            cat(n, " : ", out_n,"\n", sep="")
        }else{
            cat(n, " :\n", sep="")
            indent(x[[n]])
        }
    }else{
        cat(n, " :\n", sep="")
        indent(x[[n]])
    }
}
}

par.list <- function(...){
structure(list(...), class=c("par.list", "list"))
}

print.par.list <- function(x){
print.par.node(x)
}

## -------------------------------------------------------------------------------------------------—•°

config.node <- function(...){
structure(list(...), class=c("config.node", "list"))
}

print.config.node <- function(x){
print.par.node(x)
}

## config.list  - new name for par.list
config.list <- function(...){
structure(list(...), class=c("config.list", "list"))
}

print.config.list <- function(x){
print.par.node(x)
}


## -------------------------------------------------------------------------------------------------—•°
## probably not used...

log.node <- function(...){
structure(list(...), class=c("log.node", "list"))
}

print.log.node <- function(x){
print.par.node(x)
}

log.list <- function(...){
ll <- list(...)
stopifnot(all(sapply(ll, inherits,  "log.node")))     ## so log.lists cannot be nested - log.list consists only of log.nodes.
structure(ll, class("log.list", "list"))
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
## -------------------------------------------------------------------------------------------------—•°



## ---------------------------------------------------------------------------------------------------------------------—•°
}; rm(dummy)
