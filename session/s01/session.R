## ---------------------------------------------------------------------------------------------------------------------—•°
## Work session 01
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°

PATH = list()
PATH$ROOT = "C:/Users/arkadiusz.kasprzyk"
PATH$projects = file.path(PATH$ROOT, "Projects")
PATH$session = file.path(PATH$projects, "R/RCanDo/session/s01")
setwd(PATH$session)


## ----------------------------------------------------------------------------—•°

PATH$packs = file.path(PATH$projects, 'R/RCanDo')
source(file.path(PATH$pack, "RCanDo.R"))
# loadPacksAK("DM")     # reloading one of the packs

## ---------------------------------------------------------------------------------------------------------------------—•°

condlist <- function(...){
ll <- list(...)
stopifnot(any(sapply(ll, function(l){"conditem" %in% class(l)})))
structure(ll, class="condlist")
}

conditem <- function(variables=NULL, conditions=NULL, inputs=NULL, substitute=NULL, subindex=NULL){
ll <- list(
        variables=variables,
        conditions=conditions,
        inputs=inputs,
        substitute=substitute,
        subindex=subindex
    )
ll <- ll[!sapply(ll, is.null)]
structure(
    ll,
    class="conditem")
}


cl0 <- condlist(
    "ee" = conditem(  ## x
        variables = c("aa", "bb") , ## a, b, c,...
        conditions = quote(a==b) ,
        inputs = c("aa", "bb") ,    ## a, b, c,...
        substitute = quote(a==b)
        )
    , "ee" = conditem(  ## x
        variables = c("aa", "bb") , ## a, b, c,...
        conditions = quote(a!=b) ,
        inputs = c("aa", "bb") ,    ## a, b, c,...
        substitute = quote(a==b)
        )
    )
cl0

## -------------------------------------------------------------------------------------------------
name = "Qq"
val = 34
sprintf("%s is (%i)", name, val)
name %P% val

## ---------------------------------------------------------------------------------------------------------------------—•°
save.image()