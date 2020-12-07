## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {DM}
##  node()
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

vars_node <- function(
    all = character(0),
    id = character(0),
    split = character(0),
    Y = character(0),
    X = NULL
){
## -----------------------------------------------------------------------------—•°
## -----------------------------------------------------------------------------—•°
if(is.null(X)){
    X <- setdiff(all, union(id, Y))
}
structure(list(all=all, id=id, split=split, Y=Y, X=X), class=c("vars_node", "node", "list"))
}
## -----------------------------------------------------------------------------—•°


## -----------------------------------------------------------------------------------------------------------------------—•°
intersect.vars_node <- function(vn, varset){
stopifnot(is.character(varset))
for(n in names(vn)){
    vn[[n]] <- intersect(vn[[n]], varset)
}
return(vn)
}

setdiff.vars_node <- function(vn, varset){
stopifnot(is.character(varset))
for(n in names(vn)){
    vn[[n]] <- setdiff(vn[[n]], varset)
}
return(vn)
}

print.vars_node <- function(vn){
print(compact(vn, c(5, 2)))
}

print.vars_node_list <- function(vnl){
print(compact(vnl, c(5, 2)))
}

## -----------------------------------------------------------------------------------------------------------------------—•°
model_node <- function(
    all = character(0),
    Y = character(0),
    X0 = character(0),
    X = character(0),
    formula0 = character(0),
    formula = character(0),
    model0 = NA,
    model = NA
){
structure( list( all=all, Y=Y, X0=X0, X=X, formula0=formula0, formula=formula,
    model0=NA, model=NA                              #=NULL  # Error in class(x) <- union("compact", class(x)) :
                                                     #  attempt to set an attribute on NULL
    ), class=c("model_node", "node"))
}

## -----------------------------------------------------------------------------------------------------------------------—•°
intersect.model_node <- function(mn, varset){
stopifnot(is.character(varset))
for(n in names(mn)){
    if(is.character(mn[[n]])){
        mn[[n]] <- intersect(mn[[n]], varset)
    }
}
return(mn)
}

setdiff.model_node <- function(mn, varset){
stopifnot(is.character(varset))
for(n in names(mn)){
    if(is.character(mn[[n]])){
        mn[[n]] <- setdiff(mn[[n]], varset)
    }
}
return(mn)
}

print.model_node <- function(mn){
print(compact(mn, c(5, 2)))
}

print.model_node_list <- function(mnl){
print(compact(mnl, c(5, 2)))
}


## -----------------------------------------------------------------------------------------------------------------------—•°


## -----------------------------------------------------------------------------------------------------------------------—•°
