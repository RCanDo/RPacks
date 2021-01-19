## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {DM}
##  vars.node()
##  vars.list()
##  print.vars.node()
##  print.vars.list()
##  intersect.vars.node()
##  union.vars.node()
##  setdif.vars.node()
##  formula.vars.node()
##
##
## DEPENDENCIES
##
## TODO
##
## COMMENT
##  There is no generic data() method in base R.
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°

vars.node <- function(
    all = character(0),
    id = character(0),
    split = character(0),
    Y = character(0),
    X = NULL,
    ...
){
## -------------------------------------------------------------------------------------------------—•°
## List of groups (types) of variables within some data.frame.
## Each entry of the list is a **character vector** of **names** of variables being of the given type.
## Notice that each group is a user's choice and definition.
## The default entries are introduced for convenience as most common cases;
## however, one may add own entries.
##    E.g.
##  all = character(0)     all variables names from this data.frame;
##                         at the creation is re-calculated as union of `id`, `split`, `Y` and `X`;
##  id = character(0)      ID vriables like row numbers/names etc.
##  split = character(0)   variables meant to split data into smaller sub-data.frames
##                         usually factors;
##  Y = character(0)       variables meant to be target variables in model fitting;
##  X = NULL               variables meant to be predictors ";
##                         if left NULL (default) then calculated as set difference of `all` and
##                         the set sum of `id`, `split`, `Y`;
   ##  X0 = NULL              usually some subset of X variables like a final selection of predictors;
   ##                         at the onset copy of X ready for backward stepwise modelling;         ## X
   ##                         !!! for consistency it should always be a subset of X !!!
##  ...                    user defined groups of variabels names - should be passed as
##                         name=c(names of variables);
##
##   Remark
## All entries should be character vectors of names of variables!
## Otherwise methods intersect() and setdiff() will not work.
## -------------------------------------------------------------------------------------------------—•°
if(is.null(X)){
    X <- setdiff(all, c(id, split, Y))
}
all = union(all, c(id, split, Y, X))
ll <- list(all=all, id=id, split=split, Y=Y, X=X, ...)
structure(
    ll,
    class=c("vars.node", "node", "list")
    )
}

## -------------------------------------------------------------------------------------------------—•°
vars.list <-function(...){
structure(list(...), class=c("vars.list", "list"))
}

## -------------------------------------------------------------------------------------------------—•°
print.vars.node <- function(x){
indentr(x, compact=c(9, 2), info=FALSE)
}

## -------------------------------------------------------------------------------------------------—•°
print.vars.list <- function(x){
indentr(x, compact=c(9, 2), info=FALSE)
}


## -------------------------------------------------------------------------------------------------—•°
intersect.vars.node <- function(x, varset){
stopifnot(is.character(varset))
for(n in names(x)){
    x[[n]] <- intersect(x[[n]], varset)
}
return(x)
}

## -------------------------------------------------------------------------------------------------—•°
setdiff.vars.node <- function(x, varset){
stopifnot(is.character(varset))
for(n in names(x)){
    x[[n]] <- setdiff(x[[n]], varset)
}
return(x)
}

## -------------------------------------------------------------------------------------------------—•°
union.vars.node <- function(x, varset){
## -------------------------------------------------------------------------------------------------—•°
##  x         vars.list
##  `varset`   is a named list of length 1, e.g. list(group=c('a', 'b')) --
##             then 'group' will be used as a name for the new entry of the `x`;
##             it may be also name already present in `x` and then content of the relevant element
##             will be recalculated as union of old and new values;
##             it may be also just vector of names in which case will be added to group '(new)';
##             in both cases entry 'all' is recalculated as union of old and new values;
## -------------------------------------------------------------------------------------------------—•°
if(is.list(varset)){
    ## adding new var names as new group (entry of the `vn`)
    ## varset should have length 1 !!!                                                              TODO !!!
    v_name = names(varset)[1]        ## only first element is taken
    stopifnot(is.character(varset[[v_name]]))
    if(v_name %in% names(x)){
        x[[v_name]] <- union(varset[[v_name]], x[[v_name]])
    }else{
        x[[v_name]] <- varset[[v_name]]
    }
    x$all <- union(x[[v_name]], x$all)
}else if(is.character(varset)){
    x$all <- union(varset, x$all)
    x[["(new)"]] <- varset
}else{
    stop("`varset` must be named list or character vector.")
}
return(x)
}

## -------------------------------------------------------------------------------------------------—•°
formula.vars.node <- function(vn, Y="Y", X="X", bin=NULL, s=FALSE, package="mgcv"){
## -------------------------------------------------------------------------------------------------—•°
##  bin
##           if character - ... don't use it
##           if NULL - ...
##           if TRUE - ...
##           if FALSE - ...
##  s        logical or numeric > 0;
##           if FALSE or 0 then 's' option for gam() is NOT used
##           if TRUE then 's(x)' is option is created within formula;
##           if s > 0 then 's(x, df=`s`)' is created or 's(x, k=`s`)'; see below;
##  package  "gam" or "mgcv";
##           if "gam" then smooth term is of the form "s(x, df=`s`)"
##           if "mgcv" then smooth term is of the form "s(x, k=`s`)"
##           hence it is relevant only when `s` is a number > 1;
## -------------------------------------------------------------------------------------------------—•°

if(is.null(bin)){
    if(Y=="Ybin"){
        bin <- TRUE
    }else{
        bin <- FALSE
    }
}

if(is.character(bin)){
    ## `bin` is proper variable name
    if(! bin %in% vn$all){
        stop(paste0("there is no '", bin, "' in `all` entry."))
    }
    Yname <- paste0(bin, "_bin")
}

if(is.logical(bin)){
    if(bin){
        Yname <- paste0(vn[[Y]], "_bin")
    }else{
        Yname <- vn[[Y]]
    }
}

s <- s[1]

if(s){
    if(is.numeric(s)){
        if(package=="gam"){
            deg_sign = ", df="
        }else if(package=="mgcv"){
            deg_sign = ", k="
        }else{
            stop("`package` must be one of 'gam' or 'mgcv' (default).")
        }
        svars <- paste0("s(", vn[[X]], deg_sign, s, ")")
    }else{
        svars <- paste0("s(", vn[[X]], ")")
    }
    form <- formula(paste(Yname, paste(svars, collapse=" + "), sep=" ~ "))
}else{
    form <- formula(paste(Yname, paste(vn[[X]], collapse=" + "), sep=" ~ "))
}

form

}  ##----END----##


## -------------------------------------------------------------------------------------------------—•°
update.vars.node <- function(vn, what="all"){
switch(what,
    all = { vn$all <- union(union(vn$id, vn$split), union(vn$Y, vn$X)) },
    id = { vn$id <- setdiff(vn$all,  c(vn$split, vn$Y, vn$X)) },
    split = { vn$split <- setdiff(vn$all,  c(vn$id, vn$Y, vn$X)) },
    Y = { vn$Y <- setdiff(vn$all,  c(vn$id, vn$split, vn$X)) },
    X = { vn$X <- setdiff(vn$all,  c(vn$id, vn$split, vn$Y)) }
    )
vn
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

## -----------------------------------------------------------------------------—•°
## vars.node()

v1 <- vars.node(all=letters[1:4], id=letters[3], Y='Y', qq=letters[7], ryq=letters[11])
v1
intersect(v1, letters[2:12])
union(v1, list(z=c('x', 'y', 'z')))
union(v1, list(X=c('x', 'y', 'z')))
union(v1, c('x', 'y', 'z'))

## ---------------------------------------------------------------------------------------------------------------------—•°
}; rm(dummy)