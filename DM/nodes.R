## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {DM}
##  node()
##
## DEPENDENCIES
##
## TODO
##
## COMMENT
##
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
## par.list
pars.list <- function(){
structure(list(), class=c("pars.list", "list"))
}

print.pars.list <- function(pl){
indentr(pl, compact=TRUE)
}

## ---------------------------------------------------------------------------------------------------------------------—•°
## vars.node

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
vars.list <-function(){
structure(list(), class=c("vars.list", "list"))
}

## -------------------------------------------------------------------------------------------------—•°
print.vars.node <- function(vn){
indentr(vn, compact=c(9, 2), info=FALSE)
}

## -------------------------------------------------------------------------------------------------—•°
print.vars.list <- function(vl){
indentr(vl, compact=c(9, 2), info=FALSE)
}


## -------------------------------------------------------------------------------------------------—•°
intersect.vars.node <- function(vn, varset){
stopifnot(is.character(varset))
for(n in names(vn)){
    vn[[n]] <- intersect(vn[[n]], varset)
}
return(vn)
}

## -------------------------------------------------------------------------------------------------—•°
setdiff.vars.node <- function(vn, varset){
stopifnot(is.character(varset))
for(n in names(vn)){
    vn[[n]] <- setdiff(vn[[n]], varset)
}
return(vn)
}

## -------------------------------------------------------------------------------------------------—•°
union.vars.node <- function(vn, varset){
## -------------------------------------------------------------------------------------------------—•°
##  vn         vars.list
##  `varset`   is a named list of length 1, e.g. list(group=c('a', 'b')) --
##             then 'group' will be used as a name for the new entry of the `vn`;
##             it may be also name already present in `vn` and then content of the relevant element
##             will be recalculated as union of old and new values;
##             it may be also just vector of names in which case will be added to group '(new)';
##             in both cases entry 'all' is recalculated as union of old and new values;
## -------------------------------------------------------------------------------------------------—•°
if(is.list(varset)){
    ## adding new var names as new group (entry of the `vn`)
    ## varset should have length 1 !!!                                                              TODO !!!
    v_name = names(varset)[1]        ## only first element is taken
    stopifnot(is.character(varset[[v_name]]))
    if(v_name %in% names(vn)){
        vn[[v_name]] <- union(vn[[v_name]], varset[[v_name]])
    }else{
        vn[[v_name]] <- varset[[v_name]]
    }
    vn$all <- union(vn$all, vn[[v_name]])
}else if(is.character(varset)){
    vn$all <- union(vn$all, varset)
    vn[["(new)"]] <- varset
}else{
    stop("`varset` must be named list or character vector.")
}
return(vn)
}


## ---------------------------------------------------------------------------------------------------------------------—•°
## data.node

data.node <- function(datfram, only_names=TRUE, parent=NULL){
## -------------------------------------------------------------------------------------------------—•°
##  datfram             data.frame to remember (on which the model was built)
##  only_names = TRUE   if TRUE then only rownames and colnames of `datfram` will be remembered
##  parent = NULL       parent data.frame or parent data.frame name within the .GlobalEnv;
##                      'parent' means it's the data.frame in which `datfram` is contained, i.e.
##                      `datfram` is meant to be selection of rows and columns of a 'parent'
##                      without any rows/cols names changed!
##                      `datfram` is a subtable of a 'parent', e.g. obtained via subtable()
##                      see  subtable.R;
## -------------------------------------------------------------------------------------------------—•°
ll <- list(
        rownames = rownames(datfram),
        colnames = colnames(datfram),
        data = NULL,
        parent = NULL
    )

if(only_names){
    if(is.null(parent)){
        parent <- deparse(substitute(datfram))
    }
}else{
    ll$data <- datfram
}

## remembering parent's data.frame
if(!is.null(parent)){
    if(is.data.frame(parent)){
        ll$parent <- deparse(substitute(parent))
    }else if(is.character(parent)){
        ll$parent <- parent
    }else{
        stop("`parent` must be a data.frame or name of a data.frame from the .GlobalEnv.")
    }
}

structure( ll, class = c("data.node", "list") )
}

## -------------------------------------------------------------------------------------------------—•°
print.data.node <- function(dn){
indentr(dn, compact=9, omit="data")
}


## -------------------------------------------------------------------------------------------------—•°
## move to separate file ---------------------------------------------------------------------------—•°
data <- function(x, ...){ UseMethod("data") }
data.default <- function(...){ utils::data(...) }
## -------------------------------------------------------------------------------------------------—•°


## -------------------------------------------------------------------------------------------------—•°
data.data.node <- function(dn, data=NULL){
## -------------------------------------------------------------------------------------------------—•°
## dn       data.node
## data     NULL or data.frame or name of data.frame from .GlobalEnv
##
##    Remrks
## Of course one of the conditions must be satisfied to retrieve data from data.node:
## • `only_names = FALSE` — then a data.frame passed to `datfram` arg of `data.node()`
##   is remembered as a whole in the `.$data` entry;
## If `only_names = TRUE` (default) then
## • `datfram` value  (`data1` in this example) must be in present in the .GlobalEnv
## or
## • `parent` indicates the name of the parent data.frame which is present in the .GlobalEnv
##   (here it would be "data0"); notice that 'parent' is never remembered as a whole, only its name,
##   as this is potentially big data.frame of which a `datfram` is a subtable i.e. some selection
##   of rows and columns (perhaps obtained via subtable() from defined in subtable.R).
##
## It is also possible to pass a data.frame or its name as an argument to data.data.node()
## in which case the original data.frame will be recreated directly from the passed data.frame
## according to  entries `.$rownames` and `.$colnames` which are always present in data.node.

## -------------------------------------------------------------------------------------------------—•°
if(is.null(data)){
    if(!is.null(dn$data)){
        data <- dn$data
    }else if(!is.null(dn$parent)){
        data <- get(dn$parent, pos=1)[dn$rownames, dn$colnames]
    }else{
        stop("`data`, `data.node$data` and `data.node$parent` are NULL thus data.frame cannot be retrieved.")
    }
}else{
    if(is.data.frame(data)){
        data <- data[dn$rownames, dn$colnames]
    }else if(is.character(data)){
        data <- get(data, pos=1)
    }else{
        stop("`data` must be data.frame or name of a data.frame in the .GlobalEnv or left NULL.")
    }
}
return(data)
}



## ---------------------------------------------------------------------------------------------------------------------—•°
## model.node

model.node <- function(
    data = NULL,                # data.node(...)
    vars = NULL,                # vars.node(...)
    ##
    formula = character(0),     # initial
    formula0 = character(0),    # final
    model = NA,                 # initial
    model0 = NA                 # final
){
ll <- list(
        data=data,
        vars=vars,
        formula=formula,
        formula0=formula0,
        model=NULL,
        model0=NULL
    )
if(length(formula)==0){
    ll <- within( ll, {
            if(!is.null(vars)){
                formula <- paste(vars$Y, paste(vars$X, collapse=" + "), sep=" ~ ")
            }
        })
}
structure(ll, class=c("model.node", "node", "list"))
}

## -------------------------------------------------------------------------------------------------—•°
models.list <- function(){
    structure(list(), class=c("models.list", "list"))
}

## -------------------------------------------------------------------------------------------------—•°
formula.model.node <- function(mn, y="Y", x="X"){
    paste(mn$vars[[y]], paste(mn$vars[[x]], collapse=" + "), sep=" ~ ")
}
## -------------------------------------------------------------------------------------------------—•°

## -------------------------------------------------------------------------------------------------—•°
print.model.node <- function(mn){
indentr(mn, compact=9)
}

## -------------------------------------------------------------------------------------------------—•°
print.models.list <- function(ml){
indentr(ml, compact=9)
}


## -------------------------------------------------------------------------------------------------—•°
intersect.model.node <- function(mn, varset){
intersect(mn$vars, varset)
return(mn)
}

## -------------------------------------------------------------------------------------------------—•°
setdiff.model.node <- function(mn, varset){
setdiff(mn$vars, varset)
return(mn)
}

## -------------------------------------------------------------------------------------------------—•°
union.model.node <- function(mn, varset){
union(mn$vars, varset)
return(mn)
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
## RELOADER — before it works you need to source("PacksAK.R"); it's best to use {package_name}.R within pack's dir.
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

## -----------------------------------------------------------------------------—•°
## data.node()

set.seed(123)
data0 = data.frame(
    aa = sample(c(1, 2, 3, NA), 10, replace=TRUE),
    bb = sample(c('a', 'b', 'c', NA), 10, replace=TRUE),
    cc = sample(c(10, 20, 30, NA), 10, replace=TRUE),
    dd = sample(c('A', 'B', 'C', NA), 10, replace=TRUE),
    ee = sample(c('qq', NA), 10, replace=TRUE)
    )
data0

(data1 = subtable(data0, full=c("cc", "ee"), NAs=1L))   ## see  subtable.R

(dn <- data.node(data1))
data(dn)

(dn <- data.node(data1, only_names=FALSE))
(dn <- data.node(data1, FALSE))             ## the same
data(dn)

(dn <- data.node(data1, parent=data0))
rm(data1)
data(dn)
(data1 = subtable(data0, full=c("cc", "ee"), NAs=1L))   ## the same !

(dn <- data.node(data1))
df0 <- data0
df1 <- data1
rm(data0, data1)
data(dn, df1)
data(dn, df0)

## ---------------------------------------------------------------------------------------------------------------------—•°
}; rm(dummy)