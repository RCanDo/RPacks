## ---------------------------------------------------------------------------------------------------------------------—•°
## model.node and model.list
## -- for storing lots of similar models built on the subsets of the same data
## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {DM}
##  model.node()
##  summary.model.node()
##  model.list()
##  print.model.node()
##  print.model.list()
##  intersect.model.node()
##  union.model.node()
##  setdif.model.node()
##  formula.model.node()
##  data.model.node()
##  model.model.node()
##  `model<-.model.node`()
##
## DEPENDENCIES
##  whileif()
##  indentr()
##
## REMARKS/WARNINGS
##
## TODO
## •
##
## DESCRIPTION
##
## -------------------------------------------------------------------------------------------------—•°
## AUTHOR: Arkadiusz Kasprzyk; rcando@int.pl
## ---------------------------------------------------------------------------------------------------------------------—•°


## ---------------------------------------------------------------------------------------------------------------------—•°

model.node <- function(
    data,                # data.node(...)
    vars,                # vars.node(...)
    ##
    #formula = character(0),     # initial
    #formula0 = character(0),    # final
    model = NULL,                 # initial
    model0 = NULL                 # final
){
## -------------------------------------------------------------------------------------------------—•°
##    Remarks
## See help on data.node() how it's works.
## `data` entry is common for all models within `model.node` - this is very important as:
## - `data` are in general larger then needed for each separate model,
## - but we can remove data entry of each separate model and use the common `data` entry
##   of `model.node` so:
## - it saves space!
## - moreover, the `data` entry usually does not store the whole data too (although it can do this),
##   only rownames and colnames of a 'parent' data.frame which resides in .GlobalEnv
##   and by design is common to all `model.nodes` of the whole `model.list`;
## - this may massively spare memory for large `model.list`.
## It could be said that the `data` entry indicating the same 'parent' is the one thing 
## around which the whole `model.list` should be built.
## Similarily the entity pivotal for the `model.node` should be a target variable for which
## number of models (stored in this single `model.node`) is built based on the data from `data` entry.
##
##    TODO
## This class is under constructinon - some things are to be reconsidered.
## -------------------------------------------------------------------------------------------------—•°

if(is.null(data) || is.null(vars)){
    stop("`data` and `vars` must be provided.")
}

if(! "data.node" %in% class(data)){
    stop("`data` must inherit from 'data.node'")
}
if(! "vars.node" %in% class(vars)){
    stop("`vars` must inherit from 'vars.node'")
}

vars <- intersect(vars, data$colnames)

ll <- list(
        data=data,
        vars=vars
    )
#if(length(formula)==0){
#    ll <- within( ll, {
#            if(!is.null(vars)){
#                formula <- paste(vars$Y, paste(vars$X, collapse=" + "), sep=" ~ ")
#            }
#        })
#}
ll <- structure(ll, class=c("model.node", "node", "list"))

if(!is.null(model)){
    model(ll) <- model
}
if(!is.null(model0)){
    model(ll) <- model0
}

return(ll)

}

## -------------------------------------------------------------------------------------------------—•°
summary.model.node <- function(x, ...){
## this is improper! pr
for(m in names(x)){
    if(length(intersect(class(x[[m]]), getOption("class.groups")$model)) > 0){
        cat(m, "\n")
        indentr(x[[m]])
    }
}
}

## -------------------------------------------------------------------------------------------------—•°
model.list <- function(){
    structure(list(), class=c("model.list", "list"))
}

## -------------------------------------------------------------------------------------------------—•°
print.model.node <- function(x){
indentr(x, omit_class=getOption("class.groups")$model)
}

## -------------------------------------------------------------------------------------------------—•°
print.model.list <- function(x){
indentr(x, omit_class="model.node")
}


## -------------------------------------------------------------------------------------------------—•°
intersect.model.node <- function(x, varset){
intersect(x$vars, varset)
return(x)
}

## -------------------------------------------------------------------------------------------------—•°
setdiff.model.node <- function(x, varset){
setdiff(x$vars, varset)
return(x)
}

## -------------------------------------------------------------------------------------------------—•°
union.model.node <- function(x, varset){
union(x$vars, varset)
return(x)
}

## -------------------------------------------------------------------------------------------------—•°
formula.model.node <- function(mn, Y="Y", X="X", bin=NULL, s=FALSE, package="mgcv"){
## -------------------------------------------------------------------------------------------------—•°
##  bin
##           if character - ...
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
    if(! bin %in% mn$data$colnames){
        stop(paste0("there is no '", bin, "' in data."))
    }
    Yname <- paste0(bin, "_bin")
}

if(is.logical(bin)){
    if(bin){
        Yname <- paste0(mn$vars[[Y]], "_bin")
    }else{
        Yname <- mn$vars[[Y]]
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
        svars <- paste0("s(", mn$vars[[X]], deg_sign, s, ")")
    }else{
        svars <- paste0("s(", mn$vars[[X]], ")")
    }
    form <- formula(paste(Yname, paste(svars, collapse=" + "), sep=" ~ "))
}else{
    form <- formula(paste(Yname, paste(mn$vars[[X]], collapse=" + "), sep=" ~ "))
}

form

}  ##----END----##

## -------------------------------------------------------------------------------------------------—•°
data.model.node <- function(mn, Y="Y", X="X", bin=NULL, crop=TRUE, data=NULL, oryginal_Y=TRUE, ...){
## -------------------------------------------------------------------------------------------------—•°
## mn           model.node
## Y="Y"
## X="X"
## bin=NULL     if character - name of the variable to transform it into its binomial version
##              for gam(family=binomial);
##              "_bin" suffix will be added automatically to the name of transformed version;
##              if NULL  then is transformed to TRUE or FALSE depending on Y="Ybin" or not;
##              if TRUE  then variable indicated in `Y` is transformed into its binomial form ("_bin" suffix added);
##              if FALSE then `Y` will not be transformed and used as it is;
## crop=TRUE   leaves only Y/Ybin and Xs variables (according to x$vars$Y and x$vars$X)
## data=NULL   as in data.data.node()
## oryginal_Y=TRUE  logical; relevant when crop=TRUE; does include original Y
##             (i.e. variable indicated in `mn$vars$Y`) into final data?
##             usually it's not a big burdern to leave this variable even if not finally used;
## ...    passed to  success_failure_matrix()  for creating binomial form of target (when relevant)
##
##   Remarks
## The assumption is that model$vars are all within model$data$colnames;  see  data.node().
## `bin` is left NULL to automatically be set to TRUE or FALSE - it might be said that `bin` setup
## prevails over `Y` if not left NULL.
## -------------------------------------------------------------------------------------------------—•°

data <- data(mn$data, data)

## ---------------------------------------------------------—•°
## the same rules/structure as in formula.model.node() but
## here we 'generate' the data (binomial version if needed) not the formula

if(is.null(bin)){
    bin <- Y=="Ybin"
}

if(is.character(bin)){
    ## `bin` is proper variable name
    if(! bin %in% names(data)){
        stop(paste0("there is no '", bin, "' in data."))
    }
    Yname <- paste0(bin, "_bin")
    data[[Yname]] <- success_failure_matrix(data[[bin]], ...)
}

if(is.logical(bin)){
    if(bin){
        Yname <- paste0(mn$vars[[Y]], "_bin")
        data[[Yname]] <- success_failure_matrix(data[[ mn$vars[[Y]] ]], ...)
    }else{
        Yname <- mn$vars[[Y]]
    }
}

## ---------------------------------------------------------—•°
if(crop){
    if( (Yname != mn$vars$Y) && oryginal_Y ){
        data <- data[c(Yname, mn$vars$Y, mn$vars[[X]])]
    }else{
        data <- data[c(Yname, mn$vars[[X]])]
    }
}

return(data)

}  ##----END----##


## -------------------------------------------------------------------------------------------------—•°
`model<-.model.node` <- function(mn, value=list("model"=NULL)){
## -------------------------------------------------------------------------------------------------—•°
## Please use `value=model` or value `list("name"=model)`
## as in case of `value=list(model)` the name is set up automatically from class of the `model`.
## -------------------------------------------------------------------------------------------------—•°

if(is.vector(value)){      ## we need plain "list" (with first entry being "model")
    name <- names(value)[1]
    model <- value[[1]]
    if(is.null(name)){
        name <- paste(class(model), collapse="-")
    }
}else{
    name <- deparse(substitute(value))
    model <- value
}

stopifnot(inherits(model, getOption("class.groups")$model))

if(length(setdiff(rownames(model$data), mn$data$rownames))>0){
    stop("data on which the model was built have different rows then that of this `model.node`.")
}

cols <- mn$data$colnames
if(!is.null(mn$vars$Ybin)){                                              ## it's sloppy!   TODO   !!!
    cols <- union(mn$data$colnames, paste0(mn$vars$Ybin, "_bin"))
}
if(length(setdiff(colnames(model$data), cols))>0){
    stop("data on which the model was built have different columns then that of this `model.node`.")
}

for(item in c("model", "data", "smooth.frame")){
    if(!is.null(model[[item]])){
        model[[item]] <- NULL
    }
}

mn[[name]] <- model
mn

}  ##----END----##

## -------------------------------------------------------------------------------------------------—•°
model.model.node <- function(mn, name=NULL, which=NULL, ...){
## `name` and `which` are synonyms with precedence for `name`
name <- whileif(name, ifnull=which)
if(is.null(name)){
    if("model0" %in% names(mn)){
        name <- "model0"
    }else{
        name <- "model"
    }
}
model <- mn[[name]]
# model$model <- data(mn)  ## crop it or not ???  cropping here is not that simple and obvious!
model <- recreate_models_data(model, data(mn))
model
}  ##----END----##

## -------------------------------------------------------------------------------------------------—•°


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