## ---------------------------------------------------------------------------------------------------------------------—•°
## For use with  model.node(); see  model.node.R
## ---------------------------------------------------------------------------------------------------------------------—•°

recreate_models_data <- function(x, ...){UseMethod("recreate_models_data")}

## ---------------------------------------------------------------------------------------------------------------------—•°

recreate_models_data.gam <- function(mod, data=NULL){
## -------------------------------------------------------------------------------------------------—•°
## "gam" is a result of mgcv::gam()
## -------------------------------------------------------------------------------------------------—•°

## -------------------------------------------------------------------------------------------------—•°
## common to both gam:: & mgcv::
if(is.null(data)) stop("`data` cannot be left NULL - MUST be provided as a data.frame or it's name.")

if(is.character(data)){
    data <- get(data, .GlobalEnv)
}else{
    if(!is.data.frame(data)) stop("`data` MUST be provided as a data.frame or it's name.")
}

Yname <- as.character(mod$formula)[2]

if(mod$family$family=="binomial"){
    ## which name is the longest match for Yname -- each name serves here as pattern !!!
    ## we assume that  Yname = one of the elements of names(data) + suffix
    Yname0 <- match_longest_pattern(names(data), Yname)
    if(length(Yname0)==0){
        stop("There is no target variable in data provided (must be the name from `mod$formula` perhaps minus some prefix.)")
    }
    if(is.null(dim(data[[Yname0]]))){
        data[[Yname]] <- success_failure_matrix(data[[Yname0]])
    }
}else{
    if(! Yname %in% names(data)){
        stop("There is no target variable in data provided (must have the same name as in `mod$formula`.)")
    }
}
## -------------------------------------------------------------------------------------------------—•°

mod$model <- data[names(attr(mod$terms, "dataClasses"))]      ## strange but true
## in mgcv:: there is only mod$model entry where all necessary data are without s() in names
## it's a different story for gam::

return(mod)
}

## ---------------------------------------------------------------------------------------------------------------------—•°

recreate_models_data.Gam <- function(mod, data=NULL){
## -------------------------------------------------------------------------------------------------—•°
## "Gam" is a result of gam::gam()
## TODO : restore the original order of variables in the final data frames .$data .$model .$smooth.frame
## -------------------------------------------------------------------------------------------------—•°

## -------------------------------------------------------------------------------------------------—•°
## common to both gam:: & mgcv::
if(is.null(data)) stop("`data` cannot be left NULL - MUST be provided as a data.frame or it's name.")

if(is.character(data)){
    data <- get(data, .GlobalEnv)
}else{
    if(!is.data.frame(data)) stop("`data` MUST be provided as a data.frame or it's name.")
}

Yname <- as.character(mod$formula)[2]

if(mod$family$family=="binomial"){
    ## which name is the longest match for Yname -- each name serves here as pattern !!!
    ## we assume that  Yname = one of the elements of names(data) + suffix
    Yname0 <- match_longest_pattern(names(data), Yname)
    if(length(Yname0)==0){
        stop("There is no target variable in data provided (must be the name from `mod$formula` perhaps minus some prefix.)")
    }
    if(is.null(dim(data[[Yname0]]))){
        data[[Yname]] <- success_failure_matrix(data[[Yname0]])
    }
}else{
    if(! Yname %in% names(data)){
        stop("There is no target variable in data provided (must have the same name as in `mod$formula`.)")
    }
}
## -------------------------------------------------------------------------------------------------—•°

 # data <- data(MODEL$Powder$SENS_ADHESIVE_TX)

pure_names <- function(s.names){         ## how to generalize it?   is it worth it?
    s.names.without.s <- gsub("s\\(", "", s.names)
    s.names.without.s <- regmatches(s.names.without.s, regexpr("^[[:alnum:]_]+", s.names.without.s))
    s.names.without.s
}

mod$data <- data[pure_names(names(attr(mod$terms, "dataClasses")))]    ## "raw" data with proper form/name of target
## for gam::  attr(mod$terms, "dataClasses") has s() in names   (unlike for mgcv::)
mod$model <- mod$data

all.vars <- names(mod$model)
names(all.vars) <- all.vars

s.vars <- colnames(mod$smooth)
names(s.vars) = pure_names(s.vars)

all.vars[names(s.vars)] <- s.vars
names(mod$model) <- all.vars

mod$smooth.frame <- mod$model[s.vars]

return(mod)
}

## ---------------------------------------------------------------------------------------------------------------------—•°
