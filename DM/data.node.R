## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {DM}
##  data.node()
##  print.data.node()
##  data.data.node()
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

data.node <- function(datfram, only_names=TRUE, parent=NULL){
## -------------------------------------------------------------------------------------------------—•°
##  datfram             data.frame to remember (on which the model was built)
##  only_names = TRUE   if TRUE then only rownames and colnames of `datfram` will be remembered
##  parent = NULL       parent data.frame or parent data.frame name within the .GlobalEnv;
##                      'parent' means it's the data.frame in which `datfram` is contained, i.e.
##                      `datfram` is meant to be selection of rows and columns of a 'parent'
##                      without any rows/cols names changed!
##                      `datfram` is a subtable of a 'parent', e.g. obtained via clear()
##                      see  clear.R;
##
##  TODO: datfram=NULL, rownames=c(...), colnames=c(...), parent=?
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
print.data.node <- function(x){
indentr(x, compact=9, omit="data")
}


## -------------------------------------------------------------------------------------------------—•°
data.data.node <- function(x, data=NULL){
## -------------------------------------------------------------------------------------------------—•°
## x        data.node
## data     NULL or data.frame or name of data.frame from .GlobalEnv
##
##    Remrks
## Of course one of the conditions must be satisfied to retrieve data from data.node:
## • `only_names = FALSE` — then a data.frame passed to `datfram` arg of `data.node()`
##   is remembered as a whole in the `.$data` entry;
## If `only_names = TRUE` (default) then
## • `datfram` value  (`data1` in this example) must be present in the .GlobalEnv
## or
## • `parent` indicates the name of the parent data.frame which is present in the .GlobalEnv
##   (here it would be "data0"); notice that 'parent' is never remembered as a whole, only its name,
##   as this is potentially big data.frame of which a `datfram` is a subtable i.e. some selection
##   of rows and columns (perhaps obtained via clear() defined in clear.R).
##
## It is also possible to pass a data.frame or its name as an argument to data.data.node()
## in which case the original data.frame will be recreated directly from the passed data.frame
## according to  entries `.$rownames` and `.$colnames` which are always present in data.node.

## -------------------------------------------------------------------------------------------------—•°
if(is.null(data)){
    if(!is.null(x$data)){
        data <- x$data
    }else if(!is.null(x$parent)){
        data <- get(x$parent, pos=1)[x$rownames, x$colnames]
    }else{
        stop("`data`, `data.node$data` and `data.node$parent` are NULL thus data.frame cannot be retrieved.")
    }
}else{
    if(is.data.frame(data)){
        data <- data[x$rownames, x$colnames]
    }else if(is.character(data)){
        data <- get(data, pos=1)[x$rownames, x$colnames]
    }else{
        stop("`data` must be data.frame or name of a data.frame in the .GlobalEnv or left NULL.")
    }
}
return(data)
}  ##----END----##

## ---------------------------------------------------------------------------------------------------------------------—•°

`[.data.node` <- function(x, value, drop=TRUE){
    if(!is.null(x$data)){
        res <- x$data[value, drop=drop]
    }else if(!is.null(x$parent)){
        if(all(value %in% x$colnames)){
            res <- get(x$parent, pos=1)[x$rownames, value, drop=drop]
        }else{
            v0s <- value[! value %in% x$colnames]
            v0s <- paste(paste0("\"", v0s, "\""), collapse=", ")
            stop( paste0(v0s," are not present in this data.node$colnames."))
        }
    }else{
        stop("`data.node$data` and `data.node$parent` are NULL thus data.frame cannot be retrieved.")
    }
    res
}

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
data(cars)   ## works as before, OK!


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

(data1 = clear(data0, full=c("cc", "ee"), NAs=1L))   ## see  clear.R

(dn <- data.node(data1))
data(dn)

(dn <- data.node(data1, only_names=FALSE))
(dn <- data.node(data1, FALSE))             ## the same
data(dn)

(dn <- data.node(data1, parent=data0))
rm(data1)
data(dn)
(data1 = clear(data0, full=c("cc", "ee"), NAs=1L))   ## the same !

(dn <- data.node(data1))
df0 <- data0
df1 <- data1
rm(data0, data1)
data(dn, df1)
data(dn, df0)

## ---------------------------------------------------------------------------------------------------------------------—•°
}; rm(dummy)