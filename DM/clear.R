## ---------------------------------------------------------------------------------------------------------------------—•°
## Filtering rows and selecting columns according to number of allowed NAs and unique values.
## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {DM}
##  subtable( datafram, full=NULL, OKs=NULL, NAs=NULL, uniques=NULL, except=NULL)
##
## DEPENDENCIES
##  whileif()        DM::whileif.R
##
## REMARKS/WARNINGS
##
## TODO
## • 
##
## DESCRIPTION
##  The aim, the idea, problems, solutions... briefly!, concerning the whole set of functions
##
## -------------------------------------------------------------------------------------------------—•°
## AUTHOR: Arkadiusz Kasprzyk; akasp@...
## ver. 1.0
## PATH: ./DM/subtable.R
## start: 2020-12-02    last: 2020-12-02
## ---------------------------------------------------------------------------------------------------------------------—•°


## ---------------------------------------------------------------------------------------------------------------------—•°
clear <- function(x, ...){UseMethod("clear")}
## -------------------------------------------------------------------------------------------------—•°
## In general a method for clearing object of NA values - a default method.
## However for structured objects like data.frame this may be understood in number of ways,
## thus specific methods do more sofisticaterd things; see e.g. clear.data.frame().
## Notice that default method destroys dimensionality of x.
## -------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
clear.default <- function(x, ...){
## -------------------------------------------------------------------------------------------------—•°
## Clearing object of NA values;
## Default method destroys structure of x.
## -------------------------------------------------------------------------------------------------—•°
x[!is.na(x)]
}

## ---------------------------------------------------------------------------------------------------------------------—•°
clear.list <- function(x, length=TRUE, ...){
## -------------------------------------------------------------------------------------------------—•°
## Clearing object of NA values;
## Default method destroys structure of x.
## -------------------------------------------------------------------------------------------------—•°
cls <- class(x)
x <- x[!is.na(x)]
x <- x[!is.null(x)]
if(length){
    x <- x[sapply(x, function(x){length(x)>0})]
}
class(x) <- cls
x
}

## ---------------------------------------------------------------------------------------------------------------------—•°
clear.data.frame <- function(
    datfram,
    full = NULL,                    # 1 filter rows - only OKs for given variables
    OKs = NULL,       # at least    # 2 select columns - only
    NAs = NULL,       # at most     #    "
    uniques = NULL,   # at least
    except = NULL
){
## -------------------------------------------------------------------------------------------------—•°
## From data.frame `datfram` removing all rows (filtering) which have NA for columns mentioned
## in `full` (i.e. these columns will beacome "full" of data) and AFTER this removing all
## columns (negative selecting) having not enough non-NA values (OKs), according to parameters `OKs` or `NAs`,
## and having not enough unique-values, according to `uniques`;
## `OKs`/`NAs` and `uniques` are checked for all columns except those mentioned in `except`.
##
##    Arguments
##  datfram          data.frame
##  full = NULL      character vector of `datfram` columns which will be used to filter rows
##                   to get rid of NAs for all of these columns.
##                   if NULL or FALSE or have length 0, then no filtering is done;
##                   if "" or TRUE then filtering is done for ALL colums;
##                   columns mentioned in the `except` are not checked at all;
##  OKs = NULL       numeric; how much OKs (at least!) must have each of the remaining columns
##                   (except those in `except` which are passed unchecked
##                   and of course except `full` which have no NAs by definition
##                   and would pass all the OKs threshold);
##                   if the column has less OKs than this threshold then is removed from `datfram`
##                   AFTER filtering of rows is performed for `full` columns;
##                   if NULL then will not be considered at all and the `NAs` is checked (see below);
##                   if > 1 then rounded down to integer and interpreted as number of necessary OKs
##                   in the column;
##                   if <= 1 then treated as minimal proportion of OKs for column to be preserved;
##                   to make threshold '1 OK value' pass `1L` or `as.integer(n)` for some varying `n`;
##                   columns mentioned in the `except` are not checked at all;
##  NAs = NULL       numeric; how much NAs (at most!) a column can have to be preserved
##                   (except ... - as for `OKs`);
##                   if the column has more NAs then this threshold then will be removed from `datfram`
##                   AFTER filtering of rows is performed for `full` columns;
##                   `NAs` is cosidered only when `OKs == NULL`;
##                   if NULL then is not considered at all;
##                   columns mentioned in the `except` are not checked at all;
##  uniques = NULL   integer, threshold of number of unique values (without NA) for a variable
##                   to be left in `datfram` - it must have at least `uniques` unique values;
##                   e.g. if 0 then it's doing nothing (like for NULL default) while for 1 removes
##                   only empty variables; for 2 it removes all empty and 1-value variables; and so on;
##                   columns mentioned in the `except` are not checked at all;
##  except = NULL    which columns exclude from checking of all conditions above;
##                   these columns will be always present in the final result
##                   and also are not considered while filtering rows (according to `full`);
##                   if NULL then there are NO exceptions i.e. all columns must pass all the conditions;
##                   if "" then all columns are excluded from checking making all conditions irrelevant
##                   (nothing happens) -- this is for edge case.
##                   BTW: Notice the difference between c() and character(0):
##                   is.null(c(0)) == TRUE  while  is.null(character(0)) == FALSE  !
##                   Nevertheless both can be used and will have the same effect as NULL.
##
##    Result
## `datfram` with rows filtered in such a way that all columns from `full` have no NA values,
## and AFTER this filtering only those columns are preserved which:
## - have at least `OKs` non-NA values or (if `OKs` left NULL) have at most `NAs` NA values
## (by number or proportion -- see above),
## - have at least `uniques` unique values;
## Each of these conditions may be left NULL -- then will not be checked.
## For each condition only columns not mentioned in the `except` are checked.
##
##    Description/Comments/Remarks
## 1. `full`, `OKs`/`NAs` and `uniques` are always checked in that order;
##    if one needs to do `OKs`/`NAs` or `uniques` before `full` then one need to call
##    clear.data.frame() twice: df %>% clear(OKs=x) %>% clear(full="...").
## 2. Remember: integers are passed with `L` e.g. `9L` is integer while `9` is float.
##    This is important for `OKs=1` and `OKs=1L` -- in the first case it is interpreted as 100%
##    and in the latter as just 1 OK (non-NA) value.
## 3. clear.data.frame() operates also on the multidimensinal variables, i.e. columns of `datfram`
##    which itself are matrices as each data point consists of m values (where m is constant for
##    such a variable). In such case, if for a given record NA value appears at any
##    of the m possible entries then the whole record is treated as NA.
##
## -------------------------------------------------------------------------------------------------—•°


## ---------------------------------------------------------—•°
## returns `todo` -- names of all columns of `datfram` except those mentioned in `except`;
## columns from `todo` will be processed according to `full`, `OKs`, `NAs`, `uniques`

if(is.null(except)){           ## no exceptions -- consider all variables
    todo <- names(datfram)
    ## Notice the difference between c() and character(0) (used below):
    ##  is.null(c(0)) == TRUE  while  is.null(character(0)) == FALSE  !
}else{   
    if(is.character(except)){
        if(length(except) == 1 && except[1] == ""){   ## == except all variables i.e. nothing to do -- for edge cases;
            todo <- character(0)
        }else{
            todo <- setdiff(names(datfram), except)
        }
    }else{
        stop("`except` must be character vector with selection of `datfram`s columns or left NULL.")
    }
}
## `todo` is NEVER NULL ! is always character (even if empty).

## -------------------------------------------------------------------------------------------------—•°
## filter rows according to `full` 

full <- whileif(full, iffalse=NULL, iftrue="")

## adjusting `full` ----------------------------------------—•°
if(is.null(full)){
    full <- character(0)
}else{
    if(is.character(full)){
        if(length(full) == 1 && full == ""){
            full <- todo
        }else{
            full <- intersect(full, todo)
        }
    }else{
        stop("`full` must be character vector with selection of `datfram`s columns or left NULL.")
    }
}

## ---------------------------------------------------------—•°
## searching for proper indices in case of uni- and multi- variate columns
NAs_idx <- function(column){
    ## logical index of NA values for uni-variate (standard) and multi-variate (rare) columns
    ## for multivariate columns a row is considered to be NA if one of its elements is NA.
    variable <- datfram[[column]]
    if(is.null(dim(variable))){
        idx <- is.na(variable)
    }else{
        idx <- apply(variable, 1, function(x)as.logical(sum(is.na(x))))
    }
    return(idx)
}

## ---------------------------------------------------------—•°
## removing rows with NAs for `full` columns
if(length(full) > 0){
    for(column in full){
        datfram <- datfram[ !NAs_idx(column), ]
    }
}
if(nrow(datfram) == 0){
    return(datfram)          ## it will stop here !
}

nrows <- nrow(datfram)

## -------------------------------------------------------------------------------------------------—•°
## select columns according to `OKs` or `NAs` and `except` 
## (*) removing columns from `todo` which have too much NAs
todo_not_full <- setdiff(todo, full)
if(length(todo_not_full) > 0 && !(is.null(OKs) & is.null(NAs))){

    NAs.vec <- sapply(todo_not_full, function(column) sum(NAs_idx(column)))
    OKs.vec <- nrows - NAs.vec

    if(!is.null(OKs)){
        OKs <- ifelse(OKs > 1, as.integer(OKs), OKs)
        if(!is.integer(OKs)){
            OKs <- round(nrows * OKs)      ## at least of OK values
        }
    }else if(!is.null(NAs)){
        NAs <- ifelse(NAs > 1, as.integer(NAs), NAs)
        if(!is.integer(NAs)){
            NAs <- round(nrows * NAs)      ## at most of NAs values
        }
        OKs <- nrows - NAs
    }

    to_remove <- names(OKs.vec[OKs.vec < OKs])

    datfram <- datfram[setdiff(names(datfram), to_remove)]

    todo <- setdiff(todo, to_remove)
}


## -------------------------------------------------------------------------------------------------—•°
## uniques 
if(length(todo) > 0 && !is.null(uniques)){

    count_uniques <- function(column){
        variable <- datfram[[column]]
        if(is.null(dim(variable))){
            lgth <- length(unique(variable[ !is.na(variable) ]))
        }else{
            idx <- apply(variable, 1, function(x)as.logical(sum(is.na(x))))  #!#! ??? see NAs_idx()
            lgth <- nrow(unique(variable[ !idx , ]))
        }
        return(lgth)
    }

    uniques <- ifelse(uniques > 1, as.integer(uniques), uniques)
    if(!is.integer(uniques)){
        uniques <- round(nrows * uniques)      ## at least of `uniques` values
    }

    mask <- sapply(names(datfram), is.character)    ## shortcut trick
    uniques.vec <- sapply(todo, function(column){ count_uniques(column) >= uniques})
    mask[names(uniques.vec)] <- uniques.vec
    datfram <- datfram[mask]

}

## -------------------------------------------------------------------------------------------------—•°
return(datfram)

}  ##----END----##
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
## vector
clear(c(NA, 1, NA, 2))

## -----------------------------------------------------------------------------—•°
## matrix
clear(matrix(c(NA, 2, 3, 4, NA, NA), 2, 3))
## the structure destroyed


## -----------------------------------------------------------------------------—•°
## data.frame
set.seed(123)
datfram = data.frame(
    aa = sample(c(1, 2, NA), 10, replace=TRUE),
    bb = sample(c('a', 'b', 'c', NA), 10, replace=TRUE),
    cc = sample(c(10, 20, 30, NA), 10, replace=TRUE),
    dd = sample(c('A', 'B', 'C', NA), 10, replace=TRUE),
    ee = sample(c('qq', NA), 10, replace=TRUE)
    )
datfram

## filtering rows to obtain data.frame with selected columns full (without NAs)
clear(datfram)
clear(datfram, full="")       ## "" means we want all columns to be full, i.e. remove all records
                                 ## with any NA; only one record full
clear(datfram, full=TRUE)     ## the same

datfram$ff <- rep(NA, 10)
datfram
clear(datfram, full="")       ## empty data.frame
clear(datfram, full=TRUE)
clear(datfram, full="", except=c("aa", "ff"))  ## we need all columns full wxcept "aa" and "ff"


clear(datfram, full=c("aa", "bb"))   ## we want only "aa" and "bb" to be full

## selecting columns according to nr of NAs/OKs
clear(datfram, OKs=.7)               ## remove all columns which have LESS then 70% of OK values
clear(datfram, OKs=8, except="bb")   ## remove all columns which have LESS then 8 OK values
                                        ## but do not check "bb" (leave it).

clear(datfram, NAs=.3)               ## remove all columns which have MORE then 30% of NA values
clear(datfram, NAs=2, except="bb")   ## remove all columns which have MORE then 2 NA values
                                        ## but do not check "bb" (leave it).
## i.e.  `NAs` works like "at most"
## while `OKs`      "     "at least"

## selecting columns according to nr of unique values
clear(datfram)
clear(datfram, uniques=0)     ## at least 0 unique values, i.e. even empty variable passes
## !!!
clear(datfram, uniques=1)     ## 1 is interpreted as 100% i.e. all values different (no ties)
## to get 1 in a sense of "at least 1 unique value" use `L` notation to force integer
clear(datfram, uniques=1L)     ## at least 1 unique value, i.e. only empty variables are removed

clear(datfram, uniques=2)     ## at least 2 unique values, i.e. empty and 1 value variables are removed
clear(datfram, uniques=3)
clear(datfram, uniques=3, except=c("aa", "bb"))  ## do not check "aa" and "bb"
clear(datfram, uniques=3, except="")    ## except all columns i.e. check nothing -- just edge case :)

## -----------------------------------------------------------------------------—•°
##    Mixed examples
## The basic rule is that:
## checking (and applying) `full` condition precedes `OKs` and `NAs` which precedes `unique`;
## If one wants to apply first `uniques` then `OKs` and already then `full`
## then clear.data.frame() must be called separately for each condition:
##  df %>% clear(uniques=n) %>% clear(OKs=m) %>% clear(full=x)
## When calling
##  clear(df, uniques=n, OKs=m, full=x)
## `full` will be always applied first then `OKs` then `uniques` at the end.
## Also of the two `OKs` & `NAs` only one is used with precedence for `OKs` i.e. `OKs` overrides `NAs`.

## -----------------------------------------------------------------------------—•°
##    Multidimensional variables
set.seed(123)
df1 <- data.frame(
    yy = ((1:30) + sample(0:10, 30, replace=T)),
    xx = 1:30
    )
df1
plot(yy ~ xx, data=df1)

(Yb <- success_failure_matrix(df1$yy))

df1$xx[25:30] <- NA
df1
clear(df1, full=T)

df2 <- df1
df2$yb <- Yb
df2
dim(df2)
names(df2)
df2$yb
dim(df2["yb"])
dim(df2[["yb"]])
dim(df2["xx"])
dim(df2[["xx"]])

is.na(df2[["xx"]])

clear(df2, full=T)
column <- "yb"
is.na(df2[column])
df2[!is.na(df2[column]),]  #! Error in xj[i, , drop = FALSE] : (subscript) logical subscript too long

df2[column][3,]

df2[[column]][t(rbind(c(1, 3, 5, 7), c(1, 2, 1, 2)))] <- NA
df2

which(is.na(df2[[column]]), arr.ind=T)

apply(df2[[column]], 1, function(x)as.logical(sum(is.na(x))))
clear(df2, full=T)

clear(df2, NAs=1L)

length(unique(df2$yb))

clear(df2, NAs=4)
clear(df2, NAs=3)

df2$zz <- cbind(p=sample(1:3, 30, replace=T), q=sample(1:3, 30, replace=T))
df2
unique(df2$zz)
clear(df2, uniques=9)
clear(df2, uniques=10)

## ---------------------------------------------------------------------------------------------------------------------—•°
}; rm(dummy)
