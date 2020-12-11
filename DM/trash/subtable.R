## ---------------------------------------------------------------------------------------------------------------------—•°
## TITLE (descriptive).
## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {DM}
##  subtable( datafram, full=NULL, OKs=NULL, NAs=NULL, except=NULL)
##  rm.onevals( datafram, except=NULL )
##
## DEPENDENCIES
##  funa()        {package} file.R     [defalt {package} is the same; default file.R is the same as function name]
##  funb()
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
subtable <- function(
    datfram,
    full = NULL,                    # 1 filter rows - only OKs for given variables
    OKs = NULL,       # at least    # 2 select columns - only
    NAs = NULL,       # at most     #    "
    except = NULL
){
## -------------------------------------------------------------------------------------------------—•°
## From data.frame `datfram` removing all rows (filtering) which have NA for columns mentioned
## in `full` (i.e. these columns will beacome "full" of data) and AFTER this removing all
## columns (negative selecting) having not enough non-NA values (OKs) according to parameters `OKs` or `NAs`
## but except those columns mentioned in `except`.
##
##    Arguments
##  datfram          data.frame
##  full = NULL      character vector of `datfram` columns which will be used to filter rows
##                   to get rid of NAs for all of these columns.
##                   if NULL then no filtering is done;
##                   if "" then filtering is done for ALL colums;
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
##  NAs = NULL       numeric; how much NAs (at most!) a column can have to be preserved
##                   (except ... - as for `OKs`);
##                   if the column has more NAs then this threshold then will be removed from `datfram`
##                   AFTER filtering of rows is performed for `full` columns;
##                   `NAs` is cosidered only when `OKs == NULL`;
##                   if NULL then is not considered at all;
##  except = NULL    which columns exclude from `OKs`/`NAs` thresholds checking described above;
##                   these columns will be always present in the final result;
##                   if NULL then there are NO exceptions i.e. all columns must pass the `OKs`/`NAs` threshold;
##                   if "" then all columns are excluded from checking making `OKs`/`NAs` irrelevant
##                   (nothing happens).
##
##    Result
##  `datfram` with rows filtered in such a way that all columns from `full` have no NA values,
##  and AFTER this filtering only those columns are preserved which are mentioned in `except`
##  or have at least `OKs` non-NA values or (if `OKs` left NULL) have at most `NAs` NA values
##  (by number or proportion -- see above).
##  If both `OKs` and `NAs` are left NULL (default) then only filtering according to `full` is performed.
##
##    Description/Comments/Remarks
## Above.
##    Remember: integers are passed with `L` e.g. `9L` is integer while `9` is float.
##
## -------------------------------------------------------------------------------------------------—•°

## filter rows according to `full` -----------------------------------------------------------------—•°

## adjusting `full` ----------------------------------------—•°
if(!is.null(full)){
    if(is.character(full)){
        if(length(full) == 1 && full == ""){
            full <- names(datfram)
        }else{
            full <- intersect(full, names(datfram))
        }
    }else{
        stop("`full` must be character vector with selection of `datfram`s columns or left NULL.")
    }
}else{
    full <- c()
}

## removing rows with NAs for `full` columns ---------------—•°
if(length(full) > 0){
    for(column in full){
        datfram <- datfram[ !is.na(datfram[column]), ]
    }
}


if(nrow(datfram) == 0){
    return(datfram)          #??? will stop here ?
    # break
}

## select columns according to `OKs` or `NAs` and `except` -----------------------------------------—•°

## ---------------------------------------------------------—•°
## adjusting `todo` accordinng to `full` and `accept`
## -- each of these columns will be checked for amount of NAs
## -- if too much wrt to `OKs` or `NAs`
## then such a column will be removed from `datfram`

if(!is.null(except)){
    if(is.character(except)){
        if(length(except) == 1 && except == ""){   ## == except all variables i.e. nothing to do more, see (*) -- just for consistence with `full`
            todo <- c()
        }else{
            todo <- setdiff(names(datfram), union(except, full))
        }
    }else{
        stop("`except` must be character vector with selection of `datfram`s columns or left NULL.")
    }
}else{   ## no exceptions -- consider all variables besides `full` (which are full and will satisfy all threshold)
    todo <- setdiff(names(datfram), full)
}

## ---------------------------------------------------------—•°
## (*) removing columns from `todo` which have too much NAs
if(length(todo) > 0 && !(is.null(OKs) & is.null(NAs))){

    nrows <- nrow(datfram)
    OKs.vec <- sapply(names(datfram), function(n) sum(!is.na(datfram[n])))
    NAs.vec <- nrows - OKs.vec

    if(!is.null(OKs)){
        OKs <- ifelse(OKs > 1, as.integer(OKs), OKs)
        if(!is.integer(OKs)){
            OKs <- round(nrows * OKs)      ## at least of OK values
        }
    }else if(!is.null(NAs)){
        NAs <- ifelse(NAs > 1, as.integer(NAs), NAs)
        if(!is.integer(NAs)){
            NAs <- round(nrows * NAs)
        }
        OKs <- nrows - NAs
    }

    OKs.vec <- OKs.vec[todo]
    to_remove <- names(OKs.vec[OKs.vec < OKs])


    datfram <- datfram[setdiff(names(datfram), to_remove)]
}

## -------------------------------------------------------------------------------------------------—•°

return(datfram)

}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
rm.onevals <- function(
    datfram,
    uniques = 1,
    except = NULL
){
## -------------------------------------------------------------------------------------------------—•°
## Removes from `datfram` all variables with nr of unique values less then or equal to `uniques`
## which by default is 1 (hence name as this is most important case).
##
##    Arguments
##  datfram       data.frame
##  uniques = 1   integer, threshold of number of unique values (without NA) for a variable
##                to be left in `datfram` - it must have MORE unique values than this threshold.
##  except        which variables do not check; if NULL (default) all will be checked;
##                if "" then all variables are excluded from checking i.e. nothing happens.
##
##    Remarks
## Notice that it also removes empty variables i.e. without any values i.e. having onbly
## -------------------------------------------------------------------------------------------------—•°
uniques <- sapply(datfram, function(x){length(unique(x[ !is.na(x) ])) > uniques})
if(!is.null(except)){
    if(except[1] == ""){
        except <- names(datfram)
    }
    uniques[except] <- TRUE
}
datfram <- datfram[uniques]
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
## RELOADER — before it works you need to source("PacksAK.R"); it's best to use {package_name}.R within pack's dir.
 loadPacksAK("DM")
## -------------------------------------------------------------------------------------------------—•°

set.seed(123)
datfram = data.frame(
    aa = sample(c(1, 2, 3, NA), 10, replace=TRUE),
    bb = sample(c('a', 'b', 'c', NA), 10, replace=TRUE),
    cc = sample(c(10, 20, 30, NA), 10, replace=TRUE),
    dd = sample(c('A', 'B', 'C', NA), 10, replace=TRUE),
    ee = sample(c('qq', NA), 10, replace=TRUE)
    )
datfram

subtable(datfram)
subtable(datfram, full="")
subtable(datfram, full=c("aa", "bb"))

subtable(datfram, OKs=.7)
subtable(datfram, OKs=8, except="bb")

rm.onevals(datfram)
rm.onevals(datfram, uniques=2)
rm.onevals(datfram, uniques=2, except=c("aa", "bb"))

## ---------------------------------------------------------------------------------------------------------------------—•°
}; rm(dummy)
