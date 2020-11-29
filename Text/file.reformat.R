## ---------------------------------------------------------------------------------------------------------------------—•°
## TITLE (descriptive).
## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {package}
##  fun1( x , y , ... )
##  fun2( ... )
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
## PATH: D:\ROBOCZY\...
## start: 2020-mm-dd    last: 2020-mm-dd
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
old2new.breaklines <- function(in.file, out.file=FALSE){
## ---------------------------------------------------------------------------------------------------------------------—•°
## Changes line brek in .R file (by purpose) from
## "####{n}" to "## -{n}" e.g. from "##########" to "## -------"
## what is currently in common use e.g. RStudio uses the latter to create editor's 'foldings'.
##    Arguments
##  in.file             input .R file;
##  out.file = FALSE    output .R file; if FALSE then stdout() i.e. screen;
##                      
##
##    Result
##  NULL
##
##    Description/Comments/Remarks
## Files are written via cat(), and `out.file = FALSE` is equivalent to cat()'s' `file = ""`.
## ---------------------------------------------------------------------------------------------------------------------—•°
if(is.logical(out.file)){
    if(out.file){
        out.file <- in.file
    }else{
        out.file <- ""    # i.e.  std.out() == screen
    }
}

file.text = readLines(in.file)

for(n in grep('^###', file.text)){
    ln <- file.text[n]
    ln <- gsub("^###", "", ln)
    ln <- gsub("#", "-", ln)
    ln <- paste("##", ln)
    file.text[n] <- ln
}

cat(file.text, sep="\n", file=out.file)

cat(in.file, "  written to  ", out.file, "\n")

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
 loadPacksAK("Text")  # not registered !
## -------------------------------------------------------------------------------------------------—•°

setwd("C:/Users/arkadiusz.kasprzyk/Projects/R/RCanDo/")
getwd()

dir()
dirs = list.dirs(recursive=FALSE)
dirs

## ----------------------------------------------------------------------------—•°
## one use function for correcting all breaklines in the whole RCanDo packs collection
recursive_breaklines_correction <- function(start="."){

    lf = list.files(start)

    rfiles = grep("\\.r$", lf, ignore.case=TRUE, value=TRUE)
    for(f in rfiles){
        old2new.breaklines(file.path(start, f), TRUE)
        #!!! we OVERWRITE files - so it's done in the new Git's topic branch !!!
    }

    subdirs = Filter(function(f){file.info(f)$isdir}, lf)

    for(d in subdirs){
        recursive_breaklines_correction(file.path(start, d))
    }
}

## ----------------------------------------------------------------------------—•°
recursive_breaklines_correction(".")



## ----------------------------------------------------------------------------—•°




## ---------------------------------------------------------------------------------------------------------------------—•°
}; rm(dummy)
