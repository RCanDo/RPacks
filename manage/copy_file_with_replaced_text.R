## ---------------------------------------------------------------------------------------------------------------------—•°
## copy_file_with_replaced_text
## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {manage}
##  copy_file_with_replaced_text(in.file, out.file=FALSE, replace=list(c("", "")))
##
## DEPENDENCIES
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

copy_file_with_replaced_text <- function(in.file, out.file=FALSE, replace=list(c("", ""))){
## -------------------------------------------------------------------------------------------------—•°
##    Arguments
##  in.file             input .R file;
##  out.file = FALSE    output .R file; if FALSE then stdout() i.e. screen;
##  replace = list(c(find="", replace=""))  list of replacements which will be applied to each line
##                      sequentially
##
##    Result
##  TRUE
##
##    Description/Comments/Remarks
## Files are written via cat(), and `out.file = FALSE` is equivalent to cat()'s' `file = ""`
## i.e.  std.out() == screen.
## -------------------------------------------------------------------------------------------------—•°
if(is.logical(out.file)){
    if(out.file){
        out.file <- in.file
    }else{
        out.file <- ""    # i.e.  std.out() == screen
    }
}

file.text = readLines(in.file)

for(n in 1:length(file.text)){
    for(ll in replace){
        if(ll[1] != ""){
            file.text[n] <- gsub(ll[1], ll[2], file.text[n])
        }
    }
}

cat(file.text, sep="\n", file=out.file)

cat(in.file, "  written to  ", out.file, "\n")

return(TRUE)
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
for(ll in list(c('a','b'), 1:3, list(c("v", "d"), 2:4))){
    print(ll); print(class(ll))
}
## ----------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
}; rm(dummy)
