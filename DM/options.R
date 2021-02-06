## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {DM}
##  No functions here.
##  It is setup of options:
##    class.groups
##    class.attributes
##
## DEPENDENCIES
##
## TODO
## -
##
## ---------------------------------------------------------------------------------------------------------------------—•°

options(
    class.groups = list(
        language = c("function", "call", "formula", "promise", "language", "{",
                     "quosure"),   ## rlang
        implicite = c("integer", "character", "numeric", "double", "complex", "logical", "matrix", "array"),
        common = c("table", "data.frame", "factor",
                   "grouped_df", "tbl_df", "tbl"),    ## dplyr
        model = c("lm", "glm", "gam", "tree", "rpart")
        )
)

options(
    class.attributes = list(
        basic = c("names", "dim", "dimnames", "colnames", "rownames", "row.names", "class", "levels",
                  "groups"),        ## dplyr -- it's really bad!  shouldn't be here !!!
                                    ## see also attributes.R  unattr() comment at line ~44
                                    ##      and indentr.R comment at `attrs.mustbe` line ~222
        tm =  c("transpose", "margins", "head", "round"),
        compact = "compact"
        )
)

options(messages = TRUE)    ## this options works only for "tm" class and some methods defined in packages from PacksAK
## ---------------------------------------------------------------------------------------------------------------------—•°

#print.list <- function(x,...){ indentr(x,...) }

## ---------------------------------------------------------------------------------------------------------------------—•°
dummy <- function(){
   getOption("class.attributes")
   options("class.attributes")
   getOption("class.attributes")$tm
   options("class.attributes")[[1]]$tm
   getOption("class.attributes")$basic

   getOption("class.groups")
   getOption("class.groups")$language

   getOption("messages")
}
rm(dummy)
## ---------------------------------------------------------------------------------------------------------------------—•°
