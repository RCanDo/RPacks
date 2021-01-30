## ---------------------------------------------------------------------------------------------------------------------—•°
## INFIX FUNCTIONS
## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE
##
##
## DEPENDENCIES
##
## TODO
##
## ---------------------------------------------------------------------------------------------------------------------—•°


## ---------------------------------------------------------------------------------------------------------------------—•°
`%C%` <- function(a, b) if (!is.null(a)) a else b       ## like  coalesce() in SQL
# function_that_might_return_null() %||% default value
# NULL %C% 2
# 3 %C% 2
# NULL %C% NULL %C% 2

coalesce = function(...){             ## like  coalesce() in PgSQL
ll = list(...)
Find(Negate(is.null),ll)
}
#   coalesce(NULL,"c",2,"a")
#   coalesce(NULL,2,"a")
#   coalesce(NULL,NULL,2,"a")

`%P%` <- function(a, b) paste0(a, b)             ## backticks !!!
# "a" %P% "b" #> [1] "ab"

## -------------------------------------------------------------------------------------------------—•°
## set operators

## (ak)
`%+%` <- function(a,b) union(a,b)
#   letters[1:4] %+% 1:4
#   letters[1:4] %+% 1:4 %+% letters[3:6]

`%&%` <- function(a,b) intersect(a,b)
#   letters[1:4] %&% 1:4
#   letters[1:4] %&% letters[2:6]
#   letters[1:4] %&% letters[2:6] %&% letters[3:8]

`%|%` <- function(a,b) setdiff(union(a,b),intersect(a,b))
# letters[1:4] %|% letters[3:6]

## -------------------------------------------------------------------------------------------------—•°
## logical operators

`%||%` <- function(a,b) a&!b | !a&b
# c(TRUE,TRUE,FALSE,FALSE) %||% c(TRUE,FALSE,FALSE,TRUE)


## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
## EXAMPLES ############################################################################################################—•°
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
dummy = function(){
## This is dummy function - it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## - in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
## -------------------------------------------------------------------------------------------—•°
## RELOADER —— before it works you need to source("PacksAK.R"); it's best to use EfficiencyCurves.R within pack's dir.
 loadPacksAK("DM")
## -------------------------------------------------------------------------------------------—•°

NULL %C% 2
3 %C% 2
NULL %C% NULL %C% 2

coalesce(NULL,"c",2,"a")
coalesce(NULL,2,"a")
coalesce(NULL,NULL,2,"a")

"a" %P% "b"

letters[1:4] %+% 1:4
letters[1:4] %+% 1:4 %+% letters[3:6]

letters[1:4] %&% 1:4
letters[1:4] %&% letters[2:6]
letters[1:4] %&% letters[2:6] %&% letters[3:8]

# disjunction  (pl. dysjunkcja)
c(TRUE,TRUE,FALSE,FALSE) %||% c(TRUE,FALSE,FALSE,TRUE)

c(TRUE,FALSE)|c(TRUE,TRUE)
c(TRUE,FALSE)||c(TRUE,TRUE)

c(FALSE,FALSE)|c(FALSE,TRUE)
c(FALSE,FALSE)||c(FALSE,TRUE)

# symmetric difference
letters[1:4] %|% letters[3:6]


c(TRUE,FALSE)&c(TRUE,TRUE)
!c(TRUE,TRUE)&!c(TRUE,TRUE)
!c(TRUE,FALSE)&!c(TRUE,TRUE)
!c(TRUE,FALSE)&!c(TRUE,FALSE)
!c(TRUE,FALSE)&!c(FALSE,FALSE)

c(TRUE,FALSE)&&c(TRUE,TRUE)

## ---------------------------------------------------------------------------------------------------------------------—•°
}; rm(dummy)
