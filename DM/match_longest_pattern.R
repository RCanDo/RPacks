## ---------------------------------------------------------------------------------------------------------------------—•°
## Finds longest pattern fitting to the given string.
## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {package}
##  match_longest_pattern(patterns, string)
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

match_longest_pattern <- function(patterns, string){
## -------------------------------------------------------------------------------------------------—•°
## Finds longest pattern matching the given string.
## Returns only first longest match
## -------------------------------------------------------------------------------------------------—•°

res <- sapply(patterns, function(x)grep(x, string, value=TRUE))
ok <- sapply(res, function(x)length(x)>0)
nams <- names(res[ok])
nams.length <- nchar(nams)
idx = which(nams.length==max(nams.length))[1]
nams[idx]

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
 loadPacksAK("{package_name}")
## -------------------------------------------------------------------------------------------------—•°

patterns <- c('aba', 'baba', 'var', 'Var', 'variable')
string <- 'var_bin'
match_longest_pattern(patterns, string)   ## 'var'

patterns <- c('aba', 'baba', 'var', 'Var', 'varibaba')
string <- 'aba_baba_varibaba'
match_longest_pattern(patterns, string)   ## 'var'

## ---------------------------------------------------------------------------------------------------------------------—•°
}; rm(dummy)
