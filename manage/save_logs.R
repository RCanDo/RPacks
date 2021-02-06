## ---------------------------------------------------------------------------------------------------------------------—•°
## Updating configuration lists, like ID, PATH, FILE, PAR with respective values of
## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {manage}
##  update_configs( config_node, config_node_2, overwrite = FALSE, envir = .GlobalEnv )
##
## DEPENDENCIES
##  indentr()         dm
##
## REMARKS/WARNINGS
##
## TODO
##  Try rlang version
##
## DESCRIPTION
##
## -------------------------------------------------------------------------------------------------—•°
## AUTHOR: Arkadiusz Kasprzyk; rcando@int.pl
## ---------------------------------------------------------------------------------------------------------------------—•°

save_logs_json <- function(..., one_file=NULL, dir="."){
## -------------------------------------------------------------------------------------------------—•°
## ...             objects to be saved to .json files with the same names as these objects
## one_file=NULL   if NULL then each object is saved to separate .json file
##                 if a name of a file is provided (with ".json" extension) then all objects
##                 are joined to one list and saved to the file with this name
## dir="."         path to save the files.
## -------------------------------------------------------------------------------------------------—•°

ll <- list(...)
names(ll) <- all.vars(match.call())[1:length(ll)]
  ## [1:length(ll)] is usually unnecessary but sometimes strange things happen
  ## (see comments in save_logs_txt())

if(is.null(one_file)){
    lapply( names(ll), function(n) jsonlite::write_json(ll[[n]], path = file.path(dir, paste0(n, ".json")), pretty=TRUE) )
}else{
    jsonlite::write_json(ll, path = file.path(PATH$model, one_file), pretty=TRUE)
}

return(TRUE)
}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°

save_logs_txt <- function(..., one_file=NULL, dir=".", args=list()){
## -------------------------------------------------------------------------------------------------—•°
## ...             objects to be saved to .txt files with the same names as these objects
## one_file=NULL   if NULL then each object is saved to separate .json file
##                 if a name of a file is provided (with ".txt" extension) then all objects
##                 are joined to one list and saved to the file with this name
## dir="."         path to save the files.
## args=list()     used to pass arguments to indentr() from DM;
## -------------------------------------------------------------------------------------------------—•°

ll <- list(...)

names(ll) <- all.vars(match.call())[1:length(ll)]
  ## [1:length(ll)] is usually unnecessary but sometimes strange things happen:
  ## e.g. without this when calling
  ##   save_logs_txt(a, b, f, pars=list(compact=c(0,2), info=F))
  ## all.vars(match.call())  returns  "a" "b" "f" "F"    WHY???
  ## and there is an error; however
  ##   save_logs_txt(a, b, f, pars=list(compact=c(0,2), info=FALSE))
  ## works!

print_to_txt_file <- function(obj, path){
    sink(file=path, split=FALSE)
    do.call(indentr, c(list(ll=obj), args))
    sink()
}

if(is.null(one_file)){
    lapply( names(ll), function(n) print_to_txt_file(ll[[n]], path = file.path(dir, paste0(n, ".txt"))) )
}else{
    print_to_txt_file(ll, path = file.path(PATH$model, one_file))
}

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
 loadPacksAK("manage")  # not registered !
## -------------------------------------------------------------------------------------------------—•°

a = list(1, 3, 10.3, 100, TRUE, FALSE)
b = 1:3
d = list(p=list('a','b', 'c'), q=4:6, r='A', u=c(F, T, F, F, F, T))
f = function(x) x+1

save_logs_json(a, b, d, f)

save_logs_txt(a, b, d, f)
save_logs_txt(a, b, d, f, args=list(compact=c(0,2), info=F))


## see: https://stackoverflow.com/questions/11885207/get-all-parameters-as-list
## think below over
ff <- function(..., p1=".log", all=TRUE){
print(all.vars(match.call()))
}

ff(a, b, d, p1=".qq")

all.names(expression(sin(x+y)))
all.names(quote(sin(x+y))) # or a call
all.vars(expression(sin(x+y)))
all.vars(expression(log(x, y)))

args
all.vars

## ---------------------------------------------------------------------------------------------------------------------—•°
}; rm(dummy)