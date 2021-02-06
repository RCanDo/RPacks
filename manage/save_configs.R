## ---------------------------------------------------------------------------------------------------------------------—•°
## Saving config lists to .json file.
## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {manage}
##  save_configs(file)
##
## DEPENDENCIES
##
## REMARKS/WARNINGS
##
## TODO
##
## DESCRIPTION
##
## -------------------------------------------------------------------------------------------------—•°
## AUTHOR: Arkadiusz Kasprzyk; rcando@int.pl
## ---------------------------------------------------------------------------------------------------------------------—•°

save_configs <- function(file){
## -------------------------------------------------------------------------------------------------—•°
##
## -------------------------------------------------------------------------------------------------—•°
result <- tryCatch({
    configs <- list(
        ID = if(exists("ID", envir=.GlobalEnv)){ get("ID", envir=.GlobalEnv) }else{ NULL },
        PATH = if(exists("PATH", envir=.GlobalEnv)){ get("PATH", envir=.GlobalEnv) }else{ NULL },
        FILE = if(exists("FILE", envir=.GlobalEnv)){ get("FILE", envir=.GlobalEnv) }else{ NULL },
        PAR = if(exists("PAR", envir=.GlobalEnv)){ get("PAR", envir=.GlobalEnv) }else{ NULL }
        )
    jsonlite::write_json(configs, path = file, pretty=TRUE)
    return(configs)
    },
    error = function(e){ print(e); return(NULL)}
)

return(result)
}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°
