## -------------------------------------------------------------------------------------------------—•°
## import configs
##
## DEPENDENCES:
##  update_config()
##  reclassr()
##
## TODO:
##  recursive "config.list" class !!!
## -------------------------------------------------------------------------------------------------—•°

## -------------------------------------------------------------------------------------------------—•°
import_configs <- function(
    file,
    flatten = TRUE,
    update = TRUE,
    overwrite = FALSE,
    envir = .GlobalEnv,
    message=TRUE
){
## -------------------------------------------------------------------------------------------------—•°
## Imports config from given location
## and uses update_config() to update already present config lists: ID, PATH, FILE, PAR.
## ...
## -------------------------------------------------------------------------------------------------—•°
if (!require(jsonlite)) install.packages("jsonlite")

result <- tryCatch(
    {
        configs <- jsonlite::fromJSON(file, flatten = flatten)  ## read_json()
        configs <- reclassr(configs, "list", "config.list")

        if(message) message(Sys.time(), " configs imported successfully")

        for(cn in names(configs)){
            ##class(configs[[cn]]) <- c("config.list", "list")                                   #!!! ???
            if(update){
                update_configs(cn, configs[[cn]], overwrite, envir)
            }
        }
        return(configs)
    },
    error = function(e){ print(e); return(NULL)}
)

return(result)
}
## -------------------------------------------------------------------------------------------------—•°

