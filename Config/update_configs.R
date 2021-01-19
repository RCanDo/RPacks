## -------------------------------------------------------------------------------------------------—•°
## Updating configuration lists, like ID, PATH, FILE, PAR with respective values of
## -------------------------------------------------------------------------------------------------—•°

## -------------------------------------------------------------------------------------------------—•°
update_configs <- function(
    config_node, config_node_2,
    overwrite = FALSE,
    envir = .GlobalEnv
){
## -------------------------------------------------------------------------------------------------—•°
## Updating configuration lists, like ID, PATH, FILE, PAR with respective values of ...
##    Arguments
##  config_node    config node object (list) or its name as character string;
##  config_node_2  config node object (list);
## Both are joined and if names of their items overlap then:
## if `overwrite=TRUE` then values from `config_node_2` overwrite values from `config_node`;
## if `overwrite=FALSE` then values from `config_node` are retained;
##    Returns
## It works by side-effects (although returns TRUE):
## it updates `config_node` (if doesn't exist then is created) within .GlobalEnv
## with values from `config_node_2`.
## -------------------------------------------------------------------------------------------------—•°
class(config_node_2) <- c("config.list", "list")

if(!is.character(config_node)){
    config_node <- deparse(substitute(config_node))  ## to string
}

if(exists(config_node, envir=envir)){
    CONF <- get(config_node, envir=envir)
}else{
    CONF <- config.list()
}

if(overwrite){
    CONF[names(config_node_2)] <- config_node_2
}else{
    config_node_2[names(CONF)] <- CONF
    CONF <- config_node_2
}

assign(config_node, CONF, envir=envir)

return(TRUE)
}
## -------------------------------------------------------------------------------------------------—•°
