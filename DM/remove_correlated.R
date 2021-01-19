## ---------------------------------------------------------------------------------------------------------------------—•°\
##
## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {DM}
##  node()
##
## DEPENDENCIES
##
## TODO
##
## COMMENT
##
## ---------------------------------------------------------------------------------------------------------------------—•°


## ---------------------------------------------------------------------------------------------------------------------—•°
remove_correlated <- function(
    datfram,
    threshold = .9,
    columns = "",
    only_names = FALSE,
    verbose=FALSE
    ){

all_columns <- names(datfram)

columns <- whileif(columns, iffalse=NULL, iftrue="")

if(is.null(columns)){
    columns <- c()
}else{
    if(is.character(columns)){
        if(length(columns) == 1 && columns == ""){
            columns <- all_columns
        }else{
            columns <- intersect(columns, all_columns)
        }
    }else{
        stop("`columns` must be character vector with selection of `datfram`s columns or left NULL.")
    }
}

to_remove <- character(0)

if(length(columns > 0)){
    columns_2 <- columns

    for(col in columns){
        columns_2 <- setdiff(columns_2, c(to_remove, col))
        if(is.character(datfram[[col]]) || is.factor(datfram[[col]])){
            if(verbose){
                cat(col, " is character or factor and won't be checked'", "\n", sep="")
            }
            break
        }
        for(col_2 in columns_2){
            if(is.character(datfram[[col_2]]) || is.factor(datfram[[col_2]])){
                columns_2 <- setdiff(columns_2, col_2)
                if(verbose){
                    cat(col_2, " is character or factor and won't be checked'", "\n", sep="")
                }
                break
            }
            cr <- cor(datfram[[col]], datfram[[col_2]], use="pairwise.complete.obs")
                #print(cr)
            if(!is.na(cr) && abs(cr) >= threshold){
                to_remove <- c(to_remove, col_2)
                if(verbose){
                    cat("\n cor(", col, ", ", col_2, ") = ", cr, " -- ", col_2, " removed.\n", sep="")
                }
            }
        }
    }

    datfram <- datfram[setdiff(all_columns, to_remove)]
}else{
    warning("`columns` is empty -- nothing to do.")
}

if(verbose){
    cat("\nVariables removed due to high correlation with others\n")
    print(to_remove)
}

if(only_names){
    return(to_remove)
}else{
    return(datfram)
}
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
datfram <- data.frame(
    aa = 1:10 + rnorm(10, mean=0, sd=.5),
    bb = 10:1 + rnorm(10, mean=0, sd=1),
    cc = 1:10 + rnorm(10, mean=0, sd=2),
    dd = 10:1 + rnorm(10, mean=0, sd=3),
    ee = (1:10)^2 + rnorm(10, mean=0, sd=1),
    ff = 1:10 + sample(1:10, 10, replace=T)/2,
    gg = 1:10 - sample(1:10, 10, replace=T)/2,
    hh = sample(letters[1:3], 10, replace=T)
    )
datfram

cor(datfram[-8])

df2 <- remove_correlated(datfram)
names(df2)

df2 <- remove_correlated(datfram, .95)
names(df2)

df2 <- remove_correlated(datfram, columns=c("aa","bb", "cc"))
names(df2)

df2 <- remove_correlated(datfram, .5)
names(df2)


## ????
idxs <- cbind(sample(1:10, 10, replace=T), sample(1:7, 10, replace=T))
df <- as.matrix(datfram)
df[idxs] <- NA
(datfram <- as.data.frame(df))

df2 <- remove_correlated(datfram)
names(df2)

## ---------------------------------------------------------------------------------------------------------------------—•°
}; rm(dummy)
