## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE
##  nulls.table()
##  nulls0.table()
##
## DEPENDENCIES
##  capply
##  update.df
##    ...() , ...
##
## ---------------------------------------------------------------------------------------------------------------------—•°

nulls.table <- function(datfram, class=NULL, rownames="order", sort="NAs", decreasing=FALSE){
## -------------------------------------------------------------------------------------------------—•°
## Simple table (of class 'data.frame') with number of NAs in each column of data frame 'datfram'.
##  datfram = data.frame
##  nulls.table = [ NAs  OKs  NAs/all unique.values ]
##    NAs -- nr of NAs in each column
##    OKs -- nr of non-empty entries in each column
##    OKs/all -- ratio of OKs to all values in each column.
##
## class    indicates variables of which class take into consideration - those which are not of a given class(es)
##          are excluded from the datfram; a character vector;
## rownames "name", "id", "order"
## sort     character, one of "id", "NAs", "OKs", "unique_values", "class"
##          this however allows for sorting only wrt to one variable;
##          for sorting with more variables use
##           taRifx::sort(df, f = ~ -a + b)  or  dplyr::arrange(df, desc(a), b)
##          taRifx::sort() allows for using strings for variable names as it uses formula syntax:
##           taRifx::sort(df, f = formula("~ -a + b"));
##          dplyr::arange() does not allow for using strings.
## decreasing = FALSE
##
##   Remark
## For multidimensional columns (those which are matrices) a record is considered to be NA
## if one of its elements is NA.
## -------------------------------------------------------------------------------------------------—•°

var.idx = 1:ncol(datfram)

if(!is.null(class)){
   var.in.class = sapply(datfram, base::class) %in% class
   datfram = datfram[ , var.in.class ]
}else{
   var.in.class = rep(TRUE, length(var.idx))
}

nr = nrow(datfram)

## ---------------------------------------------------------—•°
## searching for proper indices in case of uni- and multi- variate columns
NAs_idx <- function(variable){
    ## logical index of NA values for uni-variate (standard) and multi-variate (rare) columns
    ## for multivariate columns a row is considered to be NA if one of its elements is NA
    if(is.null(dim(variable))){
        idx <- is.na(variable)
    }else{
        idx <- apply(variable, 1, function(x)as.logical(sum(is.na(x))))
    }
    return(idx)
}


NAs = sapply(datfram, function(v){sum(NAs_idx(v))})
OKs = nr - NAs

nulls.df = data.frame(
      "id" = var.idx[var.in.class]
    , "NAs"  = NAs
    , "OKs"  = OKs
    , "OKs/all" = signif(OKs/nr, 3)
    , "unique_values" = sapply(datfram, function(x){length(unique(x[ !is.na(x) ]))})
    , "class" = sapply(datfram, FUN=function(x){base::class(x)[1]})
    #, "mode" = sapply(datfram, mode)       ## it is ALWAYS numeric...
    , stringsAsFactors = FALSE
)

## ---------------------------------------------------------—•°
##
#colnames(nulls.df) = c("id", "NAs", "OKs", "OKs/all", "unique_values", "class") #, "mode")     ## only because of "NAs/all"
colnames(nulls.df)[4] <- "OKs/all"

## ---------------------------------------------------------—•°
## sorting

if(!sort %in% c('name', 'id', 'NAs', 'OKs', 'OKs/all', 'unique_values', 'class')){
    warning("For sorting `sort` must be one of 'name', 'id', 'NAs', 'OKs', 'OKs/all', 'unique_values', 'class'.")
    sort <- "id"
}

if(rownames=="name"){       # rownames are already "name"s
    if(sort=="name"){
        nulls.df <- nulls.df[order(rownames(nulls.df), decreasing=decreasing), ]
    }else{
        nulls.df <- nulls.df[order(nulls.df[, sort], decreasing=decreasing), ]
    }
    nulls.df$order <- 1:nrow(nulls.df)
    nulls.df <- nulls.df[c("order", "id", "NAs", "OKs", "OKs/all", "unique_values", "class")]
}else if(rownames=="id"){   # rownames need to be changed
    nulls.df$name <- rownames(nulls.df)
    rownames(nulls.df) <- nulls.df$id
    nulls.df$id <- NULL
    if(sort=="id"){
        nulls.df <- nulls.df[order(as.integer(rownames(nulls.df)), decreasing=decreasing), ]
    }else{
        nulls.df <- nulls.df[order(nulls.df[, sort], decreasing=decreasing), ]
    }
    nulls.df$order <- 1:nrow(nulls.df)
    nulls.df <- nulls.df[c("order", "name", "NAs", "OKs", "OKs/all", "unique_values", "class")]
}else if(rownames=="order"){
    nulls.df$name <- rownames(nulls.df)
     # sorting
    nulls.df <- nulls.df[order(nulls.df[, sort], decreasing=decreasing), ]
     #
    rownames(nulls.df) <- 1:nrow(nulls.df)
    nulls.df <- nulls.df[c("id", "name", "NAs", "OKs", "OKs/all", "unique_values", "class")]
}

nulls.df

} ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°

nulls0.table <- function(datfram, class=NULL, rownames="order", sort="NAs", decreasing=FALSE){
## -------------------------------------------------------------------------------------------------—•°
## Version of nulls.table() with additional columns "x<0" , "x==0"
## which reports number of values < 0 and == 0 for each numeric variable.
##
## sort     character, one of "id", "NAs", "OKs", "unique_values", "x<0", "x==0", "class"
##          see more in help for nulls.table();
## decreasing = FALSE
## -------------------------------------------------------------------------------------------------—•°

if(sort %in% c("x<0", "x==0")){
    sort0 <- sort
    sort <- "id"
}else(
    sort0 <- NULL
)

nulls.df <- nulls.table(datfram, class, rownames, sort, decreasing)

nulls.df.0 <- as.data.frame( t(  #!!!
    capply(
        datfram,
        class=c("integer", "numeric"),
        function(x){
            c( "x<0" = sum(x<0, na.rm=TRUE),
               "x==0" = sum(x==0, na.rm=TRUE)
             )
        }
        )
    ) )

if(rownames=="name"){
    nulls.df = update.df( nulls.df, nulls.df.0, id = 0 )
}else{
    nulls.df = update.df( nulls.df, nulls.df.0, id = "name" )
}

if(!is.null(sort0)){
    nulls.df <- nulls.df[order(nulls.df[, sort0], decreasing=decreasing), ]
    if(rownames=="order"){
        rownames(nulls.df) <- 1:nrow(nulls.df)
    }else{
        nulls.df$order <- 1:nrow(nulls.df)
    }
}

nulls.df

}  ##----END----##
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
## -------------------------------------------------------------------------------------------------—•°
 ## RELOADER - before it works you need to source("RCanDo.R"); it's best to use DM.R within pack's dir.
 loadPacksAK("DM")
## -------------------------------------------------------------------------------------------------—•°


## ---------------------------------------------------------------------------------------------------------------------—•°
}; rm(dummy)
