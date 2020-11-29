########################################################################################################################•
## FUNCTIONS HERE
##  nulls.table()
##  nulls0.table()
##
## DEPENDENCIES
##  capply.r
##  update.df
##    ...() , ...
##
########################################################################################################################•

nulls.table <- function(datfram, class=NULL, rownames="id"){
########################################################################################################################
## Simple table (of class 'data.frame') with number of NAs in each column of data frame 'datfram'.
##  datfram = data.frame
##  nulls.table = [ NAs  OKs  NAs/all unique.values ]
##    NAs -- nr of NAs in each column
##    OKs -- nr of non-empty entries in each column
##    NAs/all -- ratio of NAs to all values in each column.
##
## class    indicates variables of which class take into consideration - those which aare not of a given class(es)
##          are excluded from the datfram; a character vector;
########################################################################################################################

var.idx = 1:ncol(datfram)

if(!is.null(class)){
   var.in.class = sapply(datfram, base::class) %in% class
   datfram = datfram[ , var.in.class ]
}else{
   var.in.class = rep(TRUE, length(var.idx))
}

nr = nrow(datfram)
NAs = sapply(datfram, function(x){sum(is.na(x))})
OKs = nr - NAs

nulls.df = data.frame(
      "name" = colnames(datfram)
    , "NAs"  = NAs
    , "OKs"  = OKs
    , "NAs/all" = signif(NAs/nr, 3)
    , "unique_values" = sapply(datfram, function(x){length(unique(x[ !is.na(x) ]))})
    , "class" = sapply(datfram, FUN=function(x){base::class(x)[1]})
    #, "mode" = sapply(datfram, mode)       ## it is ALWAYS numeric...
    , stringsAsFactors = FALSE
)

colnames(nulls.df) = c("name", "NAs", "OKs", "NAs/all", "unique_values", "class") #, "mode")     ## only because of "NAs/all"

rownames(nulls.df) = var.idx[var.in.class]

if(!rownames%in%c("id", "names")){
   warning("'rownames' should be \"id\" or \"names\". Current value is improper and is set to \"id\".")
}
if(rownames=="names"){
   nulls.df[, "id"] = rownames(nulls.df)
   rownames(nulls.df) = nulls.df$name
   nulls.df = nulls.df[, c("id", "NAs", "OKs", "NAs/all", "unique_values", "class")]
}

nulls.df

}


########################################################################################################################

nulls0.table <- function(datfram, class=NULL, rownames="id"){
## Version of nulls.table() with additional columns "x<0" , "x==0"
## which reports number of values < 0 and == 0 for each numeric variable.

nulls.df = update.df( nulls.table(datfram, class),
    as.data.frame( t(  #!!!
        capply(
            datfram,
            class=c("integer", "numeric"),
            function(x){
                c( "x<0" = sum(x<0, na.rm=TRUE),
                   "x==0" = sum(x==0, na.rm=TRUE)
                )
            }
        )
    ) ),
    id = 1 )[ , c(1:5, 7:8, 6 )]#c("name", "NAs", "OKs", "NAs/all", "unique_values", "x<0" , "x==0" , "class") ]  #!!!


if(!rownames%in%c("id", "names")){
    warning("'rownames' should be \"id\" or \"names\". Current value is improper and is set to \"id\".")
}

if(rownames=="names"){
    nulls.df[, "id"] = rownames(nulls.df)
    rownames(nulls.df) = nulls.df$name
    nulls.df = nulls.df[ ,c("id", "NAs", "OKs", "NAs/all", "unique_values", "x<0" , "x==0" , "class")]
}

nulls.df

}

########################################################################################################################
