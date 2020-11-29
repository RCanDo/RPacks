## ---------------------------------------------------------------------------------------------------------------------—•°
## Write() generic and methods.
## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {DM}
##  Write()    generic
##  Write.summary.curves_by_groups()
##  Write.summary.curves_and_prediction()
##
## DEPENDENCIES
##  tm()             {DM}  tm.R     [defalt {package} is the same; default file.R is the same as function name]
##  write.table()    {base}
##
## REMARKS/WARNINGS
##
## TODO
## •
##
## DESCRIPTION
##  The aim, the idea, problems, solutions... briefly!, concerning the whole set of functions
##
## -------------------------------------------------------------------------------------------------—•°
## AUTHOR: RCanDo (Arkadiusz Kasprzyk) akasp@...
## ver. 1.0
## PATH: D:\ROBOCZY\...
## start: 2017-09-01    last: 2017-09-27
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
Write <- function(x,...){UseMethod("Write")}
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
Write.summary.curves_by_groups <- function( scbg , file = paste(deparse(substitute(scbg)),format(Sys.time(),"%Y-%m-%d_%H'%M'%S.csv"),sep="_")
                                          , head = FALSE , round = FALSE , as_rows = FALSE  ## tm parameters
                                          , sep = ";" , na="NA" , dec = "," , qmethod = "double"
                                          , col.names = NA , row.names = TRUE, ...
                                          ){
## ------------------------------------------------------------------------------------------------—•°
##
## ------------------------------------------------------------------------------------------------—•°
force(file)
scbg <- tm(scbg, head = head , round.attr=FALSE , round = round , transpose = as_rows )
write.table( scbg , file=file , sep = sep , na=na , dec = dec , qmethod = qmethod , col.names = col.names , row.names = row.names, ... )
cat("Table written to file \"",file,"\".\n",sep="")    ## "

}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
Write.summary.curves_and_prediction <- function( scap , file = paste(deparse(substitute(scap)),format(Sys.time(),"%Y-%m-%d_%H'%M'%S.csv"),sep="_")
                                          , head = FALSE , round = FALSE , as_rows = FALSE  ## tm parameters
                                          , sep = ";" , na="NA" , dec = "," , qmethod = "double"
                                          , col.names = NA , row.names = TRUE, ...
                                          ){
## ------------------------------------------------------------------------------------------------—•°
##
## ------------------------------------------------------------------------------------------------—•°
force(file)
scap <- tm(scap, head = head , round.attr=FALSE , round = round , transpose = as_rows )
write.table( scap , file=file , sep = sep , na=na , dec = dec , qmethod = qmethod , col.names = col.names , row.names = row.names, ... )
cat("Table written to file \"",file,"\".\n",sep="")    ## "

}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
Write.smooth_gam <- function( sg , file = NULL
                           , head = FALSE , round = FALSE , as_rows = FALSE  ## tm parameters
                           , sep = ";" , na="NA" , dec = "," , qmethod = "double"
                           , col.names = NA , row.names = TRUE, ...
                           ){
## ------------------------------------------------------------------------------------------------—•°
##
## ------------------------------------------------------------------------------------------------—•°

if(is.null(file)){
   file <- deparse(substitute(sg))
   files <- paste(file,c("nor","len","cum"),format(Sys.time(),"%Y-%m-%d_%H'%M'%S.csv"),sep="_")
   tables <- c("smoothed_curves","smoothed_curves_len","smoothed_curves_cum")
}else{
   files <- adjust(files,3)    ## {DM}  lenghten.R
   if(length(unique(files))<3) files <- paste(files,c("nor","len","cum"),sep="_")
}

what <- (1:3)[ c( TRUE , !is.null(sg$lengthen) , sg$arguments$method=="cumulated" ) ]

write.fun <- function(i){
   tt <- tm(sg[[tables[i]]], head = head , round.attr=FALSE , round = round , transpose = as_rows )
   write.table( tt , file=files[i] , sep = sep , na=na , dec = dec , qmethod = qmethod , col.names = col.names , row.names = row.names, ... )
   cat("Table ", tables[i] ," written to file \"",files[i],"\".\n",sep="")    ## "
}

invisible(sapply(what,write.fun))

## ------------------------------------------------------------------------------------------------—•°

}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°


## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°


## ---------------------------------------------------------------------------------------------------------------------—•°
## EXAMPLES ############################################################################################################—•°
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
dummy = function(){
## This is dummy function — it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## — in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
## -------------------------------------------------------------------------------------------—•°
## RELOADER — before it works you need to source("PacksAK.R"); it's best to use {package_name}.R within pack's dir.
 loadPacksAK("DM")
## -------------------------------------------------------------------------------------------—•°



## ---------------------------------------------------------------------------------------------------------------------—•°
};rm(dummy)
