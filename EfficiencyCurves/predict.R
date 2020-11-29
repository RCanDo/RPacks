## ---------------------------------------------------------------------------------------------------------------------—•°
## predict() methods for "curves_by_groups" and "curves_and_prediction".
## print() methods for "predict.curves_by_groups" and "predict.curves_and_prediction".
## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {EfficiencyCurves}
##  predict.curves_by_groups()
##  print.predict.curves_by_groups()
##  predict.curves_and_prediction()
##  print.predict.curves_and_prediction()
##
## DEPENDENCIES
##  indentr()        {DM}
##  coalesce()       {DM}  infix.R
##  prediction.curves_by_groups()   {EfficiencyCurves}
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
## AUTHOR: Arkadiusz Kasprzyk; akasp@...
## ver. 1.0
## PATH: D:\ROBOCZY\...
## start: 2017-03-01    last: 2017-09-26
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
predict.curves_and_prediction <- function(cap,type="weighted_curve",messages=TRUE){
## ------------------------------------------------------------------------------------------------—•°
##    Arguments
## cap                        object of class "curves_and_prediction".
## type = "weighted_curve"    character vector with names of elements of 'cap' to be returned;
##                            possible values are any of the names(cap);
##                            you may also pass "ref" to return all of
##                             "curves", "net_curve", "ref_yields_groups", "groups", "counts", "amounts",
##                            or "val" to return all of
##                             "weights","weighted_curve", "net_curve", "val_yields_from_weighted",
##                             "val_yields_from_net", "val_yields_groups", "groups", "regroup", "counts", "amounts".
##                            if "all" then the whole 'cap' is returned.
## messages = TRUE            should messages be printed or not?
##
##    Result
## Selected elements of the object 'cap'.
##
## ------------------------------------------------------------------------------------------------—•°

pcap <- if(type[1]=="all"){
   cap
}else if(type[1]=="val"){
   type2 <- c( "counts" , "amounts"
             #,"ref_yields_groups"
             , "groups" , "regroup"
             , "weights"
             #, "curves"
             , "weighted_curve"
             , "net_curve"
             , "val_yields_from_weighted" , "val_yields_from_net"
             , "val_yields_groups"
             )
   cap$counts <- cap$counts["valuated",] ; margins(cap$counts) <- 2
   cap$amounts <- cap$amounts["valuated",] ; margins(cap$amounts) <- 2
   cap[type2]
}else if(type[1]=="ref"){
   type2 <- c( "counts" , "amounts"
             , "ref_yields_groups"
             , "groups" #, "regroup"
             #, "weights" #
             , "curves"
             #, "weighted_curve"
             , "net_curve"
             #, "val_yields_from_weighted" , "val_yields_from_net"
             #, "val_yields_groups"
             )
   cap$counts <- cap$counts[-1,] ; margins(cap$counts) <- 2
   cap$amounts <- cap$amounts[-1,] ; margins(cap$amounts) <- 2
   cap[type2]
}else if(length(type)==1){
   cap[[type]]
}else{
   cap[type]
}

if(messages){
   messages(pcap) <- messages(cap)
}

class(pcap) <- union(c("predict.curves_and_prediction","predict"),class(pcap))
attr(pcap,"predict_type") <- type

pcap

}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
print.predict.curves_and_prediction <- function( pcap
                           , round = 3        ## tm
                           , as_rows = FALSE  ## tm  (transpose)
                           , messages = coalesce(getOption("messages"),TRUE)
                           , round.attr = TRUE    ## tm
                           , ...
                           ){
## ------------------------------------------------------------------------------------------------—•°
##    Arguments
## ...
##    Result
##
## ------------------------------------------------------------------------------------------------—•°
cat("object of class \"predict.curves_and_prediction\"\n")
cat("type = \""); cat(attr(pcap,"predict_type"),sep="\", \""); cat("\"\n\n") #"

indentr( if(is.list(pcap)){unclass(pcap)}else{pcap}
       , vsep=1 , messages=messages
       , transpose=as_rows , margins=TRUE , comment=TRUE , round=round , round.attr=round.attr
       , ...
)

}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
predict.curves_by_groups <- function( cbg , newdata = NULL
                                    , type = if(is.null(newdata)) "net_curve" else "weighted_curve"
                                    , ...
                                    ){
## ------------------------------------------------------------------------------------------------—•°
##    Arguments
## ...
##    Result
##
## ------------------------------------------------------------------------------------------------—•°
force(type)
if(is.null(newdata)){

   pcap <- if(type[1]=="all")  cbg  else if(length(type)==1)  cbg[[type]]  else  cbg[type]

   class(pcap) <- union(c("predict.curves_by_groups","predict"),class(pcap))
   attr(pcap,"predict_type") <- type

}else{
   pcap <- predict( prediction(cbg, newdata, ...), type )
}

pcap

}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
print.predict.curves_by_groups <- function( pcbg
                           , round = 3        ## tm
                           , as_rows = FALSE  ## tm  (transpose)
                           , messages = coalesce(getOption("messages"),TRUE)
                           , round.attr = TRUE    ## tm
                           , ...
                           ){
## ------------------------------------------------------------------------------------------------—•°
##    Arguments
## ...
##    Result
##
## ------------------------------------------------------------------------------------------------—•°
cat("object of class \"predict.curves_by_groups\"\n")
cat("type = \""); cat(attr(pcbg,"predict_type"),sep="\", \""); cat("\"\n\n") #"

indentr( if(is.list(pcbg)){unclass(pcbg)}else{pcbg}
       , vsep=1 , messages=messages
       , transpose=as_rows , margins=TRUE , comment=TRUE , round=round , round.attr=round.attr
       , ...
)

}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
