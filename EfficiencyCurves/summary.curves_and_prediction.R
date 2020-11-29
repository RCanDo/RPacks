## ---------------------------------------------------------------------------------------------------------------------—•°
## summary() and print() methods for "curves_and_prediction".
## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {EfficiencyCurves}
##  print.curves_and_prediction()
##  summary.curves_and_prediction()
##  print.summary.curves_and_prediction()
##
## DEPENDENCIES
##  printtm()     {DM}
##  indentr()     {DM}
##  coalesce()    {DM}  infix.R
##  whileif()     {DM}
##
## TODO
## •
##
## DESCRIPTION
##  The aim, the idea, problems, solutions... briefly!, concerning the whole set of functions
##
## REMARKS/WARNINGS
##
## -------------------------------------------------------------------------------------------------—•°
## AUTHOR: Arkadiusz Kasprzyk; akasp@...
## ver. 1.0
## PATH: D:\ROBOCZY\...
## start: 2017-03-01    last: 2017-09-26
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
print.curves_and_prediction = function( cap  ## object of class "curves_and_prediction"
                           #, messages = coalesce(getOption("messages"),TRUE)
                           , round = 3         ## tm
                           , as_rows = FALSE   ## tm (transpose)
                           , round.attr = TRUE ## tm
                           , ...
                           ) {
## ------------------------------------------------------------------------------------------------—•°
##
## ------------------------------------------------------------------------------------------------—•°

cat("object of class \"", paste(class(cap),collapse="\", \"") ,"\"\n",sep="")

indentr( unclass(cap) , vsep=1
       , transpose=as_rows , margins=NULL , comment=TRUE , round=round , round.attr=round.attr
       , ...
       )
         ## passed to printtm() for elements inheriting from "tm"; but 2 first elements are not of this class;
         ## this is no problem as print() methods ignore irrelevant arguments! GREAT!

}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
summary.curves_and_prediction = function( cap  ## object of class "curves_and_prediction"
                           , head = NULL , round = 3 , as_rows = TRUE
                           , apply = FALSE
                           , yields = FALSE    ## if TRUE summary of yields else of efficiencies;
                                               ## if numeric then replaces  round_yields  and is turned into TRUE;
                           , round_yields = 0  ## how to round yields; used only if yields is not numeric
                           ) {
## ------------------------------------------------------------------------------------------------—•°
##    Arguments
## ...
##    Result
##
## ------------------------------------------------------------------------------------------------—•°


if(is.null(yields)){
   yields <- FALSE
}else if(is.numeric(yields)){
   if( yields < 0 || yields != round(yields,0) ) stop( "'yields' must be NULL, logical or positive integer." )
   round_yields <- yields
   yields <- TRUE
}else if(!is.logical(yields)){
   stop("'yields' must be NULL, logical or positive integer.")
}

if(yields){
   gw.curves.2 = cbind(cap$val_yields_groups,cap$val_yields_from_weighted)
   gw.curves.2 = round(gw.curves.2,round_yields)
   gw.curves.2 = rbind(group_yields = colSums(gw.curves.2),gw.curves.2)

#      gw.curves.2 <- tm(cbg$yields)
#      nr <- nrow(gw.curves.2)
#      rownames(gw.curves.2)[nr] <- "group_yields"
#      gw.curves.2 <- gw.curves.2[c(nr,1:(nr-1)),]
}else{
   cap$curves = cbind(cap$curves,cap$weighted_curve)
   gw.curves.2 = rbind(group_efficiencies = colSums(cap$curves),cap$curves)
                        #!#!!! groups_efficiency may by misleading if 'horizon' is not even

}

#aw.refval <- rbind( addmargins(cap$counts[1:2,] ,2)
#                  , addmargins(cap$amounts[1:2,],2)
#                  ###
#                  , addmargins(efficiency(cap$counts[1:2,] ,1),2)
#                  , addmargins(efficiency(cap$amounts[1:2,],1),2)
#                  , c(cap$weights,1)
#                  )

aw.refval <-addmargins( rbind( cap$counts[2:1,]
                             , cap$amounts[2:1,]
                             ###
                             , efficiency(cap$counts[2:1,] ,1)
                             , efficiency(cap$amounts[2:1,],1)
                             , cap$weights
                             )
                      , 2 )

rownames(aw.refval) <- c( "ref_counts" , "val_counts" , "ref_amounts" , "val_amounts"
                        , "ref_c.ratios" , "val_c.ratios" , "ref_a.ratios" , "val_a.ratios"
                        , "weights"
                        )

summary_df = rbind( aw.refval , gw.curves.2 )
colnames(summary_df)[ncol(summary_df)] = "weighted_curve"

names(attr(summary_df,"dimnames")) <- list("time","group")

if(apply){
   summary_df <- round( summary_df , round )
   summary_df <- head( summary_df , max(coalesce(head,length(cap$net_curve))+10,10) )
   if(as_rows){ summary_df <- t(summary_df) }
}

round.attr(summary_df)  <- round
attr(summary_df,"head") <- head
attr(summary_df,"transpose") <- as_rows & !apply

attr(summary_df,"yields") = yields

messages(summary_df) <- messages(cap)
  ##
if( nrow(cap$counts)>2 ){
   newMessage(summary_df) <- Message("UnevenHorizon",
"Horizon is uneven for reference portfolio i.e. counts and amounts decreases across time
while in the summary they are given only for time = 1.")
}

class(summary_df) <- union( "summary.curves_and_prediction" , class(summary_df) )
attr(summary_df,"input_class") <- class(cap)

summary_df

}  ##----END----##
## --------------------------------------------------------—•°
## --------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
print.summary.curves_and_prediction = function( scap  ## object of class "summary.curves_and_prediction"
                                              #, messages = coalesce(getOption("messages"),TRUE)
                                              , head = NULL     ## method for "summary.curves_and_prediction"
                                              , round = 3       ## tm
                                              , as_rows = NULL  ## tm (transpose)
                                              , ...             ## tm
                                       ) {
## ------------------------------------------------------------------------------------------------—•°
##
## ------------------------------------------------------------------------------------------------—•°
cat("summary of the object of class \"" , paste(attr(scap,"input_class"),collapse="\", \""), "\"; ",sep="")
if(attr(scap,"yields")) cat("yields:") else cat("efficiencies:")
cat("\n\n")

head = whileif(head,iftrue=NULL,ifnull=head.attr(scap),ifnull=FALSE)
scap <- head(scap,head)

transpose = coalesce( as_rows , transpose(scap) , FALSE )

class(scap) <- setdiff(class(scap),"summary.curves_and_prediction")

printtm( scap , transpose=transpose , round = round , ... )             ## matrix tm  time x groups

}  ##----END----##
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

 ## RELOADER — before it works you need to source("PacksAK.R"); it's best to use EfficiencyCurves.R within pack's dir.
 loadPacksAK("EfficiencyCurves")

## -------------------------------------------------------------------------------------------—•°

## Examples of using print.curves_by_groups() are in the dummy() part of  curves_by_groups.R

}
## ---------------------------------------------------------------------------------------------------------------------—•°
rm(dummy)
