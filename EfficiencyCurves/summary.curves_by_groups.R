########################################################################################################################—•°
## FUNCTIONS HERE    {EfficiencyCurves}
##  print.curves_by_groups()
##  summary.curves_by_groups()
##  print.summary.curves_by_groups()
##
## DEPENDENCIES
##  printtm()     {DM}
##  indentr()     {DM}
##  coalesce()    {DM}  infix.R
##  whileif()     {DM}
##  tm()          {DM}  .
##
## TODO
##
########################################################################################################################—•°

########################################################################################################################—•°
print.curves_by_groups = function( cbg  ## object of class "curves_by_groups"
                           #, messages = coalesce(getOption("messages"),TRUE)
                           , round = 3
                           , as_rows = FALSE
                           , round.attr = TRUE
                           , ...
                           ) {
###################################################################################################—•°
##
###################################################################################################—•°

cat("object of class \"curves_by_groups\"\n")

indentr( unclass(cbg) , vsep=1
       , transpose=as_rows , margins=NULL , comment=TRUE , round=round , round.attr=round.attr
       , ...
       )

}  ##----END----##
########################################################################################################################—•°

########################################################################################################################—•°
summary.curves_by_groups = function( cbg  ## object of class "curves_by_groups"
                           , head = NULL , round = 3 , as_rows = TRUE
                           , apply = FALSE
                           , yields = FALSE    ## if TRUE summary of yields else of efficiencies;
                                               ## if numeric then replaces  round_yields  and is turned into TRUE;
                           , round_yields = 0  ## how to round yields; used only if yields is not numeric
                           ) {
###################################################################################################—•°
##
###################################################################################################—•°

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
   gw.curves.2 <- tm(cbg$yields,round=round_yields)      #!!!! NOT TESTED !!!!
   nr <- nrow(gw.curves.2)
   rownames(gw.curves.2)[nr] <- "group_yields"
   gw.curves.2 <- gw.curves.2[c(nr,1:(nr-1)),]
}else{
   gw.curves <- cbind( cbg$curves , net_curve = cbg$net_curve )
   gw.curves.2 <- rbind(group_efficiencies = colSums(gw.curves),gw.curves)
      #!#! groups_efficiency may by misleading if horizon is not even
}

   ca.g.ref <- rbind( cbg$counts[1,], cbg$amounts[1,] )
   effs.g.ref <- rbind( efficiency( cbg$counts[1,] ) , efficiency( cbg$amounts[1,] ) )

   summary_df <- rbind( addmargins(ca.g.ref,2) ,addmargins(effs.g.ref,2) , gw.curves.2 )
   rownames(summary_df)[1:4] <- c("counts","amounts","c.ratios","a.ratios")
   colnames(summary_df)[ncol(summary_df)] <- "net_values"

names(attr(summary_df,"dimnames")) <- list("time","group")

if(apply){
   summary_df <- round( summary_df , round )
   summary_df <- head( summary_df , max(coalesce(head,length(cbg$net_curve))+5,5) )
   if(as_rows){ summary_df <- t(summary_df) }
}

round.attr(summary_df)  <- round
attr(summary_df,"head") <- head
attr(summary_df,"transpose") <- as_rows & !apply

attr(summary_df,"yields") = yields

if( nrow(cbg$counts)>2 ){
   newMessage(summary_df) <- Message("UnevenHorizon",
"Horizon is uneven for reference portfolio i.e. counts and amounts decreases across time
while in the summary they are given only for time = 1.")
}

class(summary_df) <- union( "summary.curves_by_groups" , class(summary_df) )

summary_df

}  ##----END----##
########################################################################################################################—•°

########################################################################################################################—•°
print.summary.curves_by_groups = function( scbg  ## object of class "summary.curves_by_groups"
                                         #, messages = coalesce(getOption("messages"),TRUE)
                                         , head = NULL     ## method for "summary.curves_by_groups"
                                         , round = 3       ## tm
                                         , as_rows = NULL  ## tm (transpose)
                                         , ...             ## tm
                                 ) {
###################################################################################################—•°
##
###################################################################################################—•°

cat("summary of the object of class \"curves_by_groups\"; ")
if(attr(scbg,"yields")) cat("yields:") else cat("efficiencies:")
cat("\n\n")

head = whileif(head,iftrue=NULL,ifnull=head.attr(scbg),ifnull=FALSE)
scbg <- head(scbg,head)

transpose = coalesce( as_rows , transpose(scbg) , FALSE )

class(scbg) <- setdiff(class(scbg),"summary.curves_by_groups")

printtm( scbg , transpose=transpose , round=round , ... )             ## matrix tm  time x groups

}  ##----END----##
########################################################################################################################—•°


########################################################################################################################—•°
## EXAMPLES ############################################################################################################—•°
########################################################################################################################—•°

########################################################################################################################—•°
dummy = function(){
## This is dummy function - it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## - in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
##############################################################################################—•°

 ## RELOADER —— before it works you need to source("PacksAK.R"); it's best to use EfficiencyCurves.R within pack's dir.
 loadPacksAK("EfficiencyCurves")

##############################################################################################—•°

## Examples of using print.curves_by_groups() are in the dummy() part of  curves_by_groups.R

}
########################################################################################################################—•°
rm(dummy)