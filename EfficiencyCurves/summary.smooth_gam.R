## ---------------------------------------------------------------------------------------------------------------------???
## summary() and print() methods for "smooth_gam"
## ---------------------------------------------------------------------------------------------------------------------???
## FUNCTIONS HERE    {EfficienyCurves}
##  print.smooth_gam()
##  summary.smooth_gam()
##  print.summary.smooth_gam()
##
## DEPENDENCIES
##  coalesce()    {DM}
##  messages()    {DM}
##  indentr()     {DM}
##  printtm()     {DM}
##  efficiency()  {.} a-misc.r
##
## REMARKS/WARNINGS
##
## TODO
## ?
##
## DESCRIPTION
##  The aim, the idea, problems, solutions... briefly!, concerning the whole set of functions
##
## -------------------------------------------------------------------------------------------------???
## AUTHOR: Arkadiusz Kasprzyk; akasp@...
## ver. 1.0
## PATH: D:\ROBOCZY\...
## start: 2017-03-01    last: 2017-10-08
## ---------------------------------------------------------------------------------------------------------------------???

## ---------------------------------------------------------------------------------------------------------------------???
print.smooth_gam = function( sg  ## object of class "smooth_gam"
                           #, messages = coalesce(getOption("messages"),TRUE)
                           , round = 3
                           , as_rows = FALSE
                           , round.attr = TRUE
                           , ...
                           ) {
## ------------------------------------------------------------------------------------------------???
##
## ------------------------------------------------------------------------------------------------???
cat("object of class \"",paste(class(sg),collapse="\", \""),"\"\n",sep="")      # ???

indentr( unclass(sg) , vsep=1 , add="models_list"
       , transpose=as_rows , margins=NULL , comment=TRUE , round=round , round.attr=round.attr , ... )
         ## passed to printtm() for elements inheriting from "tm"; but 2 first elements are not of this class;
         ## this is no problem as print() methods ignore irrelevant arguments! GREAT!

}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------???
dummy = function(){

sgcbg[]
indentr(sgcbg,add="models_list",vsep=1)

indent(sgcbg$models_list)

};rm(dummy)


## ---------------------------------------------------------------------------------------------------------------------???
summary.smooth_gam = function( sg  ## object of class "smooth_gam"
                           , as_rows = FALSE
                           , messages = TRUE
                           , round = NULL
                           ) {
## ---------------------------------------------------------------------------------------------------------------------???

colsums <- function(x){ if(is.null(dim(x))){x}else{colSums(x)} }

basic_args <- structure( list( future   = sg$future
                             , horizon  = sg$horizon
                             , method   = sg$arguments$method
                             , lengthen = sg$lengthen
                             , wiggliness = sg$arguments$wiggliness
                             )
                       , class = "arguments"
                       )

group_sums.df <- data.frame(
     counts = { cs <- colsums(sg$counts) ;
                if( sg$arguments$net_curve ) cs <- c( cs , net_curve = sum(cs)) ;
                cs }
   , trials = colsums(sg$trials)
   , successes = colsums(sg$successes)
   )


newMessage(group_sums.df) <- Message("counts","Sums for groups - total number of counts for each group.")
newMessage(group_sums.df) <- Message("trials","Sums for groups - total number of trials for each group.")
newMessage(group_sums.df) <- Message("successes","Sums for groups - total number of successes for each group.")

messages.df <- messages(group_sums.df)

if(!is.null(sg$weights_groups)){
  group_sums.df <- rbind( group_sums.df , weighted_curve=c(NA,NA,NA) )
}

group_sums.df <- cbind( group_sums.df
   , efficiencies = colsums(sg$efficiencies[,rownames(group_sums.df)])  #!#! this is exception, the only matrix where
                           ##! the 'Sum' (in case  net_curve=F)  or  'net_curve' (in case  net_curve=T ? default)
                           ##! is fixed column not a tm() derivative;  What's the reason behind it?!
                           ##! Because 'Sum' in 'efficiencies' (or 'net_curve') is NOT a sum of rows (or columns) BUT
                           ##! efficiencies derived from sums of rows (or columns) of 'successes' and 'trials'.
                           ##! Hence tm() couldn't be applied.
   #, weights_groups = colsums(sg$weights_groups)    ## may be NULL
   , smoothed_curves = colsums(sg$smoothed_curves)      ## [,rownames(group_cums.df)]
   )


newMessage(messages.df) <- Message("efficiencies","Sums for groups - total efficiency for each group.")
newMessage(messages.df) <- Message("smoothed_curves","Sums for groups - total efficiency from smoothed curves for each group.")


if(!is.null(sg$arguments$lengthen)){
   group_sums.df <- cbind( group_sums.df
                         , smoothed_curves_len = colsums(sg$smoothed_curves_len)
                         )
                         ## messages are lost!
   ##
   newMessage(messages.df) <- Message( "smoothed_curves_len"
                           , "Sums for groups - total efficiency from smoothed and lengthened curves for each group." )
}

if(sg$arguments$method == "cumulated"){
   group_sums.df <- cbind( group_sums.df
                         ##, successes_cum  = tail(sg$successes_cum,1)   ## this is the same as  group_sums.df$successes  above
                         , smoothed_curves_cum = as.vector(tail(sg$smoothed_curves_cum,1))
                         )
   ##
   ## newMessage(group_sums.df) <- Message( "successes_cum" , "(last row ? equal to 'trials' if 'trials' were not passed)")
   newMessage(messages.df) <- Message( "smoothed_curves_len"
                                     , "Last row - total efficiency from smoothed curves for each group." )
}

if( !sg$arguments$net_curve ){
   cs <- colSums(group_sums.df[colnames(sg$counts),1:3])        ## only groups!
   group_sums.df <- rbind(group_sums.df, Sum = c(cs,rep(NA,ncol(group_sums.df)-3)))
}

messages(group_sums.df) <- messages.df
round.attr(group_sums.df) <- round

result <- list(
     basic_arguments = basic_args
   , models_call = sg$models_list[[1]]$call
   , models_table = models_table(sg)
   ###
   , group_sums = group_sums.df
)

if(messages){
   messages(result) <- messages(sg)
}

class(result) <- "summary.smooth_gam"
result

}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------???

## ---------------------------------------------------------------------------------------------------------------------???
print.summary.smooth_gam = function( ssg  ## object of class "summary.smooth_gam"
                           , ... ){
## ---------------------------------------------------------------------------------------------------------------------???

#      mat.names = c("trials","successes","efficiency","weights_times","weights_curves","smoothed_curves")

cat("summary of the object of class \"smooth_gam\".\n")

indentr( unclass(ssg) , vsep=1 , ... )

#cat("future: ",ssg$future,"\n")
#cat("horizon: ",ssg$horizon,"\n")
#cat("method: ",ssg$method,"\n")
#
#if(!is.null(ssg$lengthen)){
#   cat("lengthen:  ",ssg$lengthen,"\n")
#   mat.names <- c(mat.names,"smoothed_curves_len")
#}
#cat("\n")
#if(ssg$method == "cumulated"){
#   mat.names <- c(mat.names,"successes_cum","smoothed_curves_cum")
#}
#
#cat("models_list \n")
#print(ssg$models_call)
#cat("wiggliness: ",ssg$wiggliness,"\n")
#print(ssg$models_table)
#cat("\n")


#for(nam in mat.names){
#   cat(paste0(nam,"\n"))
#   if(!is.null(messages[[nam]]))cat(paste0(messages[[nam]],"\n"))
#   prints <-  ssg[[nam]]
#   print( switch( nam
#                , "trials" = sums(prints)
#                , "successes" = sums(prints)
#                , "efficiency" = sums(prints)
#                , "weights_times" = sums(prints)
#                , "weights_curves" = sums(prints)
#                , "smoothed_curves" = sums(prints)
#                , "smoothed_curves_len" = sums(prints)
#                ## cum
#                , "successes_cum" = tail(prints,1)
#                , "smoothed_curves_cum" = tail(prints,1)
#                , ""
#                )
#        )
#      ## print( class(prints))
#   cat("\n")
#}

}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------???


## ---------------------------------------------------------------------------------------------------------------------???
## EXAMPLES ############################################################################################################???
## ---------------------------------------------------------------------------------------------------------------------???

## ---------------------------------------------------------------------------------------------------------------------???
dummy = function(){
## This is dummy function - it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## - in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
## -------------------------------------------------------------------------------------------???

 ## RELOADER ?? before it works you need to source("PacksAK.R"); it's best to use EfficiencyCurves.R within pack's dir.
 loadPacksAK("EfficiencyCurves")

## -------------------------------------------------------------------------------------------???

## Examples of using print.smooth_gam() are in the Help part of  smooth_gam.R

}
## ---------------------------------------------------------------------------------------------------------------------???
rm(dummy)
