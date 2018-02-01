########################################################################################################################—•°
## FUNCTIONS HERE    {EfficiencyCurves}
##  head.summary.curves_by_groups()
##  head.summary.curves_and_prediction()
##
## DEPENDENCIES
##  whileif()     {DM}
##  head.attr()     {DM}
##
## TODO
##
########################################################################################################################—•°

########################################################################################################################—•°
head.summary.curves_by_groups <- function( scbg , head = NULL ){
###################################################################################################—•°
##
###################################################################################################—•°

   head = whileif(head,iftrue=NULL,ifnull=head.attr(scbg),ifnull=6,iffalse=NULL)

   if(!is.null(head)){

      attrs <- attributes(scbg)
      attrs$dim <- NULL
      attrs$dimnames <- NULL

      if(names(dimnames(scbg))[1]=="time"){
         scbg <- scbg[ 1:(5+max(0,min(head,nrow(scbg)-5))) , ]
      }else{
         scbg <- scbg[ , 1:(5+max(0,min(head,ncol(scbg)-5))) ]
      }

      attributes(scbg)[names(attrs)] <- attrs
   }

   #head.attr(scbg) <- head
   scbg

}  ##----END----##
########################################################################################################################—•°


########################################################################################################################—•°
head.summary.curves_and_prediction <- function( scap , head = NULL ){
###################################################################################################—•°
##
###################################################################################################—•°

   head = whileif(head,iftrue=NULL,ifnull=head.attr(scap),ifnull=6,iffalse=NULL)

   if(!is.null(head)){

      attrs <- attributes(scap)
      attrs$dim <- NULL
      attrs$dimnames <- NULL

      if(names(dimnames(scap))[1]=="time"){
         scap <- scap[ 1:(8+max(0,min(head,nrow(scap)-8))) , ]
      }else{
         scap <- scap[ , 1:(8+max(0,min(head,ncol(scap)-8))) ]
      }

      attributes(scap)[names(attrs)] <- attrs
   }

   #head.attr(scap) <- head
   scap

}  ##----END----##
########################################################################################################################—•°

########################################################################################################################—•°
## EXAMPLES ############################################################################################################—•°
########################################################################################################################—•°

########################################################################################################################—•°
dummy = function(){
## This is dummy function — it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## — in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
###################################################################################################—•°

 ## RELOADER — before it works you need to source("PacksAK.R"); it's best to use EfficiencyCurves.R within pack's dir.
 loadPacksAK("EfficiencyCurves")


};rm(dummy)
########################################################################################################################—•°