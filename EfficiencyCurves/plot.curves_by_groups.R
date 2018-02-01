########################################################################################################################—•°
## FUNCTIONS HERE
##  plot.curves_by_groups()
##
## DEPENDENCIES
##  parGraphical.r
##    coldef()
##    marginsMy()
##  #lengthen.R
##  #  adjust()
##
## TODO
##
########################################################################################################################—•°

########################################################################################################################—•°
plot.curves_by_groups = function( cbg
                           , groups = NULL
                           , what = NULL # "cumulated" , "normal"
                           , width = 8 , height = width    ## what.nr  defined in the body
                           , coldef = "black"
                           , legend_pos = c("topright","bottomright")
                           , add = NULL ## "counts" "amounts"
                           , ... ) {
########################################################################################################################—•°
## Plot method for curves_by_groups (the result of a function curves_by_groups()).
## Arguments
##  cbg            object of class curves_by_groups -- a result of the function curves_by_groups()
##  groups         groups for which we'd like to ploty curves; numeric position in curves matrix or names of groups;
##                 if NULL then all curves will be plotted;
##  what           one of "cumulated", "normal"; if left NULL (default) both types will be plotted.
##  width, height  width and height of a graphical window (parameters of windows()); default of the 'width' is 8 (inches)
##                 and default of the 'height' is equal to 'width';
##                 #old# the height of the final window is height * number of plots to draw resulting from the value of 'what'.
##  coldef         default is "black"; other possible values are "white" (equivalent to "") and "grey".
##  legend_pos     one of "topright", "topleft", "bottomright", "bottomleft" or
##                 TRUE (then "topright"), FALSE or NULL —— then there will be no legend;
##  add            one of "counts", "amounts" or NULL (default);
##                 the chosen values will be plotted with y-axis on the right but only if they vary in time i.e. if
##                 cbg was calculated with varing horizon;
##  ...            arguments passed to plot().
##
##! WARNING: By default plot (window) is a square, height = width,
## BUT it is set up somwhere (where???) that
##   height cannot exceed 11 inches (approx)
##   width  cannot exceed 18
## (on the 24' FHD screen at least)
##! and if some of these parameters exceeds their limit then the plot is rescaled with RATIO MAINTAINED!!!
## This makes things very strange !!!
########################################################################################################################—•°

ngr0 <- ncol(cbg$curves)
if(coldef == "black" ){
   if(ngr0>1){
      palette(rainbow(ngr0)) ; cols0 = 1:ngr0  ; names(cols0) = colnames(cbg$curves)
   }else{
      cols0 = "yellow"
   }
   weigh.line.col = "white"
}else{
   weigh.line.col = "black"
}

if(is.null(groups)){groups=1:ncol(cbg$curves)}
cbg$curves = cbg$curves[,groups,drop=FALSE]
cols = cols0[groups]

counts  = whileif(add[1]=="counts" ,ifnull=FALSE,ifempty=FALSE,ifna=FALSE)
amounts = whileif(add[1]=="amounts",ifnull=FALSE,ifempty=FALSE,ifna=FALSE)

if( counts && dim(cbg$counts)[1]>1 ){
   cbg$counts = cbg$counts[,groups,drop=FALSE]
}else{
   counts = FALSE
}
if( amounts && dim(cbg$amounts)[1]>1 ){
   cbg$amounts = cbg$amounts[,groups,drop=FALSE]
}else{
   amounts = FALSE
}

times = nrow(cbg$curves)
ngr = ncol(cbg$curves)

what_nr = sum(c("cumulated" , "normal")%in%what)
if( what_nr == 0 ){
   what = c("cumulated" , "normal") ; what_nr = 2
}

## legend_pos
if(is.null(legend_pos)){
   do_legend = c(FALSE,FALSE)
}else{
   legend_pos <- rep(legend_pos,length=2)    ## adjust(legend_pos,2)   ## =
   if(is.logical(legend_pos)){
      do_legend <- legend_pos
      legend_pos <- c("topright","topleft")
   }else if(is.character(legend_pos)){
      do_legend = legend_pos %in% c("topright","topleft","bottomright","bottomleft")
      if(any(!do_legend)){warning("'legend_pos' must be one of \"topright\",\"topleft\",\"bottomright\",\"bottomleft\"
   or logical or NULL.")}
   }
}

windows(width,height);
coldef(coldef) ; marginsMy() ; par( mfrow = c(what_nr,1) )
if( counts || amounts ){ par(oma=c(0,0,1,1)) }else{ par(oma=c(0,0,1,0)) }

if("normal"%in%what){
   matplot(   1:times , cbg$curves , col = cols , type = "l" , lty = 1
            , ylim = c( 0 , max(cbind(cbg$curves,cbg$net_curve))*1.1 )
            , xlab = "time"  ,  ylab = "efficiency"  #)
            , ...
          )
   lines ( 1:times , cbg$net_curve , col = weigh.line.col , lwd = 2 )
   if(do_legend[1]){
      legend( legend_pos[1] , lty = 1 , lwd = c(2,rep(1,ngr)) , col = c(weigh.line.col,cols)
             , legend = c("net_curve",colnames(cbg$curves)) )
   }
   title("Efficiency curves by groups")
   if(counts){
      par(new=TRUE)
      matplot(   1:times , cbg$counts , axes=F, xlab=NA, ylab=NA, col = cols , type = "p" , pch = "-" , cex = 1
                 , ylim = c( 0 , max(cbg$counts)*1.1 )
               )
      axis(side = 4)
      mtext(side = 4, line = 1, 'counts', col="grey")
   }
   if(amounts){
      par(new=TRUE)
      matplot(   1:times , cbg$amounts , axes=F, xlab=NA, ylab=NA, col = cols , type = "p" , pch = "-" , cex = 1
                 , ylim = c( 0 , max(cbg$amounts)*1.1 )
               )
      axis(side = 4)
      mtext(side = 4, line = 1, 'amounts', col="grey")
   }
}

if("cumulated"%in%what){
   cumsum.curves = apply(cbg$curves,2,cumsum)
   matplot(   1:times , cumsum.curves , col = cols , type = "l" , lty = 1
            , ylim = c( 0 , max(cumsum.curves)*1.1 )
            , xlab = "time"  ,  ylab = "cumulated efficiency"
            , ...
          )
   lines ( 1:times , cumsum(cbg$net_curve) , col = weigh.line.col , lwd = 2 )
   if(do_legend[2]){
      legend( legend_pos[2] , lty = 1 , lwd = c(2,rep(1,ngr)) , col = c(weigh.line.col,cols)
             , legend = c("net_curve",colnames(cbg$curves)) )
   }
   title("Cumulated efficiency curves by groups")
}

mtext("Efficiency curves",outer=TRUE)

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

## Examples of using plot.curves_by_groups() are in the dummy() part of  curves_by_groups.R

}
########################################################################################################################—•°
rm(dummy)