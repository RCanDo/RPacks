########################################################################################################################—•°
## plot() method for class "smooth_gam"
########################################################################################################################—•°
## FUNCTIONS HERE    {EfficienyCurves}
##  plot.smooth_gam()
##
## DEPENDENCIES
##  coldef()         {Graphics}      parGraphical.r
##  marginsMy()
##
## REMARKS/WARNINGS
##
## TODO
## •
##
## DESCRIPTION
##  The aim, the idea, problems, solutions... briefly!, concerning the whole set of functions
##
####################################################################################################—•°
## AUTHOR: Arkadiusz Kasprzyk; akasp@...
## ver. 1.0
## PATH: D:\ROBOCZY\...
## start: 2017-03-01    last: 2017-09-26
########################################################################################################################—•°

########################################################################################################################•°
plot.smooth_gam <- function( sg ## smooth_gam
      , add_points = TRUE
      , groups = NULL
      , coldef = "black"
      , ylim = NULL
      , ylim_cum = NULL
      , titles = c( "Smoothed curves via gam" , "Cumulated smoothed curves via gam" )
      , legend_pos = c("topright","bottomright")
      , ...
){
########################################################################################################################•°
##    Arguments
##  sg                object of class "smooth_gam"
##  groups            groups for which we'd like to ploty curves; numeric position in curves matrix or names of groups;
##                    if NULL then all curves will be plotted;
##  add_points        to plot points of data (succes_ratio = successes/trials) or not to plot...
##  coldef            default is "black" what gives the plot black background, yellow lines, white text;
##                    coldef = "" will leave white background and black lines and text;
##                    the same will be obtained by coldef = "white"
##                    coldef = "grey" gives grey background;
##  ylim              limits of y-axis on the plot; if left NULL (default) will be set automatically
##  ylim_cum          limits of y-axis on the plot of cumulated success ratios;
##                    if left NULL (default) will be set to c(0,1.1);
##  titles            titles for plots; character of length 2 where the first element is a title of "normal" plot
##                    and a second is for plot of "cumulated";
##  legend_pos        position of the legends; character of length 2 where the first element is a title of "normal" plot
##                    and a second is for plot of "cumulated";
##                    one of "topright", "topleft", "bottomright", "bottomleft" or
##                    TRUE (then "topright"), FALSE or NULL —— then there will be no legend;
##  ...               arguments passed to plot()
########################################################################################################################•°

nams  <- list()
cols  <- list()
nr    <- list()
bools <- list()

nams$all <- colnames(sg$smoothed_curves)
nams$nc <- "net_curve"
nams$wc <- "weighted_curve"
nr$all <- length(nams$all)
nams$gr <- setdiff(nams$all,c(nams$nc,nams$wc))
nr$gr <- length(nams$gr)

if(coldef == "black" ){
   if(nr$gr>1){
      palette(rainbow(nr$gr)) ;  cols$gr = 1:nr$gr ; names(cols$gr) <- nams$gr
   }else{
      cols$gr = "yellow"
   }
   cols$wc = "grey"
   cols$nc = "white"

}else{
   cols$wc = "darkgrey"
   cols$nc = "black"
}


if(is.null(groups) || length(groups)==0)  groups <- nams$all

bools$plot <- nams$all %in% groups
 nams$plot <- intersect(nams$all,groups)
bools$gr   <- nams$gr %in% nams$plot
 nams$gr   <- intersect(nams$gr,nams$plot)
 cols$gr   <- cols$gr[bools$gr]


nc  <- nams$nc %in% nams$plot  ##   !is.null(as.data.frame(sg$smoothed_curves)$weighted_smoothed_curve)
wc  <- nams$wc %in% nams$plot  ##   !is.null(as.data.frame(sg$smoothed_curves)$weighted_smoothed_curve)
len <- !is.null(sg$lengthen)

nr$gr <- length(nams$gr)

maxtime <- nrow(sg$successes)
times <- 1:maxtime
horizon <- nrow(sg$smoothed_curves)

ymax=0
if(add_points){
   sg$efficiencies <- sg$efficiencies[times,]
   ymax  <- max(sg$efficiencies[times,nams$plot])
}

if(is.null(ylim)){
   ymax <- max(sg$smoothed_curves[,nams$plot],ymax)
   ylim <- c( 0 , ymax*1.1 )
}

titles <- rep(titles,length=2)
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


### normal graph
windows(); coldef(coldef) ; marginsMy() #; par(mfrow = c(2,1),oma=c(0,0,1,0))

   if(nr$gr>0){
      matplot( 1:horizon , sg$smoothed_curves[,nams$gr,drop=FALSE] , col = cols$gr , type = "l" , lty = 1 + len
              , ylim = ylim
              , xlab = "time"  ,  ylab = "efficiency"
              , ...
             )
      if(len){
         matlines( 1:horizon , sg$smoothed_curves_len[,nams$gr,drop=FALSE] , col = cols$gr , lty = 1 )
      }
      if(add_points){
         matpoints(  times , sg$efficiencies[,nams$gr,drop=FALSE] , col = cols$gr , pch ="+" )
          matlines(  times , sg$efficiencies[,nams$gr,drop=FALSE] , col = cols$gr , lty = 3  )
      }
      abline(h=0,col="darkgrey")
   }else{
      plot( 1:horizon, rep(0,horizon), type="l" , ylim = ylim , xlab = "time" ,  ylab = "efficiency" , col="darkgrey")
   }


   lwd = rep(1,nr$gr) ;  col = cols$gr ;  legend = nams$gr
   if(nc){
      lines( 1:horizon , sg$smoothed_curves[,nams$nc] , col = cols$nc , lwd = 1 , lty = 1 + len )
      if(len){
         lines( 1:horizon , sg$smoothed_curves_len[,nams$nc] , col = cols$nc , lwd = 1 , lty = 1 )
      }
      if(add_points){
         points( times , sg$efficiencies[,nams$nc] , col = cols$nc , pch ="+" )
          lines( times , sg$efficiencies[,nams$nc] , col = cols$nc , lty = 3  )
      }
      lwd = c(1,lwd) ;  col = c(cols$nc,col) ;  legend = c(nams$nc,legend)
   }
   if(wc){
      lines( 1:horizon , sg$smoothed_curves[,nams$wc] , col = cols$wc , lwd = 2 , lty = 1 + len )
      if(len){
         lines( 1:horizon , sg$smoothed_curves_len[,nams$wc] , col = cols$wc , lwd = 2 , lty = 1 )
      }
      if(add_points){
         points( times , sg$efficiencies[,nams$wc] , col = cols$wc , pch ="+" )
          lines( times , sg$efficiencies[,nams$wc] , col = cols$wc , lty = 3  )
      }
      lwd = c(2,lwd) ;  col = c(cols$wc,col) ;  legend = c("weighted_curve",legend)
   }

   if(do_legend[1]){
      legend( legend_pos[1] , lty = 1 , lwd = lwd , col = col , legend = legend )
   }
   title(titles[1])

#######################################################################################
### graph for cumulated
if(sg$arguments$method=="cumulated"){

#   if(add_points){ #!#!#!#!#!#!#!
#      sg$efficiencies_cum <- sg$successes_cum/sg$trials
#      ymax  <- max(sg$efficiencies_cum)
#   }



   windows(); coldef(coldef) ; marginsMy() #; par(mfrow = c(2,1),oma=c(0,0,1,0))

   if(is.null(ylim_cum)){ ylim_cum =  c( 0 , 1.1 ) }

   if(nr$gr>0){
      matplot( 1:horizon , sg$smoothed_curves_cum[,nams$gr,drop=FALSE] , col = cols$gr , type = "l" , lty = 1
              , ylim = ylim_cum
              , xlab = "time"  ,  ylab = "efficiency"
              , ...
              )
      if(add_points){
         matpoints( times , sg$efficiencies_cum[,nams$gr,drop=FALSE] , col = cols$gr , pch="+" )
          matlines( times , sg$efficiencies_cum[,nams$gr,drop=FALSE] , col = cols$gr , lty=2 )      #, type = "l" , lty = 1
      }
      abline( h=c(0,1) , col="darkgrey" )
   }else{
      plot( 1:horizon, rep(0,horizon), type="l", ylim = ylim_cum, xlab = "time",  ylab = "efficiency", col="darkgrey" )
   }

   lwd = rep(1,nr$gr) ;  col = cols$gr ;  legend = nams$gr

   if(nc){
      lines( 1:horizon , sg$smoothed_curves_cum[,nams$nc] , col = cols$nc , lwd = 1 )
      if(add_points){
         points( times , sg$efficiencies_cum[,nams$nc] , col = cols$nc , pch ="+" )
          lines( times , sg$efficiencies_cum[,nams$nc] , col = cols$nc , lty = 3  )
      }
      lwd = c(1,lwd) ;  col = c(cols$nc,col) ;  legend = c("net_curve",legend)
   }
   if(wc){
      lines( 1:horizon , sg$smoothed_curves_cum[,nams$wc] , col = cols$wc , lwd = 3 )
      if(add_points){
         points( times , sg$efficiencies_cum[,nams$wc] , col = cols$wc , pch ="+" )
          lines( times , sg$efficiencies_cum[,nams$wc] , col = cols$wc , lty = 3  )
      }
      lwd = c(2,lwd) ;  col = c(cols$wc,col) ;  legend = c("weighted_curve",legend)
   }

   if(do_legend[2]){
      legend( legend_pos[2] , lty = 1 , lwd = lwd , col = col , legend = legend )
   }
   title(titles[2])
}


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

n=20 ; g=5 ; maxs=100
successes <- data.frame(row.names=1:n)
for(k in 1:g){ successes <- cbind(successes , rev(sort(sample(20:maxs,n,replace=TRUE))) + sample(1:20,n,replace=TRUE) + sample(-20:20,1)) }
   colnames(successes) <- paste0("g_",1:g)
   successes

sg <- smooth_gam( successes  ## vector or matrix of successes
               , trials = NULL
               , weights_curves = c(1,1,1,1,1)
               , weights_times  = NULL
               , horizon = 100
               #, lengthen = function(x){x[1]*.95}
               , wiggliness = NULL
               #, method =  "cumulated" #"normal" ##
               )
plot(sg,FALSE)
plot(sg,FALSE,1)
plot(sg,FALSE,c(1,3,5))   ## colors for groups preserved


}
########################################################################################################################—•°
rm(dummy)