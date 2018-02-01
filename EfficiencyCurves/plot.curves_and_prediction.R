########################################################################################################################•°
## FUNCTIONS HERE
##  plot.curves_and_prediction()
##
## DEPENDENCIES
##  parGraphical.r
##    coldef()
##    marginsMy()
##
## TODO
##
########################################################################################################################•°

########################################################################################################################•°
plot.curves_and_prediction = function( cap
                           , what = NULL # "cumulated" , "normal"
                           , width = 8 , height = what_nr*width    ## what.nr  defined in the body
                           , coldef = "black"
                           , ... ) {
########################################################################################################################•°
## Plot method for curves_and_prediction (the result of a function curves_and_prediction()).
## Arguments
##  cap            object of class curves_and_prediction -- a result of the function curves_and_prediction()
##  what           one of "cumulated", "normal"; if left NULL (default) both types will be plotted.
##  width, height  width and height of a graphical window (parameters of windows()); default of the 'width' is 8 (inches)
##                 and default of the 'height' is what_nr*width  where 'what_nr' is a number of plots to draw
##                 resulting from the value of 'what'.
##  coldef         default is "black"; other possible values are "white" (equivalent to "") and "grey".
##  ...            arguments passed to plot().
########################################################################################################################•°

times = nrow(cap$curves)
ngr = ncol(cap$curves)

what_nr = sum(c("cumulated" , "normal")%in%what)
if( what_nr == 0 ){
   what = c("cumulated" , "normal") ; what_nr = 2
}


windows(width,height); coldef(coldef) ; marginsMy() ; par( mfrow = c(what_nr,1),oma=c(0,0,1,0) )
if(coldef == "black" )
{ palette(rainbow(ngr))
  weigh.line.col = "white"
}else{ weigh.line.col = "black" }

if("normal"%in%what){
   matplot(   1:times , cap$curves , col = 1:ngr , type = "l" , lty = 1
            , ylim = c( 0 , max(cap$curves)*1.1 )
            , xlab = "time"  ,  ylab = "efficiency"  #)
            , ...
          )
   lines ( 1:times , cap$weighted_curve , col = weigh.line.col , lwd = 2 )
   legend( "topright", lty = 1 , lwd = c(2,rep(1,ngr)) , col = c(weigh.line.col,1:ngr)
          , legend = c("weighted_curve",rownames(cap$curves)) )
   title("Efficiency curves by groups")
}

if("cumulated"%in%what){
   cumsum.curves = apply(cap$curves,2,cumsum)
   matplot(   1:times , cumsum.curves , col = 1:ngr , type = "l" , lty = 1
            , ylim = c( 0 , max(cumsum.curves)*1.1 )
            , xlab = "time"  ,  ylab = "cumulated efficiency"
            , ...
          )
   lines ( 1:times , cumsum(cap$weighted_curve) , col = weigh.line.col , lwd = 2 )
   legend( "topleft" , lty = 1 , lwd = c(2,rep(1,ngr)) , col = c(weigh.line.col,1:ngr)
          , legend = c("weighted_curve",rownames(cap$curves)) )
   title("Cumulated efficiency curves by groups")
}

mtext("Efficiency curves",outer=TRUE)

}