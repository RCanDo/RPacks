## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE
##  plot.variable()
##
## DEPENDENCIES
##
## ---------------------------------------------------------------------------------------------------------------------—•°
plot.variable = function( variable
		, as.factor = FALSE
      , FUN = function(x)x
      , what = c("hist", "ecdf", "density", "cloud", "bars", "cvs")
      , pch = "." , cex = 1
      , coldef = "black"
      , breaks = NULL         ## breaks for histogram or bar plot
      , breaks.lines = FALSE  ## draw break lines on plots of ecdf, density, cloud
      , xlim = NULL  
      , xlab = NULL
      , main = NULL
      , title = NULL
      , col = NULL            ## colors for points in "cloud" plot
      , layout = NULL
      , horizontal = FALSE
		, plot.width = 3        ## width of each (sub)plot  (not a whole window)
		, plot.height = 3       ## height of each (sub)plot (")
      , user.window = FALSE
		, cvs.covariate = NULL
      , idx = NULL            ## indices of variable and cvs.covariate; if NULL then idx = 1:length(variable)
      , envir = 1
      , inherits = TRUE
)
{
## ---------------------------------------------------------------------------------------------------------------------—•°
## Basic plots for the given variable at one window with common scale and value breaks;
## mainly in order to localize extreme observations.
## If however  user.window = TRUE  then all plots will be drawn on externally defined window
## (supposedly by the user) what allows e.g. to draw plots of few variables within one window.
##
## Arguments:
##  variable              variable vector to be plotted;
##  as.factor = FALSE     if TRUE then numeric variable will be coerced to factor, and displayed as such;
##                        it is convienient option if variable is passed to function by its name
##								  and we want to plot it as factor;
##                        default is FALSE;
##                        may be also numeric - then variable is coerced to facator if  length(unique(variable)) <= as.factor
##                        Notice that 'as.factor' is applied before FUN.
##                        Moreover, you may also turn the 'variable' into a factor via FUN writing as.facator(x) within the body of FUN.
##  FUN = function(x)x    variable may be transformed before plotting with the function FUN;
##                        Notice that 'as.factor' is applied before FUN.
##  what = c("hist", "ecdf", "density", "cloud", "bars", "cvs")     
##                        types of plot:
##           "hist"       histogram
##           "bars"       barplot
##           "ecdf"       distribution (estimated cumulative distribution function)
##           "density"    density
##           "cvs"        counts versus sums (within groups determined by breaks)
##           "cloud"      cloud, i.e.  id.number versus value of the variable
##  pch = "."             point character used in plot (the same as in plot())
##  cex = 1               point size  (the same as in plot())
##  coldef = "black"      default is "black" what gives the plot black background, yellow lines, white text;
##                        coldef = "" will leave white background and black lines and text;
##                        the same will be obtained by coldef = "white"
##                        coldef = "grey" gives grey background;
##  breaks = NULL         vector of breaks for histogram or bar plot -- n+1 limits of n (adjecent) intervals;
##  breaks.lines = FALSE  draw break lines on plots of ecdf, density, cloud;
##  xlim = NULL           limits of x axis; if NULL then set up automatically;
##  xlab = NULL           title of x axis; if NULL then set up automatically;
##  main = NULL           main title of the plot; if NULL then set up automatically;
##  title = NULL          title of a sub-plot (description at top of each figure); if NULL then the value of 'what' parameter;
##  col = NULL            colors of points in "cloud" plot;
##                        if NULL (default) then col = par()$fg;
##                        allows to distinguish between groups of observations;
##  layout = NULL         integer vector indicating nr of rows and columns in the plot 
##                        -- if not of length 2 and/or not integer then neglected 
##                        and then plots are organized automaticaly;
##  horizontal = FALSE    if number of plots is 2, 3 or 5 should they be arranged horizontaly or vertically? 
##                        ignored when 'layout' is properly set up.
##  plot.width = 3        width of each (sub)plot in inches (NOT a whole window)
##  plot.height = 3       height of each (sub)plot in inches (")
##  user.window = FALSE   as described above.
##  cvs.covariate = NULL  name or string indicating a covariate to be summed up within groups defined by 'breaks' applied to 'variable'
##								  for the "cvs" plot;
##								  if left NULL (default) then 'variable' is summed up, what is rarely sensible;
##                        notice that if no breaks are given ( breaks = NULL) then they are calculated automatically by hist()
##								  for which "Sturges" breaks are default;
##  idx = NULL            indices of variable and cvs.covariate to be used as a common selection of theirs values;
##                        if NULL then idx = 1:length(variable)
##  envir = 1             argument passed to get( , envir ) thus relevant only when variable to plot is passed as character;
##  inherits = TRUE       argument passed to get( , inherits ) thus relevant only when variable to plot is passed as character;
##
##  If the variable is a factor then only bar plot is drawn 
##  and all options are ignored except 
##  cex           which then stands for the scaling of levels labels displayed at the bottom of each bar;
##  user.window   as described above.
##
## No value returned.
## ---------------------------------------------------------------------------------------------------------------------—•°

if(is.character(variable)){
   var.name = variable
   variable = get(variable, envir=if(is.numeric(envir)){as.environment(envir)}else{envir}, inherits=TRUE)
}else{
   var.name = if(!missing(variable)){deparse(substitute(variable))}
}

if(is.null(idx)){idx = 1:length(variable)}
variable = variable[idx]  #!#!#!

is.na.variable = is.na(variable)     ## will be needed later in case of  cvs.covariate  not null
if("cloud" %in% what && !is.null(col) && length(variable)==length(col)){col = col[!is.na.variable]}  ## 'col' is used only for 'cloud' plot
variable = variable[!is.na.variable]


if( is.null(xlab)){ xlab = var.name }

## if(length(unique(variable))==1){    stop("Variable '", var.name, "' has only one value and it is not sensible to draw its plot.") }

if( is.numeric(as.factor) ){
  as.factor = as.factor >= length(unique(variable))
}

if( is.logical(as.factor) ){
  if( as.factor & !is.factor(variable) ){ variable = as.factor(variable) }
}

if(! (is.factor(variable)|is.numeric(variable)) ){
  variable = as.factor(variable)
  warning("Variable '", var.name, "' is neither numeric nor factor and it is coerced to factor.
  It is sensible for class 'character' but in case of 'date' or 'time' it may be not what you have expected.
  Nonetheless date/time classes are not supported yet. To be repaired soon!")
}


variable = FUN(variable)
 var.tab = NULL


if( is.factor(variable) ){
## ___factor___---  

  if(is.null(main)){ main = var.name }

  plot.fac = function(variable, xlab, cex, main){ #, pch, breaks, breaks.lines, xlim=xlim){
	#      if(length(unique(variable))==1){  stop("Variable '", var.name, "' has only one value and it is not sensible to draw its plot.") }
		##
      b = length(levels(variable))
      if(b>1024){variable = variable[sample(1:b, 1024)]; b=1024}
      palette( rainbow( max(b, 2) ) )
      plot( variable #table(variable)  #, freq=TRUE
              #, names.arg = 1:b #, las = 2
              , xlab=xlab       #, pch = pch , cex = cex
              , ylab = "counts"
              , col=b:1
              , cex.names = cex
              , space=0
              #, xlim=xlim
              )
      title(main)
      palette("default")

  }

  if(!user.window){
    b = length(levels(variable))
    old = par()
    windows(height=4, width=max(b*.1, 4) )
    marginsMy();
    coldef(coldef)  
  }
  
  plot.fac(variable, var.name, cex, main)


}else{
## ___numeric___---

  n=length(variable)
  lw=length(what)
  # print(var.name)
  

  if(is.null(breaks)){
      breaks = "Sturges"      ## "Sturges" are default breaks for hist()
  }else if(is.numeric(breaks) & length(breaks)>1){
      rv = range(variable)
      if(min(breaks)>rv[1]){ breaks = c(rv[1], breaks) }
      if(max(breaks)<rv[2]){ breaks = c(breaks, rv[2]) }
  }

  if(!(is.numeric(breaks) & length(breaks)>1)){
    breaks = hist(variable, plot=FALSE, breaks=breaks)$breaks
  }

  if(is.null(xlim)){xlim = range(variable)}
  
  if(!user.window){
  
    if( length(layout) >= 2 & is.numeric(layout) ){
      h = layout[1]; w = layout[2];
      what = what[ 1:min(lw, h*w) ] 
      lw=length(what)
    }else if(lw==0){ stop("Option 'what' is empty -- no type of plot selected. 
      Possible values of 'what' are \"hist\",\"ecdf\",\"density\",\"cloud\",\"bars\",\"cvs\". 
      You may select few of them in the character vector.")
    }else if(lw==1){                h = 1 ; w = 1
    }else if(lw==2){ if(horizontal){  h = 1 ; w = 2 }else{  h = 2 ; w = 1 }
    }else if(lw==3){ if(horizontal){  h = 1 ; w = 3 }else{  h = 3 ; w = 1 }
    }else if(lw==4){                  h = 2 ; w = 2
    }else if(lw>=5){ if(horizontal){  h = 2 ; w = 3 }else{  h = 3 ; w = 2 } }
  
    old = par()
    windows(height=plot.height*h+1, width=plot.width*w+1)
    marginsMy();
    coldef(coldef) ## coldef("black")
    par(mfrow = c(h, w), oma=c(0, 0, 1.5, 0))
  
  }

  if(any(c("hist", "bars")%in%what)){
      var.tab <- table(cut(variable, breaks, include.lowest = TRUE))   ## to be printed
  }
 ## print(var.tab)
  
  plot.hist = function(variable, xlab, pch, cex, breaks, breaks.lines, xlim=xlim, col=col){
		#      if(length(unique(variable))==1){  stop("Variable '", var.name, "' has only one value and it is not sensible to draw its plot.") }
      b = length(breaks)-1
      palette(rainbow(max(b, 2)))
      hist(variable, freq=TRUE , xlab=xlab , pch = pch , cex = cex, col=b:1, breaks=breaks
           , main = "histogram")
      palette("default")
  }
  
  plot.bars = function(variable, xlab, pch, cex, breaks, breaks.lines, xlim=xlim, col=col){
		#      if(length(unique(variable))==1){  stop("Variable '", var.name, "' has only one value and it is not sensible to draw its plot.") }
      b = length(breaks)-1
      palette(rainbow(max(b, 2)))
      barplot( var.tab , space=0 #, freq=TRUE
              , names.arg = 1:length(breaks[-1]) #, las = 2
              , xlab=xlab       #, pch = pch , cex = cex
              , ylab = "counts"
              , col=b:1
              #, xlim=xlim
              )
      title("bars")
      palette("default")
  }
   

	############################################################################################
	## covariate variable for sums in plot.cvs -- relevant only if "cvs" is in the 'what' vector
	if(!is.null(cvs.covariate)){

		if(is.character(cvs.covariate)){
		   cvs.covar.name = cvs.covariate
		   cvs.covariate = get(cvs.covariate)
		}else{
		   cvs.covar.name = if(!missing(cvs.covariate)){deparse(substitute(cvs.covariate))}
		}

      cvs.covariate = cvs.covariate[idx]  #!#!#!

		if(length(cvs.covariate)==length(is.na.variable)){
			cvs.covariate = cvs.covariate[!is.na.variable]  #!!! OK !!!!
	      cvs.covariate.mark <- TRUE
		}else{
			cvs.covariate.mark <- FALSE
			warning("'cvs.covarate' is not of the same length as 'variable' hence it is neglected.")
		}

	}else{cvs.covariate.mark <- FALSE}

	############################################################################################
  plot.cvs = function(variable, xlab, pch, cex, breaks, breaks.lines, xlim=xlim, col=col){
  ## counts versus sum within groups
   	#      if(length(unique(variable))==1){  stop("Variable '", var.name, "' has only one value and it is not sensible to draw its plot.") }
      b = length(breaks)-1
      palette(rainbow(max(b, 2)))
		if(cvs.covariate.mark){
			x.name = cvs.covar.name
			##
			idx.na.covariate = is.na(cvs.covariate)
				  variable = variable[!idx.na.covariate]
         cvs.covariate = cvs.covariate[!idx.na.covariate]
			##
	      	breaks.var = cut(variable, breaks, include.lowest = TRUE)
	      	var.tab = table(breaks.var)
			sum.tab = tapply(cvs.covariate, breaks.var, sum)
	      	sum.tab[is.na(sum.tab)] = 0
		}else{
			x.name = var.name
			##
	      	breaks.var = cut(variable, breaks, include.lowest = TRUE)
	      	var.tab = table(breaks.var)
	      sum.tab = tapply(variable, breaks.var, sum)
	      	sum.tab[is.na(sum.tab)] = 0
		}
		##
     plot( as.numeric(var.tab)~as.numeric(sum.tab), xlab= paste0("sum of ", x.name, " within group")
              , pch = 15 , cex = 2*cex
              , ylab = "counts"
              , col = b:1
              , xlim = c(0, max(as.numeric(sum.tab))*1.05)
              , ylim = c(0, max(as.numeric(var.tab))*1.05)
           )
     #text(as.numeric(var.tab)~as.numeric(sum.tab), labels=1:b)
     ##
     if(is.null(title)){title = "counts ~ sum"}  ;  graphics::title(title)
     ##
     palette("default")
     # abline(h=c(0, 1), col="gray")
     # if(breaks.lines){ abline(v=breaks, lty=3, col="grey")  }
  }
  
  plot.ecdf = function(variable, xlab, pch, cex, breaks, breaks.lines, xlim=xlim, col=col){
	  #      if(length(unique(variable))==1){  stop("Variable '", var.name, "' has only one value and it is not sensible to draw its plot.") }
     n = length(variable)
     yy = (1:n)/n
     sort.v = sort(variable)
     plot(yy~sort.v, type="n" , xlab=xlab , pch = pch , cex = cex, ylab = "probability", xlim=xlim)
     ##
     if(is.null(title)){title = "distribution"}  ;  graphics::title(title)
     ##
      b = length(breaks)-1
      palette(rainbow(max(b, 2)))
     abline(h=c(0, 1), col="gray")
     if(breaks.lines){ abline(v=breaks[-1], lty=3, col=b:1) ; abline(v=breaks[1], lty=3, col="darkgrey") }
        palette("default")
     points(yy~sort.v, xlab=xlab , pch = pch , cex = cex, ylab = "probability", xlim=xlim)
  }
  
  plot.density = function(variable, xlab, pch, cex, breaks, breaks.lines, xlim=xlim, col=col){
    if(length(unique(variable))==1){
			warning("Variable '", var.name, "' has only one value and it is not sensible to draw its plot.")
			plot(variable, type = "n");
		}else{
		  d.v = density(variable)
        ##
	     plot(d.v$y ~ d.v$x , type="n" , xlab=xlab , ylab = "density", xlim=xlim)
        ##
	      b = length(breaks)-1
	      palette(rainbow(b))
	      if(breaks.lines){ abline(v=breaks[-1], lty=3, col=b:1) ; abline(v=breaks[1], lty=3, col="darkgrey") }
	         palette("default")
	     lines(d.v$y ~ d.v$x ,  xlab=xlab , ylab = "density", xlim=xlim)
		}
      if(is.null(title)){title = "density"}  ;  graphics::title(title)
  }
  
  plot.cloud = function(variable, xlab, pch, cex, breaks, breaks.lines, xlim=xlim, col=col){
     #      if(length(unique(variable))==1){  stop("Variable '", var.name, "' has only one value and it is not sensible to draw its plot.") }
     plot( 1:length(variable) ~ variable, type = "n"
           , xlab=xlab , pch = pch , cex = cex, ylab = "ID number", xlim=xlim)
     ##
     if(is.null(title)){title = "cloud"}  ;  graphics::title(title)
     ##
     if(is.null(col)){ col = par()$fg }
      b = length(breaks)-1
            ###cat(breaks, "\n", b, "\n")
      palette(rainbow(max(b, 2)))
      if(breaks.lines){ abline(v=breaks[-1], col=b:1) ; abline(v=breaks[1], col="darkgrey")}
         palette("default")
     points( 1:length(variable) ~ variable, xlab=xlab , pch = pch , cex = cex
            , ylab = "ID number", xlim=xlim , col = col)#[!is.na(variable)])
  }
  
  what = paste0("plot.", what)
  
  l = quote( funkcja( variable
                          , xlab = xlab
                          , pch = pch
                          , cex = cex
                          , breaks = breaks
                          , breaks.lines = breaks.lines
                          , xlim = xlim
                          , col = col) )
  for(wh in what){  l[[1]] = as.name(wh);  eval(l)  }
  
  if(is.null(main)){ main = var.name }
  mtext(main, outer=TRUE)

}

if(!is.null(breaks))cat("breaks = c(", paste(breaks, collapse=", "), ")", sep="");cat("\n")
if(!is.null(var.tab))print(var.tab)

if(!user.window){ suppressWarnings(par(old)) }

}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°


## ---------------------------------------------------------------------------------------------------------------------—•°
## EXAMPLES ------------------------------------------------------------------------------------------------------------—•°
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
dummy = function(){
## This is dummy function — it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## — in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
## -------------------------------------------------------------------------------------------------—•°
## RELOADER — before it works you need to source("RCanDo.R"); it's best to use {package_name}.R within pack's dir.
 loadPacksAK("Variable")
## -------------------------------------------------------------------------------------------------—•°


# 1 histogram
   #hist.all = hist(X, plot = FALSE)
#   hist.list = lapply( X.gbs, function(x){ hist(x, breaks = hist.all$breaks, plot = FALSE)$counts} )
#   barplot( rbind(hist.all$counts, hist.list$`1`, hist.list$`0` ),
#            beside = TRUE,
#            col = c("darkgrey", "darkgreen", "red"),
#            names.arg = signif(hist.all$breaks[-length(hist.all$breaks)], 3),
#            xlab=var.names[1],
#            ylab = "counts")
#   legend("topright", legend=c("all", "goods", "bads"), col = c("darkgrey", "darkgreen", "red"), pch=15)
#   title(paste0(var.names[1], " -- histogram"))

## distribution function
#plot(1:n ~ variable , pch = ".")
#   # 2
#   n = length(X);  d = (1:n)/n
#   v1 = sort(X.gbs$`1`); n1 = length(v1); d1 = (1:n1)/n1
#   v0 = sort(X.gbs$`0`); n0 = length(v0); d0 = (1:n0)/n0
#
#
#   plot( d~sort(X), col="grey", pch=".", xlab=var.names[1], ylab="distribution")
#   lines(d1~v1, col="darkgreen", pch=".")
#   lines(d0~v0, col="red", pch=".")
#   abline(h=c(0, 1), col="darkgrey")
#   title(paste0(var.names[1], " ~ ", var.names[2]))
#
#     # 3
#      xx = order(X)
#   plot(CzyDobra~X, pch=".", xlab=var.names[1], ylab=var.names[2])
#   lines(predict(mod.l, type="response")[xx] ~ X[xx], col=5, lwd=2)
#   lines(predict(mod.a, type="response")[xx] ~ X[xx], col=6)
#   title(paste0(var.names[2], " ~ ", var.names[1]))
#
#   # 4
#   plot(skutecznosc~X, xlab=var.names[1], ylab=var.names[3], pch=pch , cex = cex);
#   abline(h=c(0, 1), col="grey")
#   lines(predict(mod.lsf, type="response")[xx] ~ X[xx], col=5, lwd=2)
#   lines(predict(mod.asf, type="response")[xx] ~ X[xx], col=6)
#   title(paste0(var.names[3], " ~ ", var.names[1]))
#


## ---------------------------------------------------------------------------------------------------------------------—•°
}; rm(dummy)

