## ---------------------------------------------------------------------------------------------------------------------??
## FUNCTIONS HERE
##  plot.lm.means()  ## doda? wy?wietlanie ?redniej
##  plot.means.fit()
##  plot.means()
##
## DEPENDENCIES
##  graphical.parameters.r
##    coldef()
##    coldef.black()
##    margins.my()
##    parMy()
##
## ---------------------------------------------------------------------------------------------------------------------??
plot.lm.means = function( formula , data = NULL ,... ){
## ---------------------------------------------------------------------------------------------------------------------??
## This function is intended to plot means with confidence bars for one way ANOVA.
## ARGUMENTS
##  formula    formula for linear model which is one way ANOVA, ie. one factor regressor
##  data       data to be used to calculate the linear model;
##  ...        further arguments passed to plot.means.fit()  and then to plot.means()
## RESULT is a list having following items:
##  model      ANOVA model
##  labels     names of level of regressor taken from model or user defined labels (see par. 'labels' in plot.means.fit())
##  means      named vector of means from model; names of means may differ from 'labels'
##  SE         standard errors for each level mean
##  intervals  table of means together with left and right limit of confidence interval
##  Student's quantile         quantile of Students' distribution with proper df
##  df         degrees of freedom
##  alpha      significance level
##
## ---------------------------------------------------------------------------------------------------------------------??
if(is.null(data)){
   model = lm(formula, y=TRUE)
}else{
   model = lm(formula, y=TRUE, data)
}
result = plot.means.fit(model, mean=mean(model$y),...)
names.result = names(result)
result$model = model
result = result[c("model", names.result)]
result
} ##----END----##
## ---------------------------------------------------------------------------------------------------------------------??
## EXAMPLE
dummy = function(){

result = plot.lm.means(yy~variable.fun)

}
## ---------------------------------------------------------------------------------------------------------------------??


## ---------------------------------------------------------------------------------------------------------------------??
plot.means.fit = function(model, mean=NULL, sort = FALSE, labels=NULL,...){
## ---------------------------------------------------------------------------------------------------------------------??
## For the linear model given in 'model' with ONE factor (one-way ANOVA) it plots
## estimated means for every level of this factor together with 1-alpha confidence interval,
## in the manner similar to plot.design().
## To be improved to MULTI-WAY ANOVA but actually there's no idea...
##
##  model       object of class 'lm' or 'aov'.
##  mean        the overall mean (which sadly cannot be calculated from the model object...)
##  sort        if FALSE (default) means are not sorted. If TRUE they are sorted in increasing order.
##              You may also set it to "asc" or "desc" to indicate which oreder you want (increasing or decreasing respectively).
##  labels      user defined names for each level name; if NULL (default) then names of levels are taken from model$xlevels[[1]].
##  ...         further arguments passed to plot.means()
## ---------------------------------------------------------------------------------------------------------------------??

## ------------------------------------
means = model$coef
   n=length(means)
   means = means[1] + c(0, means[-1])
df = model$df
stderrs = summary.lm(model)$coef[, 2]

## ------------------------------------
if(!is.null(labels)){
   names(means) = labels
}else{
   names(means) <- model$xlevels[[1]] ## only first variable (one-way ANOVA)
}
ordr = 1:length(means)

## ------------------------------------
if(is.logical(sort) | is.character(sort)){
   if(is.logical(sort) && sort){
      sort = "asc"
   }

   if(is.character(sort)){
      if(!sort%in%c("asc", "desc")){
         sort = "asc"
         warning("'sort' ought to be \"asc\" or \"desc\". Now it is none of it and is set to \"asc\".")
      }

      if(sort == "asc"){
         ordr = order(means, decreasing = FALSE)
         means = sort(means, decreasing = FALSE)
      }else if(sort == "desc"){
         ordr = order(means, decreasing = TRUE)
         means = sort(means, decreasing = TRUE)
      }
   }
}else{
   stop("'sort' must be logical or character \"asc\" or \"desc\".")
}

stderrs = stderrs[ordr]
labels = names(means)

## ------------------------------------
results = plot.means(means, stderrs, df, mean, labels=labels,...)
return(results)

} ##----END----##
## ---------------------------------------------------------------------------------------------------------------------??


## ---------------------------------------------------------------------------------------------------------------------??
plot.means = function(means, stderrs, df=NULL, mean = NULL,
                      main="", labels=NULL, ylab="means", xlab="", xlas = 0,
                      text=TRUE, pos = NULL , rotate = FALSE, cex.means = 1,
                      alpha=.05,
                      type = "", wdth =.2, lwd=1, ylim = NULL,
                      windows = FALSE , height = NULL , width = NULL, coldef = "black", parMy = TRUE, ... ){
## ---------------------------------------------------------------------------------------------------------------------??
##  means       vector of means to be plotted.
##  stderrs     vector of respective standard errors. If shorter then means this will be
##              cyclically replicated to match the length of means.
#!              If the variance sigma^2 is known
##              put stderrs = sigma (a single value) and df=NULL (default).
##  df          degrees of freedom. Used to calculate the proper quantile of Students distribution.
##              Will be cyclically replicated to the length of means.
##              Default is df = NULL, and then quantile of normal distribution will be calculated.
##  mean        the value of the overall mean. May stay NULL.
##  main        title of the plot; default is "".
##  labels      labels to be given to factor levels. Default is NULL what results
#!              with labels = names(means).
##  ylab        a name of the y axis, default is "means".
##  xlab        a name of the x axis, default is "".
##  xlas        parameter las for x axis (see Graphical Parameters of R help):
##              ? 0    always parallel to the axis [default],
##              ? 1    always horizontal,
##              ? 2    always perpendicular to the axis,
##              ? 3    always vertical.
##  text        if TRUE the values of the means will be displayed.
##              If FALSE then there will be no text displayed and the parameters 'pos' and 'rotate' explained below
##              are not relevant.
##  pos         Position of the values of means to be displayed next to points on the plot
##              where this means are pinned.
##              This should be character vector of length of n = number of levels/means to be displayed.
##              We code the position in the following way:
##                      'ul' | 'ur'          up left | up right
##                      -----+-----  mean    --------+---------
##                      'dl' | 'dr'        down left | down right
##              The default (when pos==NULL) is 'ur' i.e. means values will be displayed
##              up-right wrt the means points.
##              Only one letter may be used, e.g. 'l' (left) or 'd' (down)
##              to move the text to the left or below the mean point.
##              In fact the algorithm searches only for letters 'l' and 'd',
##              thus any other letter used, including empty string '', is ignored,
##              and default displaying position is applied, i.e. 'ur'.
##              Ordering of letters is arbitrary.
##              E.g. if there are 4 means to be displayed we may put:
##              pos = c("", "d", "dl", "l") == c("ur", "dr", "dl", "ul") == c("..", "d.", "dl", "l.")
##              If the vector is not long enough then is replicated to the length n.
##              pos = c("l") == c("l", "l", "l", "l")
##              pos = c("l", "") == c("l", "", "l", "")
##
##  rotate      If TRUE then mean values labels will be rotated 90? and displayed to the right and above the
##              the mean point (pos values ignored). Default is FALSE.
##  alpha       Significance level used to calculate 1-alpha confidence intervals.
##              Default is alpha=.05.
##  cex.means   Parameter 'cex' (printed signes scaling) for printed means values (as described above).
##  type        Type of plot. Default is "" what results in "bar and whiskers" plot.
##              There is "mean bar" in the place of given mean and whiskers stretching from it
##              to the length of qt(1-alpha/2, df)*stderrs (see lines (1), (1')).
##              Whiskers are terminated by another bars.
##              If any other string will be passed then this string will be plotted
##              in the place of "mean bar"; whiskers will not be plotted
##              but only bars terminating them.
##  wdth        Length of the horizontal lines (bars). Default is wdth = .2.
##  lwd         Plotted lines width. Default is lwd=1. "Mean bar" is two times thicker (3).
##  ylim        Limits of y axis; default is NULL what results in automatic fit (2).
##  ...         further arguments passed to plot() and lines()
##
##  windows = FALSE  Do create own window? FALSE is default and then results will be plotted in currently active window
##                   or new window will open if there is no active. Then also parameters below ie. 'height', 'width',
##                   'coldef' and 'parMy' are irrelevent.
##                   If TRUE the function creates own window and all mentioned parameters are taken into account.
##  height = NULL    height of window. If NULL then is calculated automatically from length of (levels) means names (see 'labels')
##                   and parameter 'xlas'; result is never less then 4 inches.
##  width = NULL     width of window. If NULL then is calculated from number of (levels) means, length of theirs names
##                   and parameter 'xlas'; result is never less then 4 inches.
##  coldef = "black" type of color definition. Default "black" means that background is black, plot lines and axes are yellow,
##                   axes labels and and other strings are white.
##  parMy = TRUE    if TRUE then graphical parameters set in funciton parMy() are in use.
## ---------------------------------------------------------------------------------------------------------------------??

if(is.null(df)){tq = qnorm(1-alpha/2)}else{ tq = qt(1-alpha/2, df)}
n = length(means) # == length(unique(levels))
if(is.null(labels)){labels = names(means)}else{names(means)=labels}     ## (0)
upper = means + tq*stderrs
lower = means - tq*stderrs         ## (1)
d = max(upper) - min(lower)
#  nams = model$xlevels[[1]]


## ------------------------------------------------------------------------------------------------??
##  winows() and graphical parameters
   max.length.level.name = max(unlist(lapply(strsplit(as.character(unique(labels)), split=""), length)))  ## in characters
   min.width = .5

shut=is.null(dev.list())  ; print(shut)
old = par()
cin = par("cin") ## (width, height)
if(parMy){parMy()}
mar = par("mar")
if(shut){dev.off()}

if( xlas %in% c(0, 1) ){        ## horizontal
   add.to.height = 0
      ##
   multiple.width = max(max.length.level.name*cin[1], min.width)
}else if( xlas %in% c(2, 3) ){  ## vertical
      xlab = ""  #!#!#! IT'S  NOT A GOOD SOLUTION BUT HOW TO POSITION xlab ??? Say, at 10th line below axis ?
    max.length.level.name.lines = max.length.level.name*0.6
    mar[1] = max(max.length.level.name.lines, mar[1])  ## in lines
      ##
    max.length.level.name.inches = max.length.level.name.lines*cin[2]
    add.to.height.lines = max( max.length.level.name.lines - 1 , 0 )
   add.to.height = add.to.height.lines*cin[2]    ## inches
      ##
   multiple.width = min.width
}else{ stop("'xlas' must be one of 0, 1, 2, 3.") }

## --------------------------------------------------------??

if(windows){

   if(is.null(height)){
      height = 4 + add.to.height        ## inches
   }

   if(is.null(width)){
      width.from.levels = n*multiple.width
      width.from.main   = length( strsplit(main, split="")[[1]] )*cin[1]
      width = max( max( width.from.main , width.from.levels ) , 4 )  ## inches, at least 4 inches
   }

   windows(height = height , width = width)  ## in inches
   if(parMy){parMy()}
   par( mar = mar )   ## in lines
   coldef(coldef)
}


## ------------------------------------------------------------------------------------------------??


if(is.null(ylim)){ylim=c(min(lower)-d/15, max(upper)+d/15)}        ## (2)
plot(1:n, means, las=2, type="p", pch=".",...,
      xlim=c(.5, n +.5), ylim=ylim,
      axes=FALSE, ylab=ylab, xlab=xlab,
      frame.plot=TRUE)
   axis(side=1, at=1:n, tck=.01, labels=labels, las=xlas)
   axis(side=2, tck=.01, las=3)
abline(h=0, lty=1, col="grey")
             #  h        v
if(!is.null(mean)){
   abline(h=mean, lty = 1 , col="orange")
}

for(j in 1:n){
   if(type==""){
      lines(c(j, j), c(upper[j], lower[j]), lwd=lwd,...)              ## vertical line   (1')
      lines(c(j-wdth, j+wdth), c(means[j], means[j]), lwd=lwd*2,...)  ## means    (3)
   } else {
      points(j, means[j], pch=type, cex=1.5*lwd)
   }
   lines(c(j-wdth, j+wdth), c(upper[j], upper[j]), lwd=lwd,...)    ## upper bounds
   lines(c(j-wdth, j+wdth), c(lower[j], lower[j]), lwd=lwd,...)    ## lower bounds
}
if(text){
     if(rotate){
          for(j in 1:n){text( j+par()$cxy[1]/3, means[j]+par()$cxy[2]/3,
                       labels=round(means[j], 4), adj=c(0, 1), srt=90 )
          }
     } else {
          if(is.null(pos)){pos = cbind(rep(0, n), rep(0, n))
          }else{ k = length(pos)
                 pos = lapply( strsplit(pos, split=""),
                              function(x){as.numeric(is.element(c("l", "d"), x))} )
                 pos = t(as.data.frame(pos))
                 pos = pos[rep(1:k, length=n),] }
          sig = -2*pos+1
          d = sig %*% diag( par()$cxy )/5
          for(j in 1:n){
                text(  j+d[j, 1]
                     , means[j]+d[j, 2]
                     , labels=round(means[j], 4)
                     , adj=pos[j,]
                     , cex = cex.means
                    )
          }
   }
}
title(main = main)
results = cbind(lower, means, upper)
par(old)
result.list = list( "labels"=labels , "means"=means , "SE"=stderrs , "intervals"=results ,  "Student's quantile"=tq , "df"=df , "alpha"=alpha )
if(windows){ result.list[["window.size"]] = c("height"=height, "width"=width) }
return( result.list )
} ##----END----##
## ---------------------------------------------------------------------------------------------------------------------??

## ---------------------------------------------------------------------------------------------------------------------??
## EXAMPLES ############################################################################################################??
## ---------------------------------------------------------------------------------------------------------------------??

## -------------------------------------------------------------------------------------------
dummy = function(){
## This is dummy function - it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## - in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
## -------------------------------------------------------------------------------------------

#   poz = c("ld", "lu", "rd", "ru", "r", "u", "d", "l")
#   strsplit(poz, split="")
#   aa = lapply( strsplit(poz, split=""), function(x){ as.numeric( is.element(c("l", "d"), x)) } )
#   t(as.data.frame(aa))
#
#   n=9
#   poz = c("ld", "r", "u", "l")
#   k = length(poz)
#      strsplit(poz, split="")
#   aa = lapply( strsplit(poz, split=""), function(x){ as.numeric( is.element(c("l", "d"), x)) } )
#   (mm = t(as.data.frame(aa)))
#   mm[rep(1:k, length=n),]

yy = runif(100) + seq(-1, 2, length=100)^2
xx = sample(letters[1:3], 100, replace=TRUE)
   #model = lm(yy~xx)
plot.lm.means(yy~xx)

plot.lm.means(yy~xx, windows=TRUE)
plot.lm.means(yy~xx, windows=TRUE, coldef="grey")
plot.lm.means(yy~xx, windows=TRUE, coldef="white")

## --------------------------------------------------------
## more advanced example (random data)

set.seed(1000)   ## for reproducibility

## ----------------
## data

n = sapply( letters[1:16], FUN=function(x){1000+sample(800, 1)-400} )
n  ## number of observations for each level

ww = factor( unlist( sapply( 1:length(n), FUN=function(k)rep(names(n)[k], n[k]) ) ) )
table(ww)

means = runif(16)/3 + .1; names(means) = names(n); means
zz = unlist( sapply( names(n) , FUN=function(x){ pmax( rexp( n[x], rate=1/means[x]), 0) } ) )  ## list

## ----------------
## plots

cols = rep(2:8, length=16); names(cols) = levels(ww); cols = cols[as.character(ww)]
windows(); parMy()
plot(zz, pch='.', col=cols)
windows(); parMy()
plot(zz~ww)

plot.lm.means(zz~ww, windows=TRUE)
plot.lm.means(zz~ww, windows=TRUE, sort=TRUE)
plot.lm.means(zz~ww, windows=TRUE, sort=TRUE, text=FALSE)
plot.lm.means(zz~ww, windows=TRUE, sort="desc", text=FALSE)

## If the mean of one group (level of a factor) is within confidence interval of the mean of the second group,
## and vice versa, as e.g. for first two groups (l, f),
## then means of these two groups are NOT SIGNIFICANTLY DIFFERENT
## i.e. there's NO statistical evidence that difference between these means is something more than random fluctuation.
##
## (This reasoning is based on the assumptions that data comes from normal distribution,
## however it may be quite safely applied to other distributions if only they have finite variance
## ~= do not have heavy tails).
##
## To aquire factor variable having all means significantly different one must join levels
## with "overlapping" confidence intervals (i.e. CI covering means of the second group, as described above).
##

ww2 = ww
## joining factor levels
levels(ww2) = list( "A" = c("l", "f"), "B" = c("b", "j"), "C" = c("i", "d", "o")
                  , "D" = c("a", "k"), "E" = c("g"), "F" = c("e") , "G" = c("n", "m", "p", "c", "h"))
plot.lm.means(zz~ww2, windows=TRUE, sort="desc", text=FALSE)

## New factor aquired this way is a good candidate to survive variable selection in model building.

anova(lm(zz~ww))
anova(lm(zz~ww2))

## --------------------------------------------------------
## the same as above but with smaller differences between means

set.seed(3000)

means = runif(16)/8 + .2; names(means) = names(n); means
zz = unlist( sapply( names(n) , FUN=function(x){ pmax( rexp( n[x], rate=1/means[x]), 0) } ) )  ## list
plot.lm.means(zz~ww, windows=TRUE, sort="desc", text=FALSE)

## While joining groups, remember that bigger groups gives smaller CIs
## because CI is in inverse proportion to the group size.
## This helps to join levels in this example to aquire significant difference between them:

ww3 = ww
## joining factor levels
levels(ww3) = list( "A" = c("d", "a"), "B" = c("c", "g", "e", "h", "j"), "C" = c("l", "k", "i")
                  , "D" = c("b", "m"), "E" = c("p", "f", "o", "n"))
plot.lm.means(zz~ww3, windows=TRUE, sort="desc", text=FALSE)

anova(lm(zz~ww3))

}

rm(dummy)
