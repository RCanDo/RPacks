########################################################################################################################—•°
## Linear model on transformed data.
########################################################################################################################—•°
## FUNCTIONS HERE    {package}
##  tlm( ... )
##  tlm.default( ... )
##  plot.tlm( ... )
##  print.tlm( ... )
##  summary.tlm( ... )
##  print.lm0( ... )
##
##
## DEPENDENCIES
##  indentr()        {DM} .
##  indent()         {DM} .
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
## PATH: \\10.1.1.5\PK_Analitycy\R\PacksAK\EfficiencyCurves\tlm.R
## start: 2017-08-23    last: 2017-08-28
########################################################################################################################—•°

########################################################################################################################—•°
tlm <- function(x,...){UseMethod("tlm")}
########################################################################################################################—•°


########################################################################################################################—•°
tlm.default <- function( x = NULL , y = NULL
                       , formula="y~x"
                       , FUNy=function(y)y, FUNx=function(x)x, FUNyhat=NULL, FUNxhat=NULL
                       , weights=NULL
                       , plot=FALSE , xlab=NULL , ylab=NULL , title=NULL
                       ){
###################################################################################################—•°
## Description short...
##    Arguments
##  x,y
##  formula="y~x"
##  FUNy=function(y)y  function
##  FUNx=function(x)x  function
##  FUNyhat=NULL       function
##  FUNxhat=NULL       function
##
##    Result
##  Object of class "tm" comprising the following entries:
##  $
##  $
##  $lm          linear model
##  $lm0
##
##    Description/Comments/Remarks
##
###################################################################################################—•°

   result = structure(list(),class="tlm")

   if(is.null(xlab)){ xlab <- deparse(substitute(x))
       if(xlab=="NULL"){ xlab = "t" }
   }
   result$xlab <- xlab
   if(is.null(ylab)){
      ylab <- deparse(substitute(y))
   }
   result$ylab <- ylab

   if(is.null(title)){
      title = paste0(ylab," ~ ",xlab)
   }
   result$title <- title


   n <- length(y)
   if(is.null(x)){ x <- seq_len(n) }

   result$x0 <- x
   result$y0 <- y

   result$x <- x <- FUNx(x)
   result$y <- y <- FUNy(y)

   mod <- lm(formula=as.formula(formula),weights=weights)

   result$yhat <- yhat <- predict(mod)

   m = mod$rank

#   if(plot){ ... }
   result$FUN = structure(list( x = FUNx , y = FUNy , yhat = FUNyhat , xhat = FUNxhat ),class="FUNlist")
   result$lm = mod

   if(!(is.null(FUNyhat)&is.null(FUNxhat))){

      if(!is.null(FUNyhat)){ yhat <- FUNyhat(yhat) ; y <- FUNyhat(y) }
      if(!is.null(FUNxhat)){ x <- FUNxhat(x) }

         result$x0 = x
         result$y0 = y
         result$yhat0 = yhat
         result$ehat0 = y-yhat

      tss = sum((y-mean(y))^2)       ## Total
      ess = sum((result$ehat0)^2)  ## Error (Residuals)
      rss = tss - ess                ## Regression
      rse = sqrt(ess/(n-m))                ## Residual Standard Error
      r2  = rss/tss
      r2adj = 1 - (1-r2)*(n-m)/(n-1)
      Fisher = r2*(n-m)/(1-r2)/(m-1)
      pval = pf(Fisher,m-1,n-m,lower.tail=F)

      lm0 = structure(c(tss=tss,ess=ess,rss=rss,rse=rse,r2=r2,r2adj=r2adj,Fisher=Fisher,pval=pval,n=n,m=m),class="lm0")

          result$lm0 = lm0

          ## if(plot){ ...  }
   }

   if(plot){
      plot(result,title,xlab=xlab,ylab=ylab)
   }

   result

}  ##----END----##
########################################################################################################################—•°

########################################################################################################################—•°
plot.tlm <- function(tlm,title=NULL,xlab=NULL,ylab=NULL,...){
###################################################################################################—•°
##  tlm    object of class "tlm"
###################################################################################################—•°
if(is.null(xlab)){ xlab <- tlm$xlab }
if(is.null(ylab)){ ylab <- tlm$ylab }
if(is.null(title)){ title <- tlm$title }

windows();parMy();par(mfrow=c(2,2),oma=c(0,0,1.5,0))
plot(tlm$lm,sub.caption=title)
 ##
windows();parMy()
plot(tlm$y~tlm$x,pch=20,xlab=xlab,ylab=ylab,...)
lines(tlm$y~tlm$x,lty=2)
abline(h=0,col="darkgrey")
lines(tlm$yhat~tlm$x,col="white")
title(title)

if(!is.null(tlm$lm0)){
   windows();parMy()
   plot(tlm$y0~tlm$x0,pch=20,xlab=xlab,ylab=ylab,...)
   lines(tlm$y0~tlm$x0,lty=2)
   abline(h=0,col="darkgrey")
   lines(tlm$yhat0~tlm$x0,col="white")
   title(paste(title," [0]"))
}

}  ##----END----##
########################################################################################################################—•°

########################################################################################################################—•°
print.tlm <- function( tlm , ... ){
###################################################################################################—•°
##  tlm    object of class "tlm"
###################################################################################################—•°

  cat("object of class \"tlm\"\n")

  indentr(tlm ,compact = 5 ,delete = c("lm","lm0") , ... )

  cat("lm\n")
  indent(tlm$lm)

  if(!is.null(tlm$lm0)){ cat("lm0\n") ; indent(tlm$lm0) }

}  ##----END----##
########################################################################################################################—•°

########################################################################################################################—•°
print.lm0 <- function(lm0,...){
###################################################################################################—•°
##  lm0    object of class "lm0"
###################################################################################################—•°
cat("Parameters of the model in space 0 (after backward transformation):\n\n")

 for(nam in names(lm0)){
    switch( nam
          , "tss" =  cat("Total Sum of Squares: ")
          , "ess" =  cat("Error Sum of Squares: ")
          , "rss" =  cat("Regression Sum of Squares: ")
          , "rse" =  cat("Residual Standard Error: ")
          , "r2"  =  cat("Multiple R-squared: ")
          , "r2adj" =  cat("Adjusted R-squared: ")
          , "Fisher" =  cat("F-statistic: ")
          , "pval" =  cat("p-value: ")
          , "default" =  {}
          )
    if(!nam%in%c("m","n")) cat(lm0[nam])
    switch( nam
          , "rse" = cat(" on",lm0["n"]-lm0["m"],"DF")
          , "Fisher" = cat(" on",lm0["m"]-1,"and",lm0["n"]-lm0["m"],"DF")
          , "default" = cat("\n")
          )
    if(!nam%in%c("m","n")) cat("\n")
 }
 cat("\n")

}  ##----END----##
########################################################################################################################—•°
summary.tlm <- function( tlm , ... ){
###################################################################################################—•°
##  lit    object of class "tlm_item"
###################################################################################################—•°
structure( list( lm = summary(tlm$lm) , lm0 = tlm$lm0 ) , class = "summary.tlm")
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


tl <- tlm(x=1:length(cbg$net_curve),y=cbg$curves[,1],formula="y ~ I(x^2) + x")
tl <- tlm(y=cbg$curves[,1],formula="y ~ I(x^2) + x")
tl
summary(tl)
plot(tl)
plot(tl,"Portfolio")
tlm(y=cbg$curves[,1],formula="y ~ I(x^2) + x",plot=T)
tlm(y=cbg$curves[,1],formula="y ~ I(x^2) + x",plot=T,title="BZ")
tlm(y=cbg$curves[,1],formula="y ~ I(x^2) + x",plot=T,title="BZ",xlab="time",ylab="efficiency")


tl=tlm(y=cbg$curves[,1],FUNy=log,FUNyhat=exp)
tl
summary(tl)
plot(tl)
plot(tl,"Portfolio")


tlm(cbg,group=0,formula="y ~ I(t^2) + t",FUNy=log,FUNyhat=exp)
tlm(cbg,group=0,formula="y ~ I(t^3) + I(t^2) + t",FUNy=log,FUNyhat=exp)



tlm(cbg,group=0,FUNy=log,FUNt=log,FUNyhat=exp,FUNthat=exp)
tlm(cbg,group=0,formula="y ~ I(t^2) + t",FUNy=log,FUNt=log,FUNyhat=exp,FUNthat=exp)
tlm(cbg,group=0,formula="y ~ I(t^5) + I(t^4) + I(t^3) + I(t^2) + t",FUNy=log,FUNt=log,FUNyhat=exp,FUNthat=exp)

}
########################################################################################################################—•°
rm(dummy)