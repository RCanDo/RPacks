########################################################################################################################—•°
## Series of linear models for curves_by_groups.
########################################################################################################################—•°
## FUNCTIONS HERE    {package}
##  fun1( x , y , ... )
##  fun2( ... )
##
## DEPENDENCIES
##  funa()        {package} file.R     [defalt {package} is the same; default file.R is the same as function name]
##  funb()
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
## PATH: \\10.1.1.5\PK_Analitycy\R\PacksAK\EfficiencyCurves\linmod.curves_by_groups.R
## start: 2017-08-23    last: 2017-08-24
########################################################################################################################—•°

########################################################################################################################—•°
linmod_list <- function(x,...){UseMethod("linmod_list")}
########################################################################################################################—•°

########################################################################################################################—•°
linmod_list.curves_by_groups <- function( cbg , group=NULL , ... ){
###################################################################################################—•°
result = structure(list(),class="linmod_list")

if(is.null(group)){
   group = c(0,seq_len(length(cbg$group)))
}
group = colnames(cbg$curves[,group])

for(k in group){

   tryCatch(
        {result_k = linmod(cbg,group=k, ... )}
      , error = function(e){
        result_k = Message("modelError","The model is impossible for given data.")
      }
   )
   result[[k]] = result_k
}

result
}  ##----END----##
########################################################################################################################—•°

########################################################################################################################—•°
print.linmod_list <- function( lil , ... ){
###################################################################################################—•°
##  lit    object of class "linmod_list"
###################################################################################################—•°

  cat("object of class \"linmod_list\"\n")
  cat("\n")

  ## indentr(lil , add="linmod_item" , ... )  #!#!#! NIE DZIA£A !!! :(

  for(k in seq_len(length(lil))){
  cat(names(lil)[k],"\n")
      #  indent(print(lil[[k]]),1)  #!#!#! NIE DZIA£A !!! :(
        indent(summary(lil[[k]]$lm),1)
        if(!is.null(lil[[k]]$lm0)) indent(print(lil[[k]]$lm0))
  }

}  ##----END----##
########################################################################################################################—•°

########################################################################################################################—•°
linmod <- function(x,...){UseMethod("linmod")}
########################################################################################################################—•°

########################################################################################################################—•°
linmod.curves_by_groups <- function( cbg , group=NULL
                                     , formula="y~t"
                                     , FUNy=function(x)x, FUNt=function(x)x, FUNyhat=NULL, FUNthat=NULL
                                     , plot=TRUE, weights=NULL ){
###################################################################################################—•°
## Description short...
##    Arguments
##  cbg                object inheriting from "curves_by_groups";
##  group=NULL         only first element is considered;
##  formula="y~t"
##  FUNy=function(x)x  function
##  FUNt=function(x)x  function
##  FUNyhat=NULL       function
##  FUNthat=NULL       function
##
##    Result
##  $mod          linear model
##  $FUN
##  $
##
##    Description/Comments/Remarks
##
###################################################################################################—•°
                 print(class(FUNyhat))

   result_k = structure(list(),class="linmod_item")

   k <- group[1]

   n <- length(cbg$net_curve)
   t0 <- seq_len(n)

   if(k==0 || k=="net_curve"){
        y0 <- cbg$net_curve
        fig_title <- "net_curve"
   }else{
        y0 <- cbg$curves[,k]
        fig_title <- cbg$group[k]
   }

      result_k$t0 = t0
      result_k$y0 = y0

   t <- FUNt(t0)
   y <- FUNy(y0)

      result_k$t = t
      result_k$y = y

   mod <- lm(formula=as.formula(formula),weights=weights)
   yhat <- predict(mod)

      result_k$yhat = yhat
      m = mod$rank

   if(plot){
      windows();parMy();par(mfrow=c(2,2),oma=c(0,0,1.5,0))
      plot(mod,sub.caption=ifelse(is.numeric(k),fig_title,k));
       ##
      windows();parMy()
      plot(y~t,pch=20);lines(y~t,lty=2)
      abline(h=0,col="darkgrey")
      lines(yhat~t,col="white")
      title(fig_title)
   }
   result_k$FUN = structure(list( t = FUNt , y = FUNy , yhat = FUNyhat , that = FUNthat ),class="FUNlist")
   result_k$lm = mod

   if(!(is.null(FUNyhat)&is.null(FUNthat))){

      if(!is.null(FUNyhat)){ yhat <- FUNyhat(yhat) ; y <- FUNyhat(y) }
      if(!is.null(FUNthat)){ t <- FUNthat(t) }

         result_k$t0 = t
         result_k$y0 = y
         result_k$yhat0 = yhat
         result_k$ehat0 = y-yhat


      tss = sum((y-mean(y))^2)       ## Total
      ess = sum((result_k$ehat0)^2)  ## Error (Residuals)
      rss = tss - ess                ## Regression
      rse = sqrt(ess/(n-m))                ## Residual Standard Error
      r2  = rss/tss
      r2adj = 1 - (1-r2)*(n-m)/(n-1)
      Fisher = r2*(n-m)/(1-r2)/(m-1)
      pval = pf(Fisher,m-1,n-m,lower.tail=F)

      lm0 = structure(c(tss=tss,ess=ess,rss=rss,rse=rse,r2=r2,r2adj=r2adj,Fisher=Fisher,pval=pval,n=n,m=m),class="lm0")

          result_k$lm0 = lm0

      if(plot){
         windows();parMy()
         plot(y~t,pch=20);lines(y~t,lty=2)
         abline(h=0,col="darkgrey")
         lines(yhat~t,col="white")
         title(fig_title)
      }

   }

   result_k

}  ##----END----##
########################################################################################################################—•°
           is.primitive(mean)
########################################################################################################################—•°
print.linmod_item <- function( lit , ... ){
###################################################################################################—•°
##  lit    object of class "linmod_item"
###################################################################################################—•°

  cat("object of class \"linmod_item\"\n")

  indentr(lit ,compact = 5 ,delete = c("lm","lm0") , ... )

  print(summary(lit$lm))
#  if(!is.null(lit$lm0)) indent(print(lit$lm0))

}  ##----END----##
########################################################################################################################—•°

print.lm0 <- function(x,...){

cat("Parameters of the model in space 0 (after backward transformation):\n\n")

 for(nam in names(x)){
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
    if(!nam%in%c("m","n")) cat(x[nam])
    switch( nam
          , "rse" = cat(" on",x["n"]-x["m"],"DF")
          , "Fisher" = cat(" on",x["m"]-1,"and",x["n"]-x["m"],"DF")
          , "default" = cat("\n")
          )
    if(!nam%in%c("m","n")) cat("\n")
 }
 cat("\n")

}

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