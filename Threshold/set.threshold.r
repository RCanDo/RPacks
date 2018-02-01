set.threshold = function( variable
                        , goodsallratio=.1
                        , old.threshold=NULL
                        #, covariate=NULL
                        , coldef="black"
                        , na.rm=FALSE
                        , graphics = FALSE
                        #, alternative = "two.sided"
                        , threshold.name = NULL
                        , envir = NULL
                        , message = TRUE   
                        )
{
## variable = skut;  goodsallratio = .1; old.threshold = GoodBadThreshold; covariate = wplaty; coldef = "black"
##
## Arguments
##  variable    numeric variable for which we are searching for the threshold such that 
##              goodsallratio portion of all observations will be STRICTLY GREATER then threshold;
##              exact formula for threshold is
##                 if(goodsallratio<=0){ 
##                     threshold = max(variable)+1 
##                 }else{
##                     threshold = sort(variable)[ max(0,floor((1-goodsallratio)*length(variable))) + 1 ]
##                 }      
#???              hence a little more then goodsallratio portion of observations will satisfy the condition,
#???              and goodsallratio may take safely any real value, however anly those between 0 and 1 are sensible.
##  goodsallratio=.1    see above;
##  old.threshold     former value of threshold to which we like to compare the new one;
##  threshold.name    name of the variable to which new threshold will be assigned;
##                    if left NULL  (default)  then no new cariable will be crated;
##  envir       environment to which the variable will be returned (if threshold.name is not NULL and valid);
##              if NULL (default) then is set to ".GlobalEnv". 
##  graphics    if TRUE (default is FALSE) then plot(s) will be drawn.
##  alternative     alternative hypothesis passed to wilcox.test in case the covariate is nonempty and non-factor,
##                  possible values are "two.sided", "less", "greater".
##  na.rm=FALSE
##  message
##
## Value
##  Function return a list with the following elements:
##    threshold
##    
##    table         table of bads and goods ratio 
  

if(!is.numeric(variable)){
  stop("Variable must be numeric.")
}

cdf = (1:length(variable))/length(variable)
   # threshold = sort(skut)[ max( ( 1:length(skut) )[cdf<.9] ) ]
if(goodsallratio<=0){ 
    threshold = max(variable)+1 
}else{
   threshold = sort(variable)[ max(0,floor((1-goodsallratio)*length(variable))) + 1 ]
}     ## GoodBadThreshold daj¹cy goodsallratio*100% goodow


tab <- table(variable>threshold)/length(variable)                   ## sprawdzenie
names(tab) = ifelse(names(tab),"goods","bads")
  gbv = c(0,0); names(gbv) = c("bads","goods")
  if(!is.na(tab["bads"])){ gbv["bads"] = tab["bads"] }
  if(!is.na(tab["goods"])){ gbv["goods"] = tab["goods"] }
  


### assigning threshold variable 
if(!is.null(threshold.name) & is.character(threshold.name)){
    if(is.null(envir)){envir = 1} #".GlobalEnv"}
    assign(threshold.name,threshold,pos=envir)
}

   GoodsBads = variable>threshold       ##  == CzyDobra
   GoodsBads = ifelse(GoodsBads,"good","bad")

result = list( threshold = threshold 
             , table = gbv
             ) 

if(message){
   cat(paste0("New threshold is ",threshold,"\n\n"))
   print(gbv)   
}
      
###---graphics---###       
if(graphics){
    windows();margins.my();if(coldef=="black"){coldef.black()}
    
    #if(!is.null(covariate)){par(mfrow=c(2,1))}else{par(mfrow=c(1,1))}
    
    plot(cdf~sort(variable), pch=".",xlab=if(!missing(variable)){deparse(substitute(variable))});
       abline(v=0,col="grey");   abline(h=0,col="grey"); abline(h=1,col="grey")
      abline(h=1-goodsallratio,col=3); abline(v=threshold, col=2);
      tytul = paste0("Efficiency distribution. #goods = ",signif(tab[2],4),", bads = ",signif(tab[1],4))
      title(tytul);
    
    if(!is.null(old.threshold)){
       tab.2 <- table(variable>old.threshold)/length(variable)
       abline(v=old.threshold, col=6,lty=3); abline(h=1-tab.2[2], col=5,lty=3)
       legend( "bottomright",lty=c(1,1,3,3),col=c(2,3,6,5),
          legend=c(paste0("threshold = ",signif(threshold,4)), paste0("bads portion = ",signif(gbv["bads"],4)),
                   paste0("old.threshold = ",signif(old.threshold,4)), paste0("bads portion = ",signif(1-tab.2[2],4)) ) )
    }else{
       legend( "bottomright",lty=1,col=c(2,3),
          legend=c(paste0("threshold = ",signif(threshold,4)), "#bads = .09" ) )
    }
}    
###---END of graphics---###

#if(!is.null(covariate)){
#   
#
#   stat = split.stats( covariate , GoodsBads , alternative = alternative
#                      , graphics = graphics  #!#---graphics---###
#                      , message = message
#                      )
#
#   result$covariate.stats = stat
#
#}
######################################################################################################

result$GoodsBads = GoodsBads 

result

} 
