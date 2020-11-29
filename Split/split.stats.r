## ---------------------------------------------------------------------------------------------------------------------•°
## FUNCTIONS HERE
##    split.stats()
##
## DEPENDENCIES
##  plot.split.r
##    plot.split()
##    ...
## ---------------------------------------------------------------------------------------------------------------------•°
split.stats = function(variable, split
              , alternative = "two.sided"
              , message = TRUE
              , plot = FALSE
              , colors = c("red","green")
              , ...
              ){
## ---------------------------------------------------------------------------------------------------------------------•°
## Measuring disparity betweem two samples.
##
## Arguments
##  variable        variable which will be divided in two samples according to split given in:
##  split           must have exactly two unique values;
##  alternative     alternative hypothesis passed to wilcox.test,
##                  possible values are "two.sided", "less", "greater";
##  message         if TRUE (default) then summary of the calculated statistics is printed on the screen;
##                  if set to FALSE no message will be printed;
##  plot            if TRUE then uses plot.split() to plot 'variable' splitted by 'split';
##                  default is FALSE;
##  colors          colors of split groups on the graph of densities or bars;
##                  default are c("red","green");
##  ...             arguments passed to plot.split() which are then passed to density();
##
## In case 'variable' is a factor chi-square test is performed into the object 'stat'
## from which the following values are extracted and calculated:
##   method = c("Wilcoxon")
##   p.value = stat$p.value
##   statistics = stat$statistic
##   table = tab
##   auc = auc
##   gini.index = 2*auc - 1
##
## If 'variable' is numeric then Mann-Whithey-Wilcoxon test is performed into the object 'stat'
## from which the following values are extracted and calculated:
##   method = c("chi-square")
##   p.value = stat$p.value
##   statistics = stat$statistic
##   observed = stat$observed
##   prob.by.split             probability of each level conditionaly on being in the given sample
##   expected = stat$expected
##   residuals = stat$residuals
##   stdres = stat$stdres
##   df = stat$parameter
##
## In both cases result is enveloped into the list which is the only object returned by the function.
## This list becomes NULL if some error occure during calculations.
##
## ---------------------------------------------------------------------------------------------------------------------•°

result = NULL
  
  
if(length(unique(split))!=2){

  warning("This function is designed to measure disparity between two samples
  wich are abstracted from the 'variable' according to the 'split' values.
  Thus the 'split' must have exactly two unique values. ")

}else if(length(variable)!=length(split)){

  warning("'variable' and 'split' must have the same length.")
  
}else{
  tab = table(split)

  if(is.numeric(variable)){
     ###___numeric___###
    stat = wilcox.test(variable ~ split, alternative = alternative)

    zz = stat$statistic/prod(tab)
    auc = max(zz,1-zz)

    result = list( test = c("Mann-Whitney-Wilcoxon")
                 , p.value = stat$p.value
                 , statistics = stat$statistic
                 , table = tab
                 , auc = auc
                 , gini.index = 2*auc - 1
                 )
    names(result$p.value) = "p.value"
    names(result$auc) = "AUC (area under ROC curve)"
    names(result$gini.index) = "Gini Index"

    if(message){
      cat("Variable was numeric and Mann-Whitney-Wilcoxon test was applied.\n")
      cat(paste0("The null hypothesis is that neither 'goods' are statisticaly greater then 'bads' nor the opposite.\n"))
          print(data.frame("v" = c(result$p.value,result$auc,result$gini.index)))
          cat("\n")
          #  attr(result$table,"dimnames")$split = c("bads","goods")
          print(cbind(ratio = tab/sum(tab),counts = tab),digits=5)
          cat("\n")
    }
    
  }else{
     ###___factor___###
    stat = chisq.test(variable , split)
    
    result = list( test = c("chi-square")
                 , p.value = stat$p.value
                 , statistics = stat$statistic
                 , observed = stat$observed
                 , prob.by.split = scale(stat$observed,center=FALSE,scale=colSums(stat$observed)) # stat$observed%*%diag(1/colSums(stat$observed))
                 , expected = stat$expected
                 , residuals = stat$residuals
                 , stdres = stat$stdres
                 , df = stat$parameter
                 )
    names(result$p.value) = "p.value"

    if(message){
      cat("Variable was a factor and Chi-square test was applied.\n")
      cat(paste0("The null hypothesis is that both samples ('goods' and 'bads') have the same distribution.\n"))
          print(data.frame("v" = result$p.value))
          cat("\n")
          #  attr(result$observed,"dimnames")$split = c("bads","goods")
          print(cbind(ratio = tab/sum(tab), t(result$observed)),digits=5)
          cat("\n")
    }
  }
  
  if(plot){
      split.plot( variable , split , colors = colors , ... )
  }

}

result

} ##----END----##
## ---------------------------------------------------------------------------------------------------------------------•°

## ---------------------------------------------------------------------------------------------------------------------•°
## EXAMPLES ############################################################################################################•°
## ---------------------------------------------------------------------------------------------------------------------•°

## -------------------------------------------------------------------------------------------•°
dummy = function(){
## This is dummy function - it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## - in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
## -------------------------------------------------------------------------------------------•°
}
