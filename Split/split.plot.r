## ---------------------------------------------------------------------------------------------------------------------•°
## FUNCTIONS HERE
##    split.plot()
##    plot.split() # synonym
##
## DEPENDENCIES
##    ...
## ---------------------------------------------------------------------------------------------------------------------•°
split.plot = function(variable, split, colors = c("red","green") , ... )
{
## ---------------------------------------------------------------------------------------------------------------------•°
##  Plots 'variable' split into two samples according to 'split'
##  which must have exactly two unique values and have the same length as 'variable'.
##
##  If 'variable' is numeric then two densities are plotted.
##
##  In case of factor bars are plotted: two for each factor level in colors
##  distinct for each sample.
##  Bars for each sample constitute probability function,
##  i.e. each bar has height of probability of the given level conditionaly on
##  being in the given sample.
##  It allows to campare distributions of levels for both samples.
##
##  Arguments
##    variable
##    split
##    colors  vector indicating colors for samples in the usual way;
##    ...     arguments passed to density().
##
##  Values
##    Returns NULL if 'split' doesn't have exactly two unique values
##    or is of different length than 'variable'.
##    Otherwise returns TRUE.
##
#! To be generalized for split with more then 2 values.
## ---------------------------------------------------------------------------------------------------------------------•°

result = NULL

if(length(unique(split))!=2){

  warning("This function is designed to plot two samples
  wich are abstracted from the 'variable' according to the 'split' values.
  Thus the 'split' must have exactly two unique values. ")

}else if(length(variable)!=length(split)){

  warning("'variable' and 'split' must have the same length.")
  
}else{

   variable = variable[!is.na(variable)]
   split = split[!is.na(variable)]
   
   split = factor(split)
   
   if(is.numeric(variable)){
    ###___numeric___###

      if(length(unique(variable))/length(variable)<.1){
        warning("There is less then one tenth uniuqe values of a covariate compared to its length.
  In such a case densities may be misleading.
  It may be reasonable to coarce a covariate to a factor, e.g. by as.factor(variable). Then barplot will be drawn.")
      }

      variable.split = split(variable,split)
         dens0 = density(variable,...)
         dens1 = density(variable.split[[1]],...)
         dens2 = density(variable.split[[2]],...)
      plot(dens0,type="n"
                ,xlab=if(!missing(variable)){deparse(substitute(variable))}
                ,ylab="density"
                ,xlim=range(variable)
                ,ylim=c(0,max(max(dens1$y),max(dens2$y))))
      abline(v=0,col="grey")
      lines( dens1,col=colors[1]); rg = range(variable.split[[1]]);
         abline(v=rg, lty=3, col = colors[1])
      lines( dens2,col=colors[2]); rg = range(variable.split[[2]]);
         abline(v=rg, lty=3, col = colors[2])
      legend("topright",lty=1, col=colors,legend=levels(split))

   }else{
    ###___factor___###
      prob.by.split = table(variable,split)
      prob.by.split = scale(prob.by.split,center=FALSE,scale=colSums(prob.by.split))

      barplot(   t(prob.by.split)
               , beside = TRUE
               , col = colors
               , names.arg = levels(variable)
               , xlab=if(!missing(variable)){deparse(substitute(variable))}
               , ylab="probability"
             )

      legend( "topright" , legend=levels(split) , col = colors , pch=15 )

      title( paste0("levels probabilities for '",levels(split)[1],"' and '",levels(split)[2]),"'" )
   }

  result = TRUE
}

result

} ##----END----##
## ---------------------------------------------------------------------------------------------------------------------•°

## ---------------------------------------------------------------------------------------------------------------------•°
plot.split = function(variable, split, colors = c("red","green") , ... ){
   split.plot(...)
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
