########################################################################################################################—•°
## List of linear model on transformed data.
########################################################################################################################—•°
## FUNCTIONS HERE    {package}
##  tlm_list( ... )
##  tlm_list.default( ... )
##  plot.tlm_list( ... )
##  print.tlm_list( ... )
##  summary.tlm_list( ... )    ## TO BE DONE
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
tlm_list <- function(x,...){UseMethod("tlm_list")}
########################################################################################################################—•°

########################################################################################################################—•°
tlm_list.default <- function( x = NULL , y = NULL , group = NULL
                       , formula="y~x"
                       , FUNy=function(y)y, FUNx=function(x)x, FUNyhat=NULL, FUNxhat=NULL
                       , weights=NULL
                       , plot=FALSE , xlab=NULL
                       # , ylab=NULL , title=NULL                             ## will be added later
                       ){
###################################################################################################—•°
## x
## y      matrix or data.frame
## ...    arguments passed to tlm()
###################################################################################################—•°
result = structure(list(),class="tlm_list")

gr <- ncol(y)

if(is.null(group)){
   group <- seq_len(gr)
}
y <- y[,group]
if(!is.null(colnames(y))){
   group = colnames(y)
}

# if(weights) ...     #!#!!!!!!!!

for(k in group){

   ylab <- k

   tryCatch(
        {result_k = tlm( x , y[,k] ,
                       , formula=formula
                       , FUNy=FUNy, FUNx=FUNx, FUNyhat=FUNyhat, FUNxhat=FUNxhat
                       , weights=weights
                       , plot=plot, xlab=xlab, ylab=ylab
                       )}
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
print.tlm_list <- function( tll , ... ){
###################################################################################################—•°
##  tll    object of class "tlm_list"
###################################################################################################—•°

  cat("object of class \"tlm_list\"\n")
  cat("\n")

  ## indentr(tll , add="tlm" , ... )  #!#!#! NIE DZIA£A !!! :(

  for(k in seq_len(length(tll))){
  cat(names(tll)[k],"\n")
      #  indent(print(tll[[k]]),1)  #!#!#! NIE DZIA£A !!! :(
        indent(summary(tll[[k]]$lm),1)
        if(!is.null(tll[[k]]$lm0)) indent(print(tll[[k]]$lm0))
  }

}  ##----END----##
########################################################################################################################—•°

########################################################################################################################—•°
plot.tlm_list <- function(tll
                          ##,title=NULL,xlab=NULL,ylab=NULL,...                         ## will be added later
                         ){
###################################################################################################—•°
##  tll    object of class "tlm_list"
###################################################################################################—•°
invisible(lapply(tll,plot))
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
yy = cbg$curves
yy

tlm_list(y=yy)

tll <- tlm_list(y=yy,group=1:3,formula="y ~ I(x^2) + x")
tll
tll <- tlm_list(y=yy,group=1:3,formula="y ~ I(x^2) + x",plot=T)
plot(tll)
summary(tll)


tll=tlm_list(y=yy,group=1:3,FUNy=log,FUNyhat=exp)
tll
plot(tll)


}
########################################################################################################################—•°
rm(dummy)