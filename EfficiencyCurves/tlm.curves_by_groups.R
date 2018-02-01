########################################################################################################################—•°
## Linear model on transformed data from curves_by_groups.
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
## PATH: \\10.1.1.5\PK_Analitycy\R\PacksAK\EfficiencyCurves\tlm.curves_by_groups.R
## start: 2017-08-23    last: 2017-08-28
########################################################################################################################—•°


########################################################################################################################—•°
tlm.curves_by_groups <- function( cbg , group=NULL
                                      , formula="y~t"
                                     , FUNy=function(y)y, FUNt=function(t)t, FUNyhat=NULL, FUNthat=NULL
                                     , weights=NULL
                                     , plot=FALSE , title=NULL , ylab=NULL , xlab=NULL ){
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

if(is.null(group)||group[1]==0){
   k <- 0
   y <- cbg$net_curve
   if(is.null(ylab)){ ylab <- "efficiency" }
   if(is.null(title)){ title <- "net_efficiency" }
}else{
   k <- group[1]
   y <- cbg$curves[,k]
   if(is.null(ylab)){ ylab <- "efficiency" }
   if(is.null(title)){ title <- cbg$groups[k] }
}

formula <- gsub("t","x",formula)

result = tlm( x=NULL , y
           , formula=formula
           , FUNy=FUNy, FUNx=FUNt, FUNyhat=FUNyhat, FUNxhat=FUNthat
           , weights=weights
           , plot=plot , xlab=xlab , ylab=ylab , title=title
           )

result = structure(result,class=c("tlm.curves_by_groups","tlm"))

result

}  ##----END----##
########################################################################################################################—•°


########################################################################################################################—•°
tlm_list.curves_by_groups <- function( cbg , group=NULL
                                      , formula="y~t"
                                     , FUNy=function(y)y, FUNt=function(t)t, FUNyhat=NULL, FUNthat=NULL
                                     , weights=NULL
                                     , plot=FALSE  , xlab=NULL , ylab=NULL
                                     ## , title=NULL                             ## will be added later
                                     ){
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

result = structure(list(),class=c("tlm_list.curves_by_groups","tlm_list"))

gr <- ncol(cbg$curves)

if(is.null(group)){
   group <- c(0,seq_len(gr))
}

if(is.null(ylab)){ ylab <- "efficiency" }
formula <- gsub("t","x",formula)

for(k in group){

   if(k==0 || k=="net_curve"){
      y <- cbg$net_curve
      title <- "net_efficiency"
   }else{
      y <- cbg$curves[,k]
      title <- as.character(cbg$groups[k])
   }

   tryCatch(
        {result_k = tlm( x=NULL , y
                       , formula=formula
                       , FUNy=FUNy, FUNx=FUNt, FUNyhat=FUNyhat, FUNxhat=FUNthat
                       , weights=weights
                       , plot=plot , xlab=xlab , ylab=ylab , title=title
                       )}
      , error = function(e){
        result_k = Message("modelError","The model is impossible for given data.")
      }
   )
   result[[title]] = result_k
}

result

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

tlc <- tlm(cbg,formula="y ~ I(t^2) + t",FUNy=log)
tlc
summary(tlc)
plot(tlc)
tlc <- tlm(cbg,formula="y ~ I(t^2) + t",FUNy=log,plot=T)
tlc <- tlm(cbg,formula="y ~ I(t^2) + t",FUNy=log,plot=T,title="total",xlab="time",ylab="net_efficiency")

tlc <- tlm(cbg,group=0,formula="y ~ I(t^2) + t",FUNy=log,plot=T)

tlc <- tlm(cbg, group=4 ,formula="y ~ I(t^2) + t",FUNy=log)
tlc
summary(tlc)
plot(tlc)

tlc <- tlm(cbg,group=0,formula="y ~ I(t^2) + t",FUNy=log,FUNyhat=exp)
tlc
summary(tlc)
plot(tlc)


tlcl <- tlm_list(cbg,group=1:3,formula="y ~ I(t^2) + t",FUNy=log)
tlcl
plot(tlcl)

tlcl <- tlm_list(cbg,group=0:3,formula="y ~ I(t^2) + t",FUNy=log)
tlcl
plot(tlcl)

tlcl <- tlm_list(cbg,group=0:3,formula="y ~ I(t^2) + t",FUNy=log,FUNyhat=exp)
tlcl
plot(tlcl)

}
########################################################################################################################—•°
rm(dummy)