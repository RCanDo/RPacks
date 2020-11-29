## ---------------------------------------------------------------------------------------------------------------------—•°
## prediction.smooth_gam() method.
## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {EfficiencyCurves}
##  prediction.smooth_gam()
##
## DEPENDENCIES
##
## REMARKS/WARNINGS
##
## TODO
## •
##
## DESCRIPTION
##  The aim, the idea, problems, solutions... briefly!, concerning the whole set of functions.
##
## -------------------------------------------------------------------------------------------------—•°
## AUTHOR: Arkadiusz Kasprzyk; akasp@...
## ver. 1.0
## PATH: D:\ROBOCZY\...
## start: 2017-10-01    last: 2017-10-01
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
prediction.smooth_gam <- function( sg , newdata
                                 , group  = NULL , amount = NULL    ##
                                 , regroup = NULL
                                 , weights = NULL
                                 , lengthen = TRUE
                                 , ...  ){
## ---------------------------------------------------------------------------------------------------------------------—•°
   ## We are given two investment portfolios divided in groups (sub-portfolios) of the same types (e.g. balance groups):
   ##   • reference for which historical yields/payments are given,
   ##   • valuated for which prediction (within respective groups) is needed on the basis of reference data.
   ## Calculating this prediction is the aim of this function.
   ##
##		Arguments
##  sg           object of class "smooth_gam" or "smooth_gam.curves_by_groups" or "smooth_gam...."                   ???
##  newdata       data frame having at least 2 variables: "group", "amount";
##                'amount' is summed up within each group to obtain total amount for each group and by this
##                the contribution of each group into the whole portfolio.
##                In case of unnamed vector 'newdata' is treated as a vector of amounts for each group,
##                recycled if necessary.
##                If vector is named then it is not recycled and weights are taken for the groups according to names.
##  group  = NULL    which varaible of newdata gives columns
##  amount = NULL    which varaible of newdata gives amount
##  regroup = NULL   TRUE/FALSE or matrix of regrouping
##  weights = NULL   weights for groups passed to prediction.curves_by_gorups()
##  lengthen = TRUE  if TRUE and  sg$smoothed_curves_len  exists then it will be applied to make prediction;
##  ...
##
## ---------------------------------------------------------------------------------------------------------------------—•°

## ------------------------------------------------------------------------------------------------—•°
## 0. Recording arguments
## ------------------------------------------------------------------------------------------------—•°
arguments = structure( list( sg = deparse(substitute(sg))
                , newdata = deparse(substitute(newdata))
                , group  = group , amount = amount
                , regroup = deparse(substitute(regroup))
                , weights = weights
                , ... = ...
) , class = c("arguments") )

messages = structure(list(),class="messages")
#prints = list()

## ------------------------------------------------------------------------------------------------—•°
## 1. Checking data integrity
## ------------------------------------------------------------------------------------------------—•°

## ------------------------------------—•°
## newdata 1°
if(is.null(newdata)){
   stop("'newdata' cannot be NULL.
Minimal input is a vector of total weights/amounts for groups in refrerence
which is internaly replicated to the proper length if necessary
(thus even one number may be passed what gives equal weight for all groups).")
}

## ------------------------------------—•°
## copies from sg;
## 1° curves
if( lengthen & !is.null(sg$smoothed_curves_len) ){
   curves <- sg$smoothed_curves_len
   if(is.null(sg$smoothed_curves_cum)){
      msg.txt <- "\"lengthend\ curves were used obtained by \"normal\" method of smooth_gam()."
   }else{
      msg.txt <- "\"lengthend\" curves were used obtained by \"cumulated\" method of smooth_gam()."
   }
}else{
   curves <- sg$smoothed_curves
   if(is.null(sg$smoothed_curves_cum)){
      msg.txt <- "\"gam\" curves were used obtained by \"normal\" method of smooth_gam()."
   }else{
      msg.txt <- "\"gam\" curves were used obtained by \"cumulated\" method of smooth_gam()."
   }
}
msg <- Message( "SmoothedCurvesType" , msg.txt )              #!! change it

if( !"net_curve" %in% colnames(curves) ){
   stop( "\"net_curve\"  in  ",arguments$sg,"  is needed to perform further calculations.
  Create new object of class \"smooth_gam\" via  smooth_gam()  but with  net_curve=TRUE  (which is default)." )
}
net_curve <- curves[,"net_curve"]    #! IF EXISTS!!!
groups <- setdiff(colnames(curves),c("net_curve","weighted_curve"))
curves <- curves[,groups]

## ------------------------------------—•°
## newdata 2°
if(is.null(dim(newdata))){  ## we treat newdata as weights for curves <=> total amounts for groups in val
   if(is.null(names(newdata))){
      newdata = rep(newdata,length=ncol(curves))
      names(newdata) = colnames(curves)
   }
   newdata = data.frame( a = newdata , g = names(newdata) )
}else{
   if(length(dim(newdata))!=2){
      stop("'newdata' must be a vector or a data.frame or 2-d matrix.")
   }
}

## ------------------------------------—•°
## copies from sg;
## 2° other
#if(is.null(sg$counts))
 counts  <- sg$counts
 amounts <- sg$trials[,groups]
 yields  <- sg$successes[,groups]

## ------------------------------------—•°
## groups
 groups <- factor(groups,ordered=TRUE)


## ------------------------------------------------------------------------------------------------—•°
## 2. Calculations by  prediction.curves_by_groups()
## ------------------------------------------------------------------------------------------------—•°

 cbg <- list( curves = curves
            , groups = groups
            , counts = counts
            , amounts = amounts
            , yields = yields
            , net_curve = net_curve
        )

 result <- prediction.curves_by_groups( cbg
               , newdata = newdata
               #, curves  = c("net","groups") , values  = "efficiency"
               , group  = NULL , amount = NULL
               , regroup = regroup
               , weights = weights
               , ... )

## ------------------------------------------------------------------------------------------------—•°
## 2. Result
## ------------------------------------------------------------------------------------------------—•°

 result$call <- match.call()
 result$arguments <- arguments

 class(result) <- union( c("prediction.smooth_gam" , "prediction" ) , class(result) )

 newMessage(result) <- msg

 result

}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°


## ---------------------------------------------------------------------------------------------------------------------—•°
## EXAMPLES ############################################################################################################—•°
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
dummy = function(){
## This is dummy function — it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## — in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
## -------------------------------------------------------------------------------------------—•°
## RELOADER — before it works you need to source("PacksAK.R"); it's best to use EfficiencyCurves.R within pack's dir.
 loadPacksAK("EfficiencyCurves")
## -------------------------------------------------------------------------------------------—•°



## ---------------------------------------------------------------------------------------------------------------------—•°
};rm(dummy)
