########################################################################################################################—•°
## smooth_gam() method for "curves_by_groups".
########################################################################################################################—•°
## FUNCTIONS HERE    {EfficiencyCurves}
##  smooth_gam.curves_by_groups()
##
## DEPENDENCIES
##  smooth_gam()           {.}
##  smooth_gam.default()   {.}
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
## PATH: D:\ROBOCZY\...
## start: 2017-03-01    last: 2017-09-26
########################################################################################################################—•°

########################################################################################################################—•°
smooth_gam.curves_by_groups <- function( cbg , weights_groups = NULL
                           , weights_times  = NULL
                           , net_curve = TRUE
                           , future = NULL
                           , horizon = NULL
                           , wiggliness = NULL
                           , method = "normal" ## "cumulated"
                           , lengthen = NULL ){
########################################################################################################################—•°
## smooth_gam method for class "curves_by_groups"
########################################################################################################################—•°

if(nrow(cbg$amounts)==1) cbg$amounts <- as.vector(cbg$amounts)

sgd <- smooth_gam.default( successes = cbg$yields ## vector or matrix of successes
                  , trials = cbg$amounts
                  , weights_groups = weights_groups
                  , weights_times  = weights_times
                  , net_curve = net_curve
                  , future = future
                  , horizon = horizon
                  , wiggliness = wiggliness
                  , method = method ## "cumulated"
                  , lengthen = lengthen
                  )
sgd$call <- match.call()
sgd$counts <- cbg$counts

# c( "call","arguments","future","horizon","lengthen","models_list","weights_groups","weights_times",
#    "counts","successes","trials","efficiencies","smoothed_curves" )


sgd$arguments <- structure( list( cbg = deparse(substitute(cbg)) #= NULL
               , weights_groups = deparse(substitute(weights_groups)) #= NULL
               , weights_times = deparse(substitute(weights_times)) #= NULL
               , net_curve = net_curve
               , future = future
               , horizon = horizon #= 120
               , wiggliness = wiggliness #= NULL
               , method = method #= "normal" ## "cumulated"
               , lengthen = lengthen
)
, class = "arguments")

class(sgd) <- union( "smooth_gam.curves_by_groups" , "smooth_gam" )
sgd

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

cbg  ## take it from EXAMPLES of curves_by_groups.R
plot(cbg,legend_pos=c("topright","topleft"))

sg.cbg <- smooth_gam( cbg )#, weights_groups = rep(1,4) )
sg.cbg
plot(sg.cbg)
plot(sg.cbg,ylim=c(0,.3))

models_table(sg.cbg)

summary(sg.cbg)

########################################################################################################################—•°
};rm(dummy)
