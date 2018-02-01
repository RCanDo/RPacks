########################################################################################################################—•°
## smooth_gam() method for "curves_and_prediction".
########################################################################################################################—•°
## FUNCTIONS HERE    {EfficiencyCurves}
##  smooth_gam.curves_and_prediction()
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
## start: 2017-03-01    last: 2017-09-27
########################################################################################################################—•°

########################################################################################################################—•°
smooth_gam.curves_and_prediction <- function( cap #, weights_groups = NULL
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

if(nrow(cap$amounts)==1) cap$amounts <- as.vector(cap$amounts)  ## ???

sgd <- smooth_gam.default( successes = cap$ref_yields_groups ## vector or matrix of successes
                  , trials = cap$amounts[-1,]
                  , weights_groups = cap$weights
                  , weights_times  = weights_times
                  , net_curve = net_curve
                  , future = future
                  , horizon = horizon
                  , wiggliness = wiggliness
                  , method = method ## "cumulated"
                  , lengthen = lengthen
                  )

sgd$call <- match.call()
sgd$arguments <- structure( list( cap = deparse(substitute(cap)) #= NULL
               , weights_groups = paste0(deparse(substitute(cap)),"$weights") #= NULL
               , weights_times = deparse(substitute(weights_times)) #= NULL
               , net_curve = net_curve
               , future = future
               , horizon = horizon #= 120
               , wiggliness = wiggliness #= NULL
               , method = method #= "normal" ## "cumulated"
               , lengthen = lengthen
)
, class = "arguments")

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

cap  ## take it from EXAMPLES of curves_by_groups.R
plot(cap,legend_pos=c("topright","topleft"))

sg.cap <- smooth_gam( cap )#, weights_groups = rep(1,4) )
sg.cap
plot(sg.cap)
plot(sg.cap,ylim=c(0,.3))

models_table(sg.cap)

summary(sg.cap)

}
########################################################################################################################—•°
rm(dummy)