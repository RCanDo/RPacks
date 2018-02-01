########################################################################################################################•°
## FUNCTIONS HERE
##  smooth.curves.by.groups()
##
## DEPENDENCIES
##
## TODO
##
########################################################################################################################•°

########################################################################################################################•°
smooth.curves.by.groups = function( cbg    ## curves.by.groups object
                           , interest = 0
#                           , weights.times =NULL
#                           , horizon = 120
#                           , wiggliness = NULL
#                           , method = "cumulated" # "normal"
#                           , file = NULL
#                           , graphics = FALSE
#                           , coldef = "black"
                           , ...
                           ) {
## smoothes curves from curves.by.groups()
## Arguments
##  cbg           a result of the function curves.by.groups()
##  ...           arguments passed to gam()

## smoothing
interest.vec = (1+interest)^(0:(ncol(cbg$ref.yields.group)-1))
trials = outer(interest.vec,cbg$amounts[,1])
result = smooth.curves.gam(  successes = t(cbg$ref.yields.group[-nrow(cbg$ref.yields.group),]) ## vector or matrix of successes
                  , trials = trials
                  , weights.curves = cbg$amounts[,2]      
#                  , weights.times  = weights.times
#                  , horizon = horizon
#                  , wiggliness = NULL
#                  , method = "normal" ## "cumulated"
#                  , file = NULL
#                  , graphics = FALSE
#                  , coldef = "black"
                  , ...
                  )
result

}  ##----END----##
########################################################################################################################•°


########################################################################################################################•°
## EXAMPLES ############################################################################################################•°
########################################################################################################################•°

##############################################################################################•°
dummy = function(){
## This is dummy function - it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## - in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
##############################################################################################•°

#    y ~ 1-exp(a*t) + e
#    1-y ~ exp(a*t) + e

}
rm(dummy)