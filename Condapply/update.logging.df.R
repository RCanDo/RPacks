## ---------------------------------------------------------------------------------------------------------------------•°
## FUNCTIONS HERE
##  update.logging.df()
##
## DEPENDENCIES
## update.df.r
##  update.df()
## addid.r
##  names2id() with alias   addid2df()
##  var2vec()  with alias   addid2vec()
## ---------------------------------------------------------------------------------------------------------------------•°
update.logging.df = function(  datfram      ## logging data frame
							, data                 ## vector or data frame to be used to update 'datfram'
                     , variable  = NULL     ## which variables of 'data' to use to update 'datfram';
                                    ## as numeric position or names;
                                    ## if 'data' is a vector the 'variable' value will be used as a name of the variable
                                    ## created from this vector
                     , onlyFALSE = FALSE    ## if 'data' consists only of FALSE do update logging df with this data?
                                    ## If FALSE then this 'variable' will not be added to 'datfram'
                                    ## moreover
                     , ...          ## otehr arguments passed to update.df()
							){
## ---------------------------------------------------------------------------------------------------------------------•°

if(!"logging.data.frame" %in% class(datfram)){
   stop("'datfram' is not a \"logging.data.frame\".")
}


if(is.null(data) && !is.null(variable) ){
   datfram[,variable[1]] = NULL
}else{
   datfram = update.df(datfram,data,variable,...)
}

datfram$sum.. = rowSums(datfram[,-1])

datfram

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
## -------------------------------------------------------------------------------------------•°
