########################################################################################################################—•°
## smooth_gam() generic and default.
########################################################################################################################—•°
## FUNCTIONS HERE    {EfficiencyCurves}
##  smooth_gam()
##  smooth_gam.default()
##
## DEPENDENCIES
##  coalesce()       {DM}
##  messages<-       {DM}
##  efficiency()     {.} a-misc.r
##
## REMARKS/WARNINGS
##
## TODO
## •
##
## DESCRIPTION
## Smoothing curves using gam( . , family=binomial(link=logit) ) where curves are given in a form of
## successes relative to trials, rather then ready efficiency curves.
##
####################################################################################################—•°
## AUTHOR: Arkadiusz Kasprzyk; akasp@...
## ver. 1.0
## PATH: D:\ROBOCZY\...
## start: 2017-03-01    last: 2017-09-26
########################################################################################################################—•°

########################################################################################################################—•°
smooth_gam <- function(successes
                      , ...){UseMethod("smooth_gam")}
########################################################################################################################—•°

########################################################################################################################—•°
smooth_gam.default = function( successes  ## vector or matrix of successes
                           , trials = NULL
                           , weights_groups = NULL
                           , weights_times  = NULL
                           , net_curve = TRUE
                           , future = NULL
                           , horizon = NULL
                           , wiggliness = NULL
                           , method = "normal" ## "cumulated"
                           , lengthen = NULL
                           ) {
########################################################################################################################—•°
## Smoothes curves using gam( . , family=binomial(link=logit) ).
## Curves are given in a form of successes relative to trials, rather then ready efficiency curves.
## This is because of the way gam( . , family=binomial(link=logit) ) works
## — successes and failures need to be passed separately.
## If successes and trials are given in a form of matrix then an "efficiency curve" (history of successes)
## is assumed to be a column of successes relative to a (respective) column of trials.
## Each column of successes (together with respective column of trials) constitues a different curve
## with time variable being simply an order numbers of a consecutive rows of successes' matrix.
## Respective columns passes information about separate "groups" of cases.
## The aim of the function is to "smooth" each curve using gam( . , family=binomial(link=logit) ),
## i.e. construct a gam model for success rate (with logit-link)
## and on this basis make a prediction for future success rates.  
## If desired then a weighted smoothed curve is calculated from all smoothed curves
## where weights are given by the user.
## By default the net curve is calculated i.e. successes ratio for each time where ratio is given for
## sum of all successes wrt. sum of all trials across columns (groups).
## 
##    Arguments
##  successes         vector or matrix of successes. If vector of length N then is coerced to Nx1 matrix.
##  trials            vector or matrix of trials. If vector then should have length ncol(succeses) or nrow(succeses) and
##                    then is replicated as row or column (respectively) to match the size of 'succeses'.
##                    In case of equal number of columns and rows vector of trials is treated as row.
##                    If matrix than its size must match the size of 'successes'.
##                    If NULL (default) then is set to colSums(successes).
##  weights_groups    weights to calculate the weighted smoothed curve.
##                    May be considered as the importance of each curve for the calculation of a final curve.
##                    If left NULL then weighted curve WILL NOT BE CALCULATED.
##                    If length(weights_groups) doesn't match ncol(successes) then is recycled/truncated
##                    to this length. Thus it's enough to pass any single (positive) number to obtain
##                    weighted curve (then all weights are the same).
##  weights_times     weights passed to gam(); may be seen as the weights of each time point in calculating
##                    the model for given curve; should have the same dim as successes or have 
##                    length(weights_times) == nrow(successes)  (or == length(successes)).
##  net_curve         if TRUE (default) then "net curve" is calculated, i.e. efficiency curve
##                    for all successes and trials summed up across groups (columns).
##  future            the period length for the future observation we wish to have smoothed curves predicted, see below;
##                    if left NULL (default) then is internally set up to 12 what is its effective default.
##  horizon           time horizon to perform prediction
##                         horizon = maxtime + future   where   maxtime = nrow(successes) or = length(successes).
##  wiggliness        wiggliness of the s() term in gam();
##                    if left NULL then wiggliness is set up automatically by gam().
##  method            if "normal" (default) then gam() prediction is performed on raw data;
##                    if "cumulated" then prediction is performend on cumulated efficiencies,
##                    then prediction curves are differentiated to give success ratio curves.
##                  if "mixed" then "normal" method is applied for 1:maxtime while "cumulated" for maxtime+1:future.
##  lenghten          a function applied to [ x_{t-1}, x_{t-2}, ...] to obtain x_t when lengthening the smoothed curve.
##                    This provides an alternative method to make a prediction for future success rates
##                    (future t means nrow(successes) < t <= horizon),
##                    while the method based on gam prediction is preserved.
##                    Default is NULL and then only gam predicion is performed. If not NULL then
##                    result will have another entry called 'smoothed_curves_len' and two types of smoothed curves
##                    will be plotted — the normal one with dashed lines and the lengthened one with solid lines.
##                    The function should be passed like
##                       lengthen = function(x){f(x)}  where f() is any R function or user formula based on R functions,
##                    while x is a vector  [ x_{t-1}, x_{t-2}, ..., x_{1} ]  where  nrow(successes) < t <= horizon.
##                    Thus e.g. if you wish to pass an AR(2) with coeff's a1=.6 and a2=.1 then it should be stated as
##                       lengthen = function(x){ .6*x[1] + .1*x[2] } .
##                    Notice that function cannot depend on x longer then nrow(successes).
##                    You may also pass a numeric vector, like c(.6,.1) in the example above, but then result$lengthen
##                    is closure which means its parameters are not printed explicitely and are accesible only via
##                       environment(result$lengthen)$a
##
##  file              name of the file to write results (with expansion, e.g. .csv); character vector of which
##                    the first element is taken as the name of the file to write "normal" curve,
##                    the second is for "lengthened" and the third is for "cumulated" (if the latter two exists).
##                    The vector is internally turned into a named list — according to order just mentioned
##                    or according to the vectors' names if they are given (proper names are
##                    "normal", "lengthen", "cumulated" or any shortened version — only the first letter of each name
##                    is considered.). Thus the 'file' may be passed also as a list (named or unnamed).
##                    If left NULL no result will be written.
##  as_rows = FALSE   an orientation in which the tables should be written to a file:
##                    if default FALSE then NOT as rows (group-by-time) but transposed (time-by-group).
##
##    Result
## List having class "smooth_gam" with the following elements:
##  arguments         list of values of all the arguments passed to function.
##  call              how the function was called;
##  method          one of "normal", "cumulated" or "mixed".
##  lengthen          function passed to 'lengthen' or created from numeric 'lengthen'.
##  models_list       list of gam models — each model for one curve.
##  counts            may be thought of as a number of experiments; it does not take part in any calculations;
##                    For now it is a slot for remembering $counts entry from "curves_by_group" object which may be
##                    passed to dedicated smooth_gam() method and result of it is passed further to prediction() which
##                    needs $counts. In default method  counts = trials;
##  trials            matrix of trials as calculated according to rules described above.
##  successes         matrix of successes as passed to function.
##  weights_groups    effective (i.e. normalised) weights for groups.
##  weights_times     weights for times passed do gam() as 'weights'.
##  smoothed_curves   matrix of smoothed curves — each column for one curve.
##
## If 'lengthen' is passed then additional object is included
##  smoothed_curves_len    matrix of smoothed curves lenghtend according to 'lengthen'.
##
## If method = "cumulated" then additional three objects are included
##  successes_cum          matrix of cumulated successes.
##  efficiencies_cum       matrix of efficiencies for cumulated successes.
##  smoothed_curves_cum    matrix of smoothed cumulated curves of success ratio.
##
########################################################################################################################—•°

require(gam)

###################################################################################################—•°
## 0. Recording arguments
###################################################################################################—•°
arguments = structure( list(  successes = deparse(substitute(successes)) ## name of a vector or matrix of successes
               , trials = deparse(substitute(trials)) #= NULL
               , weights_groups = deparse(substitute(weights_groups)) #= NULL
               , weights_times = deparse(substitute(weights_times)) #= NULL
               , net_curve = net_curve
               , future = future
               , horizon = horizon #= 120
               , wiggliness = wiggliness #= NULL
               , method = method #= "normal" ## "cumulated"
               , lengthen = lengthen #= NULL
            ), class="arguments")

messages = structure(list(),class="messages")

   #prints = structure(list(),class="prints")

###################################################################################################—•°
## 1. Checking data integrity
###################################################################################################—•°

#######################################—•°
## successes

if(is.null(dim(successes))){
   dim(successes) = c(length(successes),1)
}

if(is.null(colnames(successes))){
   colnames(successes) <- paste0("group_",1:ncol(successes))
}
successes = round(successes)

ngr0 <- ncol(successes)    ## needed for weights_groups section

#######################################—•°
## time

maxtime = nrow(successes)
times = 1:maxtime

if(is.null(horizon)){
   if(is.null(future)) future = 12
   horizon = maxtime + future
}else{
   if(is.null(future)){
      future = horizon - maxtime
   }else{
      if(horizon != maxtime + future){
         horizon = maxtime + future
         messages$time <- Warning( "NonAgreeingFutureAndHorizon",
"Values of 'future' and 'horizon' do not agree, i.e. horizon != nrow(successes) + future.
You may set any of them to NULL to avoid inconsistency.
If both are NULL (default) then 'future' is set to 12.
Now the value of horizon is set to  nrow(successes) + future." )
         warning(messages$time)
      }
   }
}
if(horizon < 3){
   stop("horizon < 3 but 3 is minimum length to make any smoothing.")
}

#######################################—•°
## names
cnams <- colnames(successes)
dimnames(successes) <- list(time=times,group=cnams)

#######################################—•°
## trials
messages$trials = ""
if(is.null(trials)){
   trials = colSums(successes)     ## dim = NULL
   messages$trials = paste( messages$trials , "'trials' were not passed thus were taken as colSums(successes)." , sep="\n" )
}


if(is.null(dim(trials))){
   if( length(trials)==ngr0 ){
      trials = outer(rep(1,maxtime),trials)
      messages$trials = paste( messages$trials , "'trials' used as given for columns." , sep="\n" )
   }else if( length(trials)==maxtime ){
      trials = outer(trials,rep(1,ngr0))
      messages$trials = paste( messages$trials    ## in this case should be always empty
                             , "'trials' used as given for rows." , sep="\n" )
   }else{
      stop("If 'trials' have NULL dimensions then must have length
equal to ncol(successes) or nrow(successes).")
   }
}else if( any(dim(successes)!=dim(trials)) ){
   stop( "Dimensions of 'successes' and 'trials' must be equal or 'trials' may have NULL dimensions
but length equal to ncol(successes) or nrow(successes)." )
}else{
   messages$trials = paste( messages$trials       ## in this case should be always empty
, "'trials' used as given for each column and row directly." , sep="\n" )
}

trials = round(trials)

dimnames(trials) <- list(time=times,group=cnams)

#######################################—•°
## net_curve

if(net_curve){
   successes <- addmargins(successes,2)
      trials <- addmargins(trials,2)
   colnames(successes) <- colnames(trials) <- cnams <- c(cnams,"net_curve")
}

ngr = ncol(successes)      ##!!!!!!!!!

#######################################—•°
## CUMULATED successes
successes0 = successes     ##
trials0 = trials
if(method=="cumulated"){
   if(!all(apply(trials,2,function(x){min(x)==max(x)}))){  ## not all equal in a column
      effs <- successes / trials
      trials <- apply(trials,2,function(x)rep(max(x),maxtime))
      successes <- round(trials*effs)
   }
   successes <- apply(successes,2,cumsum)

   dimnames(successes) <- list(time=times,group=cnams)
}

#######################################—•°
## trials vs successes
if(any(trials<successes)){
#      which_less = which(trials<successes)
#      trials_less = trials[which_less]
   trials = pmax(trials,successes)
   messages$trials = paste( messages$trials , "There are cases where number of trials is less then number of successes.
Number of trials was increased to the number of successes and efficiency is 1 for these cases." , sep="\n" )   #"
}
messages$trials <- Message("trials",messages$trials)

#######################################—•°
## weights_times
messages$weights_times = ""

      # prints$weights_times = coalesce( weights_times , paste( "rep( 1," , nrow(successes) , ")" ) )

if(is.null(weights_times)){
   weights_times = rep(1,maxtime)
   messages$weights_times = paste( messages$weights_times
                                 , "'weights_times' were not passed thus were set up to 1s." , sep="\n" )
}
if(is.null(dim(weights_times))){
   if( length(weights_times)==maxtime ){
      weights_times = outer(weights_times,rep(1,ngr))
      messages$weights_times = paste( messages$weights_times
                                    , "'weights_times' are the same for all groups." , sep="\n" )
   }else{
      stop("If 'weights_times' have NULL dimensions then must have length equal to nrow(successes).")
   }
}else if( any(dim(successes)!=dim(weights_times)) ){
   if( net_curve && ncol(weights_times)==ngr-1 ){
      weights_times <- cbind(weights_times,rep(1,maxtime))
      messages$weights_times = paste( messages$weights_times , "'weights_times' for 'net_curve' is set up to 1s." , sep="\n" )
   }else if( !net_curve && ncol(weights_times)==ngr+1 ){
      weights_times <- weights_times[,-ncol(weights_times)]
      messages$weights_times = paste( messages$weights_times , "'weights_times' for 'net_curve' is removed as net_curve=FALSE." , sep="\n" )
   }else{
      stop( "Dimensions of 'successes' and 'weights_times' must be equal or
'weights_times' may have NULL dimensions but length equal to nrow(successes)." )
   }
}else{
   messages$weights_times = paste( messages$weights_times       ## in this case should be always empty
                                  , "'weights_times' used as given for each column and row directly." , sep="\n" )
}

dimnames(weights_times) <- list(time=times,group=cnams)

messages$weights_times <- Message("weights_times",messages$weights_times)

#######################################—•°
## weights_groups
messages$weights_groups = ""
if(is.null(weights_groups)){
   wc = FALSE  #weights_groups = trials[1,]
   messages$weights_groups = paste( messages$weights_groups
                                 , "'weights_groups' were not passed hence weighted curve was not calculated."
                                 , sep="\n" )
}else{
   wc = TRUE

   if(length(weights_groups)!=ngr0){
      weights_groups = rep(weights_groups,length=ngr0)
      messages$weights_groups = paste( messages$weights_groups
                                    , "length(weights_groups) didn't match ncol(successes) and was replicated/truncated to this length."
                                    , sep="\n" )
   }
   weights_groups = weights_groups/sum(weights_groups)  ## normalisation of weights
   #messages$weights_groups = paste( messages$weights_groups , "'weights_groups' properly passed and weighted curve was derived." , sep="\n" )
   names(weights_groups) <- cnams[1:ngr0]
}

messages$weights_groups <- Message("weights_groups",messages$weights_groups)

#######################################—•°
## lengthen  function not NULL
len <- !is.null(lengthen) && (horizon > maxtime)
if(len && !is.function(lengthen) ){
   if(is.numeric(lengthen)){
      a <- lengthen
      lengthen <- function(x){sum(a*x[1:length(a)])}
   }else{
      stop("'lengthen' must be function or numeric or left NULL.")
   }
}


####################################################################################################—•°
## 2. Estimation
####################################################################################################—•°
   
if(!is.null(wiggliness)){
   wiggliness = rep(wiggliness,length = ngr)
   names(wiggliness) = cnams
}
 
smoothed.curves = cbind(1:horizon)
models.list = list()
mod_k = list()

for(k in cnams){  #1:ngr){    #         succesess , failures
mod_k<-tryCatch({
   resp = as.matrix( cbind(successes[,k] , trials[,k] - successes[,k]) )
   wigg = ifelse(is.null(wiggliness),"",paste0(",",wiggliness[k]))
   formula_k = as.formula(paste0("resp ~ s(times",wigg,")"))
   model_k <- gam::gam( formula_k , family = binomial(link=logit) , weights = weights_times[,k])
   s_curve_k <- predict( model_k , data.frame(times = c(1:horizon)) , type="response" )
   list("model_k"=model_k,"s_curve_k"=s_curve_k)
      ##
      }
   ,error = function(e){
   warn <- Warning("ModelWarning",paste0("Model \"",k,"\" is done by lm()."))  #"
   warning(warn)
   resp = successes[,k]
   formula_k = as.formula("resp ~ times")
   model_k <- lm( formula_k , weights = weights_times[,k])
   s_curve_k <- predict( model_k , data.frame(times = c(1:horizon)) )
   list("model_k"=model_k,"s_curve_k"=s_curve_k,"warn"=warn)
   }
   #, finally = {print(k) ; print(mod_k)}
   )
models.list[[k]] <- mod_k$model_k
smoothed.curves <- cbind(smoothed.curves, mod_k$s_curve_k)
if(!is.null(mod_k$warn)){
   messages <- c(messages,list(mod_k$warn))
}
}

smoothed.curves = smoothed.curves[,-1,drop=FALSE]

###########################################################—•°
if(method=="cumulated"){

   smoothed.curves.cum <- smoothed.curves
   if(wc){ smoothed.curves.cum <- cbind(smoothed.curves.cum,smoothed.curves.cum[,1:ngr0]%*%cbind(weights_groups)) }## weights_groups  has been already normalised
   smoothed.curves = rbind( smoothed.curves.cum[1,] , diff(smoothed.curves.cum) )
   rownames(smoothed.curves.cum) <- 1:horizon
   if(wc){ colnames(smoothed.curves.cum) <- c(cnams,"weighted_curve")
   } else{ colnames(smoothed.curves.cum) <- cnams }

}else{  ## "normal"
   if(wc){ smoothed.curves = cbind(smoothed.curves,smoothed.curves[,1:ngr0]%*%cbind(weights_groups)) }
}

###########################################################—•°

#weighted_curve = as.vector(smoothed.curves[,-1]%*%cbind(weights_groups))/sum(weights_groups)
#smoothed.curves = cbind(smoothed.curves,weighted_curve)

rownames(smoothed.curves) <-  1:horizon
if(wc){
   colnames(smoothed.curves) <- c(cnams,"weighted_curve")
} else {
   colnames(smoothed.curves) <- cnams
}

if( len ){
   smoothed.curves.len = smoothed.curves
   for(t in max(times):(horizon-1)){
      smoothed.curves.len[t+1,] = apply(apply(smoothed.curves.len[1:t,,drop=FALSE],2,rev),2,lengthen)
   }
}

###################################################################################################—•°
### Result
###################################################################################################—•°

###############################################################################—•°
## preparation

#######################################—•°
## do not print everything (using compact() from DM package)

if(max(weights_times)==min(weights_times)){  ## all equal
   compact(weights_times) <- 3
}else if(all(apply(weights_times,1,function(x){min(x)==max(x)}))){   ## all equal in a row (the same time weights for all groups)
   compact(weights_times) <- nrow(weights_times)
}else{}  ## do nothing in any other case


#######################################—•°
## smoothed_curves_...
if(wc){
   dimnames(smoothed.curves) <- list(time=1:horizon,group=c(cnams,"weighted_curve"))
   if(len){
      dimnames(smoothed.curves.len) <- list(time=1:horizon,group=c(cnams,"weighted_curve"))
   }
   if(method == "cumulated"){
      dimnames(smoothed.curves.cum) <- list(time=1:horizon,group=c(cnams,"weighted_curve"))
   }
}else{
   dimnames(smoothed.curves) <- list(time=1:horizon,group=cnams)
   if(len){
      dimnames(smoothed.curves.len) <- list(time=1:horizon,group=cnams)
   }
   if(method == "cumulated"){
      dimnames(smoothed.curves.cum) <- list(time=1:horizon,group=cnams)
   }
}
#######################################—•°
## margins

margins(smoothed.curves) <- 1

if(net_curve){
   margins(successes0) <- 1
   margins(trials0) <- 1
}else{
   margins(successes0) <- 1:2
   margins(trials0) <- 1:2
   ## needed only in case of "cumulated" and then  margins(.) <- 1  is not sensible
   margins(successes) <- 2
   margins(trials) <- 2
   cnams <- c(cnams,"Sum")
}

#######################################—•°
## efficiencies

efficiencies0 <- tm(successes0)/tm(trials0)
efficiencies <- tm(successes)/tm(trials)

if(wc){
   efficiencies0 <- cbind(efficiencies0,efficiencies0[,1:ngr0]%*%cbind(weights_groups))
   efficiencies <- cbind(efficiencies,efficiencies[,1:ngr0]%*%cbind(weights_groups))

   dimnames(efficiencies0) <- list(time=c(times,"Sum"),group=c(cnams,"weighted_curve"))
   dimnames(efficiencies)  <- list(time=times,group=c(cnams,"weighted_curve"))
}else{
   dimnames(efficiencies0) <- list(time=c(times,"Sum"),group=cnams)
   dimnames(efficiencies)  <- list(time=times,group=cnams)
}

#######################################—•°
## making all matrices a "tm" class
##
## This is to have proper printing of them;
## "tm" class need to have one of the attributes: margin, transpose, head, round;
## giving transpose(x) <- FALSE you make object "tm" while not changing its printing (won't be transposed).

transpose(successes0) <- FALSE
transpose(successes) <- FALSE
transpose(trials0) <- FALSE
transpose(trials) <- FALSE
transpose(efficiencies0) <- FALSE
transpose(efficiencies) <- FALSE
transpose(smoothed.curves) <- FALSE

###############################################################################—•°
## output

result = list( #prints = prints  ,
              call = match.call()
            , arguments = arguments
            , future  = future
            , horizon = horizon
            , lengthen = eval(lengthen)       ## as string in arguments$lengthen
            , models_list = structure(models.list,class=c("models_list_gam","models_list"))
            , weights_groups = weights_groups
            , weights_times  = weights_times
            , counts = trials0
            , trials = trials0
            , successes = successes0
            , efficiencies = efficiencies0
            , smoothed_curves = smoothed.curves
            )
if(len){
   transpose(smoothed.curves.len) <- FALSE
   margins(smoothed.curves.len) <- 1
   result$smoothed_curves_len = smoothed.curves.len
}
if(method == "cumulated"){
   result$successes_cum = successes
   result$efficiencies_cum = efficiencies

   transpose(smoothed.curves.cum) <- FALSE
   result$smoothed_curves_cum = smoothed.curves.cum
}

messages(result) <- messages
class(result) <- c("smooth_gam")

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
## — in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
###################################################################################################—•°

 ## RELOADER — before it works you need to source("PacksAK.R"); it's best to use PackName.R within pack's dir.
 loadPacksAK("EfficiencyCurves")

###################################################################################################—•°
n=20 ; g=5 ; maxs=100
successes <- data.frame(row.names=1:n)
for(k in 1:g){ successes <- cbind(successes , rev(sort(sample(20:maxs,n,replace=TRUE))) + sample(1:20,n,replace=TRUE) + sample(-20:20,1)) }
   colnames(successes) <- paste0("g_",1:g)
   successes

sg <- smooth_gam( successes )  ## the simplest call
sg
plot(sg)
print(sg,as_rows=TRUE)
summary(sg)
plot(sg,add_points=FALSE)
plot(sg,add_points=FALSE,legend_pos="bottomleft")
plot(sg,groups=1)        ## colors preserved from the plot of all
plot(sg,groups=c(1,3))

sg <- smooth_gam( successes  ## vector or matrix of successes
                           , trials = NULL
                           , weights_groups = c(5,1,1,1,1)
                           , weights_times  = NULL
                           , future = 10
                           , horizon = NULL
                           , wiggliness = 3
                           , method =  "cumulated" #"normal" ##
                           , lengthen = function(x){.6*x[1]+.2*x[2]}
                           )


sg
print(sg,as_rows=TRUE)
summary(sg)
summary(sg)[]

efficiency(successes)
efficiency(successes,cum=TRUE)
###
   efficiency(successes,margin=1) ## nonsense!!! just to check how it works
   efficiency(successes,cum=TRUE,margin=1) ## nonsense!!! just to check how it works

cumsum(efficiency(sg))

plot(sg)
   args("plot.smooth_gam")
plot(sg,add_points=FALSE)
plot(sg,add_points=FALSE,legend_pos="bottomleft")
plot(sg,groups=1)


#######################################—•°
## future & horizon
smooth_gam( successes )
smooth_gam( successes , future =  12 )  ## the same as default
smooth_gam( successes , future = -10 )  ## may be negative
smooth_gam( successes , future = -18 )  ## but not to much!   ERROR
smooth_gam( successes , future = -17 )  ## 3 must be left
smooth_gam( successes , horizon = 32 )  ## = nrow(successes) + future [default]
smooth_gam( successes , horizon = 30 )
smooth_gam( successes , horizon = 10 )  ## less then nrow(successes), the same as future = -10
smooth_gam( successes , horizon = 3)    ## minimal horizon possible
smooth_gam( successes , horizon = 2)    ## below 3 —— ERROR
smooth_gam( successes , future = 10 , horizon = 32 ) ## inconsistent values —— notice that 'future' prevails
smooth_gam( successes , future = 10 , horizon = 30 ) ## consistent; but it's no need to specify both parameters.

}
########################################################################################################################—•°
rm(dummy)