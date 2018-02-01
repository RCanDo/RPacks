########################################################################################################################—•°
## FUNCTIONS HERE    {EfficiencyCurves}
##  curves_by_groups()
##
## DEPENDENCIES
##  margins()     {DM} attributes()
##  var2vec()     {DM}
##
## TODO
##
########################################################################################################################—•°

########################################################################################################################—•°
curves_by_groups = function(
    reference_df
   ,amount = NULL
   ,group = NULL
   ,time = NULL
   ,yield = NULL
   ,id = NULL
   ,horizon = NULL
   ,time_max = NULL
){
########################################################################################################################—•°
## We are given two investment portfolios divided in groups (sub-portfolios) of the same types (e.g. balance groups):
##   - reference for which historical yields/payments are given,
##   - valuated for which prediction (within respective groups) is needed on the basis of reference data.
## Calculating this prediction is the aim of this function.
##
##		Arguments
##  reference_df         reference data frame having at least five columns:
##       amount variable  —— balance on investment member/item
##                          (may be price of financial instrument, debt outstanding amount, etc.);
##       group variable   —— variable grouping members/items into sub-portfolios;
##                           if not a factor is internally coerced to factor;
##       time variable    —— periods of investment: 1, 2,... (NOT a date or exact time);
##       yield variable   —— yield (e.g. payments from investment, repayments of a debt);
##       id variable      —— member or item id (e.g. debtor id, financial instrument id);
##    The default sequence of the variables (in terms of its meanings not names which may be arbitrary)
##    in each data frame is as stated above.
##    It is also possible to indicate directly which variable in a data frame has given meaning
##    by passing its name or numeric position in the data frame to the following function variables:
##  amount
##  group
##  time
##  yield
##  id
##  horizon    this variable is optional in a sense that unlike the latter five options
##             if 'horizon' is set to NULL (default) then no variable of reference_df is set to serve as horizon.
##             If horizon variable is provided in a reference_df then it indicates the maximum time
##             which we should consider for the given id.
##             However, it is involved into calculations only if its name is passed to the 'horizon'.
##             If it is not the case then horizon is set to max(time) i.e. all times are considered.
##  time_max = what is the maximum time we'd like to calculate curves.
##
## Notice that yields/payments do not need to be grouped within id-by-period cells.
## Thus the reference data frame may contain simply a history of yields without any earlier aggregation.
## Nontheless for each yield a period in which it was made must be determined by time variable,
## i.e. time variable is not a date or exact time but rather id number of a period in which the yield was given
## (prehaps this variable should be named 'period').
##
## Notice also that this function do not check the data consistency. The following conditions should be satisfied
## but it is up to the user checking if they hold:
##  • each id should belong to only one group;
##  • each id should have only one horizon (if it is present at all) - function will work even if this condition is not
##       true as it takes max(horizon) for each id;
##  • amount should be also constant for each id - again, max(amount) is taken for each id, but it is only to make
##       program work even for inconsistent data.
##
##
##		Result
## Function returns list of class "curves_by_groups" with the following items:
##  arguments           list of values of all the arguments passed to function
##  call                how the function was called;
##  curves              matrix of groups-by-periods (from group and time variable respectively)
##                      where for reference data sum of yields is given within each time-by-group cell
##                      relative to the amount within each group (amount for each member is summed only once)
##                      at a given time; notice that if the horizon is not passed then amount is constant across time.
##  net_efficiency      vector -- net efficiency curve for reference -- it is sum of yields for each period divided by
##                      the amount of the whole portfolio at a given time, i.e. groups are not considered;
##                      notice that if the horizon is not passed then the net amount is constant across time.
##  yields              matrix of groups-by-periods (from group and time variable respectively)
##                      where for reference data sum of yields is given within each time-by-group cell;
##  groups              ordered factor obtained by unique(group); group variable is always made ordered factor;
##  counts              matrix; a table of counts unique id's (values of id) for each time within each group;
##                      if 'horizon' is passed then counts is given for each time-by-group cell;
##                      if 'horizon' is not passed then there is only one row (column, see Remarks).
##  amounts             matrix; a table of balance (amount sum) for each time within each group;
##                      if 'horizon' is passed then amounts for reference is given for each time-by-group cell;
##                      if 'horizon' is not passed then there is only one row (column).
##
##    Remarks
## There is an option of print.curves_by_groups() (a default print() method for "curves_by_groups") which alows user
## to transpose all the tables of result. If you wish to get group-by-time view rather then default time-by-group
## you need to set
## > print(cbg,as_rows=TRUE)
## The same option is available for summary.curves_by_groups() (default summary method for "curves_by_groups")
## and it can be displayed in two directions —— default time-by-group ("as columns")
## or transposed, i.e. group-by-time ("as rows"). If you wish to change default write
## > summary( cbg , ... , file = "file_name.csv", as_rows=TRUE )
##
########################################################################################################################—•°

###################################################################################################—•°
## 0. Recording arguments
###################################################################################################—•°
arguments = structure(list(  reference_df = deparse(substitute(reference_df)) ## vector or matrix of successes
               , amount = amount #= NULL
               , group = group #= NULL
               , time = time #= NULL
               , yield = yield
               , id = id #= NULL
               , horizon = horizon #= NULL
               , time_max = time_max
) , class = c("arguments") )

#messages = structure(list(),class="messages")


###################################################################################################—•°
## 1. Checking data integrity
###################################################################################################—•°

if(is.null(amount)){
  # a.val = valuated_df[,1]
   a.ref = reference_df[,1]       ## index must be integer
}else if(is.character(amount)||(is.numeric(amount)&&(floor(amount)==amount))){
 #  a.val = valuated_df[,amount]
   a.ref = reference_df[,amount]
}else{
   stop("Incorrect 'amount' identifier: must be character or integer or left NULL.
         If left NULL then the 1st variable of valuated and reference data frame will be taken as amount variable.")
}

if(is.null(group)){
  # g.val = valuated_df[,2]
   g.ref = reference_df[,2]          ## index must be integer
}else if(is.character(group)||(is.numeric(group)&&(floor(group)==group))){
  # g.val = valuated_df[,group]
   g.ref = reference_df[,group]
}else{
   stop("Incorrect 'group' identifier: must be character or integer or left NULL.
         If left NULL then the 2nd variable of valuated and reference data frames will be taken as groping variable.")
}
#if(!is.factor(g.val)){g.val = ordered(g.val)}else if(!is.ordered(g.val)){g.val = factor(g.val,levels=levels(g.val),ordered=TRUE)}
#if(!is.factor(g.ref)){g.ref = ordered(g.ref)}else if(!is.ordered(g.ref)){g.ref = factor(g.ref,levels=sort(unique(g.ref)),ordered=TRUE)}
g.ref <- as.factor(g.ref); g.ref<-ordered(g.ref)
## We end up with both factors ordered, but their levels may differ both in values and/or in order.
## This is to be solved later on.
## Levels are ordered alphabeticaly if not inherited.

if(is.null(time)){
   #t.val = valuated_df[,3]
   t.ref = reference_df[,3]      ## index must be integer
}else if(is.character(time)||(is.numeric(time)&&(floor(time)==time))){
   #t.val = valuated_df[,time]
   t.ref = reference_df[,time]
}else{
   stop("Incorrect 'time' identifier: must be character or integer or left NULL.
         If left NULL then the 3d variable of reference data frame will be taken as time variable.")
}

if(is.null(yield)){
   #y.val = valuated_df[,4]
   y.ref = reference_df[,4]       ## index must be integer
}else if(is.character(yield)||(is.numeric(yield)&&(floor(yield)==yield))){
   #y.val = valuated_df[,yield]
   y.ref = reference_df[,yield]
}else{
   stop("Incorrect 'yield' identifier: must be character or integer or left NULL.
         If left NULL then the 4th variable of reference data frame will be taken as yield variable.")
}

if(is.null(id)){
   id.ref = reference_df[,5]     ## index must be integer
}else if(is.character(id)||(is.numeric(id)&&(floor(id)==id))){
   id.ref = reference_df[,id]
}else{
   stop("Incorrect 'id' identifier: must be character or integer or left NULL.
         If left NULL then the 5th variable of reference data frame will be taken as id variable.")
}

if(is.null(horizon)){
   #h.ref = rep(max(time),length(time))     ## EXCEPTION from the above
}else if(is.character(horizon)||(is.numeric(horizon)&&(floor(horizon)==horizon))){
   h.ref = reference_df[,horizon]
}else{
   stop("Incorrect 'horizon' identifier: must be character or integer or left NULL.
         If left NULL then effectively max(time) of reference data frame will be taken as horizon variable.")
}


if(!is.null(time_max)){
   if(!(is.numeric(time_max) && floor(time_max)==time_max)){
      stop("Incorrect 'time_max' value: must be integer or left NULL.")
   }else{
      time_ok = t.ref<=time_max
       a.ref =  a.ref[time_ok]
       g.ref =  g.ref[time_ok]
       t.ref =  t.ref[time_ok]
       y.ref =  y.ref[time_ok]
      id.ref = id.ref[time_ok]
      if(!is.null(horizon)){
         h.ref = h.ref[time_ok]
         h.ref = pmin(h.ref,time_max)
      }
   }
}

###################################################################################################—•°
## 2. calculations
##
## a. - amount,   g. - group,    t. - time,  y. - yield,    id. - id,   h. - horizon
## .ref - reference

###########################################################—•°
lr = length(levels(g.ref))
nr = length(g.ref)

#######################################—•°
## aggregating AMOUNTS within each group

if(!is.null(horizon)){
   ## horizon
   a <- tapply(a.ref,list(id.ref),max)
   h <- tapply(h.ref,list(id.ref),max)
   g <- var2vec(unique(data.frame(g.ref,id.ref)),names=2)[names(a)]
   df <- data.frame(a,g,h)
   a.gt.ref = cbind(tapply(a,g,sum))
   id.gt.ref = cbind(tapply(names(a),g,length))
      ##
   for(t in 2:max(df$h)){
      a.gt.ref <- cbind( a.gt.ref , with( df[df$h>=t,] , tapply(a,g,sum,na.rm=TRUE)  ) )
     id.gt.ref <- cbind( id.gt.ref , with( df[df$h>=t,] , tapply(names(a),g,length)  ) )
   }
   colnames(a.gt.ref) = 1:max(df$h)
   colnames(id.gt.ref) = 1:max(df$h)
   a.gt.ref[is.na(a.gt.ref)] <- 0
   id.gt.ref[is.na(id.gt.ref)] <- 0

   ################—•°
   ## yields
   df <- data.frame(g = g.ref, y = y.ref, h = h.ref, tt = t.ref)
   y.gt.ref <- with( df[df$t==1,] , cbind(tapply(y,g,sum,na.rm=TRUE)))
   for(t in 2:max(df$h)){
         y.gt.ref <- cbind( y.gt.ref , with( df[df$h>=df$tt & df$tt==t,] , tapply(y,g,sum,na.rm=TRUE)  ) )
   }
   colnames(y.gt.ref) <- 1:max(df$h)
   y.gt.ref[is.na(y.gt.ref)] <- 0     ##
   y.t.ref  = colSums(y.gt.ref)
      ##
   eff.t.ref = y.t.ref / colSums(a.gt.ref)

   ## efficiency curves within groups on reference
   curves = y.gt.ref / a.gt.ref

}else{
   ## for .ref without horizon  must be made in 2 steps!!!
   a.g.ref = colSums( tapply( a.ref, list(id.ref,g.ref), max), na.rm=TRUE )   #!!!  max()
   id.g.ref = tapply(id.ref,g.ref,function(x)length(unique(x)))  ## how many unique id's for each group
   a.g.ref[is.na(a.g.ref)] <- 0
   id.g.ref[is.na(id.g.ref)] <- 0

   ####################################—•°
   ## yields
   ## aggregating yields within each time value for each group:
   y.gt.ref = tapply(y.ref,list(g.ref,t.ref),sum)     ## yields for each group across time
   y.gt.ref[is.na(y.gt.ref)] <- 0     ##
   y.t.ref  = colSums(y.gt.ref)
      ##
   eff.t.ref = y.t.ref/sum(a.g.ref)     ##

   ## efficiency curves within groups on reference
   curves = rbind(apply(y.gt.ref,2,function(x){x/a.g.ref}))

}

## efficiency curves
curves[is.nan(curves)] <- 0

###########################################################—•°

########################################—•°
###  weighted efficiency curve for valuated  -- NOT IN THIS FUNCTION!!! (but in curves_and_prediction())
###  ...

###################################################################################################—•°
## 3. final result
##

###################—•°
## time should run vertically (tradition!)
## thus we need to transpose
curves <- t(curves)
y.tg.ref <- t(y.gt.ref)  ; rm(y.gt.ref)

if(!exists("id.gt.ref")){
   id.tg.ref <- rbind(id.g.ref) ; rownames(id.tg.ref) <- "1"
}else{
   id.g.ref <- id.gt.ref[,1]
   id.tg.ref <- t(id.gt.ref)  ;  rm(id.gt.ref)
}

if(!exists( "a.gt.ref")){
   a.tg.ref <-  rbind(a.g.ref) ; rownames(a.tg.ref) <- "1"
}else{
#   a.g.ref <-  a.gt.ref[,1]
   a.tg.ref <- t(a.gt.ref)  ;  rm(a.gt.ref)
}

names(attr(curves,"dimnames")) <- list("time","group")
names(attr(y.tg.ref,"dimnames")) <- list("time","group")
names(attr(id.tg.ref,"dimnames")) <- list("time","group")
names(attr(a.tg.ref,"dimnames")) <- list("time","group")

###################—•°
## margins   ## margins is attr. defined in  DM::attributes.R
             ##  —— it shows which margin is sensible to be calculated, e.g. via addmargins()
margins(curves) <- 1       ## across time
margins(eff.t.ref) <- 2
margins(y.tg.ref) <- 1:2   ## across time & groups
margins(id.tg.ref) <- 2    ## across groups
margins(a.tg.ref) <- 2   ## across groups


########################################—•°
result = list(   call                     = match.call()
               , arguments                = arguments
               ##
               , counts                   = id.tg.ref          ## matrix
               , amounts                  = a.tg.ref           ## matrix
               , yields                   = y.tg.ref           ## matrix        ## ref_yields_groups
               ##
               , groups                   = sort(unique(g.ref))      ## factor (short)
                  ##, regroup                  = if(is.null(regroup)){regroup}else{as.table(regroup)}
               ##
                  ##, weights                  = aval_normal        ## numeric
               , curves                   = curves             ## matrix  time x groups
                  ##, weighted_curve           = weighted_curve     ## numeric
               , net_curve                = eff.t.ref          ## numeric       ## ref_net_eff_curves
                  ##, net_curve                = cbg$net_curve      ## numeric    #=eff.t.ref
                  ##, val_yields_from_weighted = y.t.val.wc        ## numeric
                  ##, val_yields_from_net      = y.t.val.net       ## numeric
                  ####
                  ##, val_yields_groups        = y.tg.val           ## matrix
             )

#messages(result) <- messages  #! no warnings in this function!
class(result) <- "curves_by_groups"

result

}  ##----END----##
########################################################################################################################—•°

########################################################################################################################—•°
## EXAMPLES ############################################################################################################—•°
########################################################################################################################—•°

########################################################################################################################—•°
dummy = function(){
## This is dummy function —— it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## —— in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
###################################################################################################—•°

 ## RELOADER —— before it works you need to source("PacksAK.R"); it's best to use EfficiencyCurves.R within pack's dir.
 loadPacksAK("EfficiencyCurves")

###################################################################################################—•°
## 1.
( ref.df = sim_ref( members = 20
                  , groups = letters[1:4]
                  , horizon = 5 + sample(5) #60     ## recycled to get length = members
                  , yield_par = c(.02,.05,.1,.15)    ## recycled to get length = groups
                  #, exp_down = FALSE
                  , amount = NULL
                  ) )

( cbg = curves_by_groups( ref.df
                        ,horizon = "horizon"
                       ) )

plot(cbg)
summary(cbg)

print(cbg,as_rows=TRUE)
summary(cbg,as_rows=FALSE)

names(cbg)

class(cbg$curves)                        ## matrix
class(cbg$net_efficiency)                ## numeric       ## ref_net_eff_curve
class(cbg$yields)                        ## matrix        ## ref_yields_groups
class(cbg$groups)                        ## ordered factor
class(cbg$counts)                        ## table
class(cbg$amounts)                       ## table
class(summary(cbg))                      ## matrix

as.table(cbg$amounts)

## plotting variants
plot(cbg)
plot(cbg,,6,12)
plot(cbg,,8,12)
plot(cbg,,10,15)
plot(cbg,"normal")
#
plot(cbg,legend_pos="bottomright")
plot(cbg,legend_pos=c("bottomleft","bottomright"))
plot(cbg,legend_pos=NULL)
plot(cbg,legend_pos=FALSE)
plot(cbg,legend_pos=c(FALSE,TRUE))
plot(cbg,legend_pos=c("","bottomright"))


## some smoothing
plot(cbg,"normal",10,8)
lines(lowess(cbg$net_efficiency),col="white")
lines(smooth.spline(cbg$net_efficiency,nknots=3),col="grey")   ## warning... ???

## more on smoothing in help for smooth_gam.curves_by_groups

   ## this is probably to remove
   curtab <- t(src$yields) ; curtab <- curtab[,-ncol(curtab)]
   curtab <- curtab[,colSums(curtab) != 0]
      ##
   smooth_curves_gam( curtab , horizon = 20 , graphics = TRUE)
   smooth_curves_gam( curtab , horizon = 20 , graphics = TRUE
                    , weights.curves = c(1,1,1,1)
                    , lengthen = function(x){x[1]*.9}
                    )


###################################################################################################—•°
## 2.  varying horizon
( ref.df = sim_ref( members = 100
                  , groups = 4
                  , horizon = c(30,40,50,60)     ## recycled to get length = members
                  , yield_par = c(.02,.05,.1,.15)   ## recycled to get length = groups
                  #, exp_down = FALSE
                  , amount = NULL
                  ) )

(cbg = curves_by_groups( ref.df
                       , horizon = "horizon"  ## horizon MUST be passed to have an effect !!!!
                       ))
plot(cbg)
summary(cbg)

## horizon for a single member indicates what maximum time value we consider for this member,
## i.e. records having time value greater then horizon are neglected.



########################################################################################################################—•°
## remnents after testing the function (to be removed soon)
## variables assigning

## not for Ex. 1.2
##
a.ref = ref.df$amount
g.ref = ref.df$group
t.ref = ref.df$time
y.ref = ref.df$yield
id.ref = ref.df$id

   h.ref = ref.df$horizon       ## for Ex. 1.3

## coercing group to ordered factor
if(!is.factor(g.val)){g.val = ordered(g.val)}else if(!is.ordered(g.val)){g.val = factor(g.val,levels=levels(g.val),ordered=TRUE)}
if(!is.factor(g.ref)){g.ref = ordered(g.ref)}else if(!is.ordered(g.ref)){g.ref = factor(g.ref,levels=levels(g.ref),ordered=TRUE)}

##############################################################################################—•°
## some check out
unique(g.val)
unique(g.ref)
c(g.val,g.ref)
# now you can go to the main function code    STAGE 2

}
########################################################################################################################—•°
rm(dummy)