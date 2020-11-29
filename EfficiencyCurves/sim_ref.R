## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE
##  sim_ref()        {EfficiencyCurves}
##
## DEPENDENCIES
##  addid.r
##    var2vec()
##
## TODO
##
## ---------------------------------------------------------------------------------------------------------------------—•°


## ---------------------------------------------------------------------------------------------------------------------—•°
## valuated portfolio
sim_val = function(members = 100, groups = 3){
## ---------------------------------------------------------------------------------------------------------------------—•°
## Simulation function for valuated portfolio to which we want to apply efficiency curves
## (aquired from reference portfolio).
##
##    Arguments
## members = 100     number of members in the whole portfolio. It should not exceed 899999 (see below).
## groups = 3        if numeric then only the first element of it is taken and it means a number of groups
##                   to which members belong (each member belongs only to one group).
##                   Names of groups are conseuqutive letters of alphabet.
##                   If character vector then all its elements are used as names of groups.
## ---------------------------------------------------------------------------------------------------------------------—•°
   if(is.numeric(groups) && groups[1]>0 && groups[1] == floor(groups[1])){
      items = LETTERS[1:min(groups,26)]
   }else if(length(groups)>0){
      items = groups
   }else{
      stop("Parameter groups must be one integer nonnegative value
   or vector of names of levels of a factor to be simulated. ") }
   val.df = data.frame(   amount = sample( (1:10)*100 , members , replace=TRUE )
                        , group  = factor(sample( items , members , replace=TRUE ), levels=items)  )
   val.df
}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
sim_ref = function( members = 100
                  , groups = 3
                  , horizon = 4     ## recycled to get length = members
                  , yield_par = .1     ## recycled to get length = groups
                  , exp_down = TRUE
                  , amount = NULL
                  ){
## ---------------------------------------------------------------------------------------------------------------------—•°
## Simulation function for reference portfolio for which we need to calculate efficiency curves.
##
##    Arguments
## members = 100     number of members in the whole portfolio. It should not exceed 899999 (see below).
## groups = 3        if numeric then only the first element of it is taken and it means a number of groups
##                   to which members belong (each member belongs only to one group).
##                   names of groups are conseuqutive letters of alphabet.
##                   if character vector then all its elements are used as names of groups.
## horizon = 4       numeric vector recycled to get length = members; each element means the length of time for which
##                   corresponding member (according to the position number in a vector) is present in a portfolio
##                   always starting from time = 1.
## yield_par =.1     numeric vector of coefficients for yield at each time wrt amount specific for each group.
##                   Recycled to get length = groups if shorter than this.
## exp_down = TRUE   if TRUE then yields will diminish exponentialy with time (details below).
## amount = NULL     numeric vector of amounts for each member; will be recycled to get length = 'members';
##                   if NULL then is generated as  sample(c(1:10)*100,members,replace=TRUE).
##
##    Result
## Data frame which simulates a reference portfolio. It comprises the following variables:
## amount            initial amount constant for each member (id);
## group             group idendifier constant for each member;
## time              time identifiers as numbers from  1:max(horizon);  due to randomness some times may not be present
## yield             yield sampled for each time-by-id separately as
##                      rpois( lambda = amount*yield_par ) + 1  if   exp_down = FALSE
##                   or
##                      rpois( Y, ref.df$amount*yield_par*(1+yield_par)^-ref.df$time ) + 1  if   exp_down = TRUE.
## id                id number of each member sampled from the set 100000:999999;
## horizon           horizon as passed to the function or default 4.
## Order of the variables is the same as default in curves_by_groups() and curves_and_prediction().
##
##    Remarks
## Due to the inherent randomnes of the sampling procedure any of the parameters may not be retained in the final result.
## Thus number of members and groups may be less then passed to the function as well as horizon means only maximum time
## for given member which however may not be attained. Finaly, the yield_par is used only as the multiplier
## to each individual amount and such a value is used as lambda parameter of the rpois().
## Notice also that for each member there are yields generated for the times 1:max(horizon).
## The values of the 'horizon' parameter does not result in truncating the final data frame to get only times
## up to horizon (for each member). However, value of the horizon is returned in the result as the separate variable,
## so that it is easy to truncate data frame properly.
##
##
## ---------------------------------------------------------------------------------------------------------------------—•°

## members = 100 ; groups = 3 ; horizon = 4 ; yield.par = .1
##
## Simulating reference data frame

M = members
if(M>899999){
   M=899999
   warning("Member id's are sampled from the set 100000:999999 thus their number cannot exceed 899999.")
}
H = max(horizon)
horizon = rep(horizon,length=M)

if(is.null(amount)){
   amount = sample(c(1:10)*100,M,replace=TRUE)
}else{
   amount = rep(amount,length=M)
}

   if(is.numeric(groups) && groups[1]>0 ){
      items = LETTERS[1:min(floor(groups[1]),26)]
   }else if(is.character(groups) && length(groups)>0){
      items = groups
   }else{
      stop("Parameter groups must be integer nonnegative value
   or vector of names of levels of a factor 'group' to be simulated. ")
   }
group = factor( sample(items,M,replace=TRUE), levels=items)

   yield_par = rep(yield_par,length=length(items))
   names(yield_par) = items

id = sample(100000:999999,M)
time.mat = as.data.frame(matrix(rep(1:H,each=M),ncol=H)); colnames(time.mat) = 1:H      ## M x H   members[1:M] x time[1:H]
ref.df.0 = cbind(amount = amount, group = group, id = id, horizon = horizon, time.mat)
  # print(ref.df.0)

ref.df.long = reshape(ref.df.0, varying = as.character(c(1:H)) ## names of sets of variables in the wide format that correspond to single variables in long format (‘time-varying’).
                     , v.names = "time", direction = "long")

Y = M*H + rpois(1,round(M*H/10)) ## random (but moderate) deviation from  M*H
choice.id = sample( M*H, Y, replace=TRUE )  ## random yield moments for each member
ref.df = ref.df.long[choice.id,]
   yield_par = yield_par[ref.df$group]
## non-zero random yields roughly proportional to the amount for given member:
if(exp_down){
   ref.df = cbind( ref.df, yield = rpois( Y, ref.df$amount*yield_par*(1+yield_par)^-ref.df$time ) + 1 )  ## +1 to exclude 0 yields
}else{
   ref.df = cbind( ref.df, yield = rpois( Y, ref.df$amount*yield_par ) + 1 )  ## +1 to exclude 0 yields
}

ref.df[,c("amount", "group", "time", "yield", "id", "horizon")]
}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
## EXAMPLES ############################################################################################################—•°
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
dummy = function(){
## This is dummy function - it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## - in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
## -------------------------------------------------------------------------------------------—•°

 ## RELOADER —— before it works you need to source("PacksAK.R"); it's best to use EfficiencyCurves.R within pack's dir.
 loadPacksAK("EfficiencyCurves")

## -------------------------------------------------------------------------------------------—•°

## ----------------------------------------------------------------------------—•°
## 1.
( val.df = sim_val() )

## ----------------------------------------------------------------------------—•°
## 1.
( ref.df = sim_ref() )

## testing
with(ref.df,  length(unique(id)) )
with(ref.df,  addmargins(table(id,time)) )
with(ref.df,  tapply(id,group,function(x){length(unique(x))}) )
## each member in one group
with(ref.df,  apply(table(id,group),1,function(x)sum(x>0)) )  ##
with(ref.df,  any(apply(table(id,group),1,function(x)sum(x>0))>1) )  ## FALSE  OK!
##
with(ref.df,  addmargins(table(group,time)) )

## ----------------------------------------------------------------------------—•°
## 2.
( ref.df = sim_ref( members = 10 , groups = 4 , horizon = 6 , yield_par = .1   ) )

## testing
with(ref.df,  length(unique(id)) )
with(ref.df,  addmargins(table(id,time)) )
with(ref.df,  tapply(id,group,function(x){length(unique(x))}) )
## each member in one group
with(ref.df,  apply(table(id,group),1,function(x)sum(x>0)) )  ##
with(ref.df,  any(apply(table(id,group),1,function(x)sum(x>0))>1) )  ## FALSE  OK!
##
with(ref.df,  addmargins(table(group,time)) )

## ----------------------------------------------------------------------------—•°
## 3.  varying horizon
( ref.df = sim_ref( members = 10 , groups = 4 , horizon = c(4,6,8), yield_par = .2 , amount = (1:4)*1000  ) )

with(ref.df , ref.df[order(id,time),] )

## testing
with(ref.df,  length(unique(id)) )
with(ref.df,  colSums(table(id,amount)>0) )
with(ref.df,  addmargins(table(id,time)) )
with(ref.df,  tapply(id,group,function(x){length(unique(x))}) )
## each member in one group
with(ref.df,  apply(table(id,group),1,function(x)sum(x>0)) )  ##
with(ref.df,  any(apply(table(id,group),1,function(x)sum(x>0))>1) )  ## FALSE  OK!
##
with(ref.df,  addmargins(table(group,time)) )

## If you need to truncate according to horizon
with( rdf <- ref.df[ref.df$time<=ref.df$horizon,] ## it's enough!
      , rdf[order(id,time),] )                    ## just to see better

ref.df$group

## ----------------------------------------------------------------------------—•°
## 4.  varying horizon
( ref.df = sim_ref( members = 50 , groups = letters[2:5] , horizon = sample(8:11,10,replace=TRUE) , yield_par = .1 ) )
## horizon for a single member indicates what maximum time value we consider for this member,
## i.e. records having time value greater then horizon are neglected.

   with( rdf <- ref.df[ref.df$time<=ref.df$horizon,] , {
                     print(tapply(amount,list(id,time,group),max))
                     print(tapply(amount,list(id,time),max))
                     print(tapply(time,list(id),max))
                  } )


   a.t.df <- with( ref.df , { a <- tapply(amount,list(id),max)
                  ; h <- tapply(horizon,list(id),max)
                  #; t2 <- tapply(group,list(id),function(x)x[1])
                  ; g <- var2vec(unique(ref.df[,c("group","id")]),names=2)[names(a)]
                  ; df <- data.frame(a,g,h)
                  #; lapply( 1:max(df$h) , tapply(a,,sum,data=ref.df[]) )
                  ; a.t.df = cbind(tapply(a,g,sum))
                  ; for(t in 2:max(df$h)){ a.t.df <- cbind( a.t.df , with( df[df$h>=t,] , tapply(a,g,sum,na.rm=TRUE)  ) ) }
                  ; colnames(a.t.df) = 1:max(df$h)
                  ; a.t.df[is.na(a.t.df)] <- 0
                  ; a.t.df ##print(as.matrix(a.t.df))
                  } )
   class(a.t.df)

}
## ---------------------------------------------------------------------------------------------------------------------—•°
rm(dummy)
