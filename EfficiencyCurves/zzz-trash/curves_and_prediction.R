########################################################################################################################—•°
## FUNCTIONS HERE    {EfficiencyCurves}
##  curves_and_prediction()
##
## DEPENDENCIES
##  merge_factors()     {DM} merge_factors.R
##  margins()           {DM} attributes()
##  round.attr()           "
##  Warning()           {DM} conditions()
##  messages()             "
##
## TODO
##
########################################################################################################################—•°

########################################################################################################################—•°
curves_and_prediction = function(
     valuated_df , reference_df
   , amount = NULL
   , group = NULL
   , time = NULL
   , yield = NULL
   , id = NULL
   , horizon = NULL
){
########################################################################################################################—•°
## We are given two investment portfolios divided in groups (sub-portfolios) of the same types (e.g. balance groups):
##   - reference for which historical yields/payments are given,
##   - valuated for which prediction (within respective groups) is needed on the basis of reference data.
## Calculating this prediction is the aim of this function.
##
##		Arguments
##  valuated_df          valuated data frame having at least two columns:
##                        amount variable, 
##                        group variable;
##  reference_df         reference data frame having at least five columns:
##                        amount variable  -- balance on investment member/item 
##                                            (may be price of financial instrument, debt outstanding amount, etc.)
##                        group variable   -- variable grouping members/items into sub-portfolios; 
##                                            if not a factor is internally coerced to factor
##                        time variable    -- periods of investment: 1, 2,... (NOT a date or exact time)
##                        yield variable   -- yield (e.g. payments from investment, repayments of a debt)
##                        id variable      -- member or item id (e.g. debtor id, financial instrument id);
## The default sequence of the variables (in terms of its meanings not names which may be arbitrary)
## in each data frame is as stated above.
## It is also possible to indicate directly which variable in the data frame has given meaning
## by passing its name or numeric position in a data frame to the following function variables:
##  amount
##  group
##  time
##  yield
##  id
## There is also argument
##  horizon
## which is optional and if left NULL (default) then is not considered. It indicates numeric variable which for each id
## gives the maximum 'time' value to be taken into account, i.e. all records having 'time' greater then 'horizon'
## are ommited. Notice that if there is inconsistency in 'horizon' variable for any given 'id' value
## then the maximum of 'horizon's for this 'id' are taken. If 'horizon' is not passed then there is one value
## of 'horizon' for all 'id's and it is the maximmum of 'time' across the whole data frame.
##
## Notice that yields/payments do not need to be grouped within id-by-period cells.
## Thus the reference data frame may contain simply a history of yields without any earlier aggregation.
## Nontheless for each yield a period in which it was made must be determined by time variable,
## i.e. time variable is not a date or exact time but rather id number of a period in which the yield was given
## (prehaps this variable should be named 'period.var').
##
##		Result
## Function returns list with the following items:
##  curves              matrix of groups-by-periods (from group and time variable respectively)
##                      where for reference data sum of yields is given within each group-by-period cell
##                      relative to the amount within each group (amount for each member is summed only once)
##                      at a given time; notice that if the horizon is not passed then amount is constant across time.
##  weighted_curve      vector —— a weighted sum of curves given for groups present in valuated portfolio,
##                      where weights are amounts within these groups relative to their sum.
## calculating weighted curve with only amounts for these groups wich are common for both portfolios
##                      Notice that
##!                           weighted curve can be calculated with only amounts for these groups
##!                           wich are common for both portfolios
##                      i.e. there are NO curves for groups absent in reference —— they only may be copied from
##                      other groups present in reference —— we call it "curves reweighting".
##                      Curves may be reweighted using function curves.reweight() (under construction).
##  val_yields_from_weighted  vector —— yields on valuated p. predicted as
##                               a.val * weighted_curve        where  a.val := 'total amount of valuated';
##                      This will be equal to  y.t.val (see description of val_yields_groups below)
##                      only if all groups of valuated are present in reference.
##                      If not then its values will be in general greater then those of  y.t.val .
##  ref_net_eff_curve   vector —— net efficiency curve for reference —— it is sum of yields for each period divided by
##                      the amount of the whole portfolio at a given time, i.e. groups are not considered;
##                      notice that if the horizon is not passed then amount is constant across time.
##  val_yields_from_net_eff   vector —— yields on valuated p. predicted as
##                               a.val * ref_net_eff_curve     where  a.val := 'total amount of valuated';
##                      This will be equal to  y.t.val (see description of val_yields_groups below)
##                      only if all groups of valuated are present in reference
##                      and contribute to the total amount of portfolio in the same proportions (what is v. rare).
##                      If this is not the case then its values will be most probably somwhere between
##                               y.t.val  and  val_yields_from_weighted .
##                      If all groups of valuated p. are present in reference then  y.t.val = val_yields_from_weighted
##                      and they should be used.
##                      However, if some groups from valuated are absent in reference then
##                            none of these three curves is proper !!!
##                      curves.reweight() may help to solve such problem.
##  ref_yields_groups   matrix of groups-by-periods (from group and time variable respectively)
##                      where for reference data sum of yields is given within each group-by-period cell;
##  val_yields_groups   matrix of groups-by-periods (from group and time variable respectively)
##                      where for valuated portfolio sum of yields is given within each group-by-period cell
##                      predicted from amounts * curves for each group (see table 'amounts' below and 'curves' above).
##                      Notice that if the group is absent in reference then respective curve is 0 thus
##                      predicted yield is 0 too; see 'weighted curve' above
##                      (function curves.reweight() to solve this problem).
##                      The last row is a
##                               y.t.val  :=  'sum of predicted yields for each time'
##                      and should be compared with the following two curves (described above)
##                         val_yields_from_weighted
##                         val_yields_from_net_eff
##                      They predict yields in a different way.
##  levels_as_numeric   table of numerical representation of groups (factor) levels for both portfolios (data frames)
##  counts              table of counts unique id's (values of id) within each group for each data frame;
##                      if 'horizon' is passed then counts for reference is given for each group by time cell;
##                      if 'horizon' is not passed then there is only one column for reference.
##  amounts             table of balance (amount sum) for each group within each data frame;
##                      for reference data amount for each member is summed only once;
##                      if 'horizon' is passed then amounts for reference is given for each group by time cell;
##                      if 'horizon' is not passed then there is only one column for reference.
## 
########################################################################################################################—•°

###################################################################################################—•°
## 0. Recording arguments
###################################################################################################—•°
arguments = structure(list( valuated_df  = deparse(substitute(valuated_df))
                , reference_df = deparse(substitute(reference_df))
                , amount = amount #= NULL
                , group = group #= NULL
                , time = time #= NULL
                , yield = yield
                , id = id #= NULL
                , horizon = horizon #= NULL
), class=c("arguments") )

messages = structure(list(),class="messages")

###################################################################################################—•°
## 1. checking data integrity
##
   if(is.null(amount)){
      a.val = valuated_df[,1]
      a.ref = reference_df[,1]       ## index must be integer
   }else if(is.character(amount)||(is.numeric(amount)&(floor(amount)==amount))){
      a.val = valuated_df[,amount]
      a.ref = reference_df[,amount]
   }else{
      stop("Incorrect 'amount' identifier: must be character or integer or left NULL.
            If left NULL then the 1st variable of valuated and reference data frame will be taken as amount variable.")
   }
   
   if(is.null(group)){
      g.val = valuated_df[,2]
      g.ref = reference_df[,2]      ## index must be integer
   }else if(is.character(group)||(is.numeric(group)&(floor(group)==group))){
      g.val = valuated_df[,group]
      g.ref = reference_df[,group]
   }else{
      stop("Incorrect 'group' identifier: must be character or integer or left NULL.
            If left NULL then the 2nd variable of valuated and reference data frames will be taken as grouping variable.")
   }
   if(!is.factor(g.val)){g.val = ordered(g.val)}else if(!is.ordered(g.val)){g.val = factor(g.val,levels=levels(g.val),ordered=TRUE)}
   if(!is.factor(g.ref)){g.ref = ordered(g.ref)}else if(!is.ordered(g.ref)){g.ref = factor(g.ref,levels=levels(g.ref),ordered=TRUE)}
   ## We end up with both factors ordered, but their levels may differ both in values and/or in order.
   ## This is to be solved later on.
   ## Levels are ordered alphabeticaly if not inherited.

   if(is.null(time)){
      #t.val = valuated_df[,3]
      t.ref = reference_df[,3]      ## index must be integer
   }else if(is.character(time)||(is.numeric(time)&(floor(time)==time))){
      #t.val = valuated_df[,time]
      t.ref = reference_df[,time]
   }else{
      stop("Incorrect 'time' identifier: must be character or integer or left NULL.
            If left NULL then the 3d variable of reference data frame will be taken as time variable.")
   }

   if(is.null(yield)){
      #y.val = valuated_df[,4]
      y.ref = reference_df[,4]      ## index must be integer
   }else if(is.character(yield)||(is.numeric(yield)&(floor(yield)==yield))){
      #y.val = valuated_df[,yield]
      y.ref = reference_df[,yield]
   }else{
      stop("Incorrect 'yield' identifier: must be character or integer or left NULL.
            If left NULL then the 4th variable of reference data frame will be taken as yield variable.")
   }
   
   if(is.null(id)){
      id.ref = reference_df[,5]     ## index must be integer
   }else if(is.character(id)||(is.numeric(id)&(floor(id)==id))){
      id.ref = reference_df[,id]
   }else{
      stop("Incorrect 'id' identifier: must be character or integer or left NULL.
            If left NULL then the 5th variable of reference data frame will be taken as id variable.")
   }

   if(is.null(horizon)){
      #h.ref = rep(max(time),length(time))     ## EXCEPTION from the above
   }else if(is.character(horizon)||(is.numeric(horizon)&(floor(horizon)==horizon))){
      h.ref = reference_df[,horizon]
   }else{
      stop("Incorrect 'horizon' identifier: must be character or integer or left NULL.
            If left NULL then effectively max(time) of reference data frame will be taken as horizon variable.")
   }

###################################################################################################—•°
## 2. calculations
##
## a. - amount,   g. - group,    t. - time,  y. - yield,    id. - id,   h. - horizon
## .val - valuated,   .ref - reference

###########################################################—•°
## group factors merging
##
lv = length(levels(g.val))
lr = length(levels(g.ref))

nv = length(g.val)
nr = length(g.ref)

## groups from both data frames must inherit levels values and order from the common factor variable
g.fm = merge_factors(g.val,g.ref)   ## DM::merge_factors.R
group = g.fm$factor
levels.as.numeric = g.fm$order_table   ## by the way we create groups incidences for each data frame
   #
names(attr(levels.as.numeric,"dimnames")) = c("portfolio","group")
attr(levels.as.numeric,"dimnames")$portfolio = c("valuated","reference")

## proper ordering of factors
g.val = group[ 1:nv ]
g.ref = group[ nv + 1:nr ]

###########################################################—•°
## other group tables
##

a.g.val = tapply(a.val,g.val,sum);
a.g.val[is.na(a.g.val)] = 0;

#######################################—•°
## counts == incidence == nr. of unique id's for each group
##
id.g.val = table(g.val)

#######################################—•°
## aggregating AMOUNTS within each group

if(!is.null(horizon)){
   ## horizon
   a <- tapply(a.ref,list(id.ref),max)
   h <- tapply(h.ref,list(id.ref),max)
   g <- var2vec(unique(data.frame(g.ref,id.ref)),names=2)[names(a)]    ## DM::addid.R
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

   incidence = cbind("valuated"=id.g.val,id.gt.ref)

   ################—•°
   ## yields
   df <- data.frame(g = g.ref, y = y.ref, h = h.ref, tt = t.ref)
   y.gt.ref <- with( df[df$t==1,] , cbind(tapply(y,g,sum,na.rm=TRUE)))
   for(t in 2:max(df$h)){
         y.gt.ref <- cbind( y.gt.ref , with( df[df$h>=df$tt & df$tt==t,] , tapply(y,g,sum,na.rm=TRUE)  ) )
   }
   colnames(y.gt.ref) = 1:max(df$h)
   y.gt.ref[is.na(y.gt.ref)] = 0     ##
   y.t.ref  = colSums(y.gt.ref)
      ##
   eff.t.ref = y.t.ref/colSums(a.gt.ref)

   ## efficiency curves within groups on reference
   curves = y.gt.ref / a.gt.ref

   a.gt.refval = cbind(a.g.val,a.gt.ref)
   colnames(a.gt.refval)[1] <- "valuated"

}else{
   ## for .ref without horizon  must be made in 2 steps!!!
   a.g.ref = colSums( tapply( a.ref, list(id.ref,g.ref), max), na.rm=TRUE )   #!!!  max()
   id.g.ref = tapply(id.ref,g.ref,function(x)length(unique(x)))  ## how many unique id's for each group

   incidence = cbind("valuated"=id.g.val,"reference" = id.g.ref)

   ####################################—•°
   ## yields
   ## aggregating yields within each time value for each group:
   y.gt.ref = tapply(y.ref,list(g.ref,t.ref),sum)     ## yields for each group across time
   y.gt.ref[is.na(y.gt.ref)] = 0     ##
   y.t.ref  = colSums(y.gt.ref)
      ##
   eff.t.ref = y.t.ref/sum(a.g.ref)     ##

   ## efficiency curves within groups on reference
   curves = apply(y.gt.ref,2,function(x){x/a.g.ref})

   a.gt.refval = cbind("valuated"=a.g.val,"reference"=a.g.ref)
}


## efficiency curves
curves[is.nan(curves)] = 0
curves = rbind(curves)

incidence[is.na(incidence)] <- 0

###########################################################—•°

#######################################—•°
##  weighted efficiency curve for valuated
##
gval.in.gref = unique(g.val) %in% unique(g.ref)
if( any( !gval.in.gref )){
   warn = Warning( "nonMatchingGroups" ,
"There are groups in valuated data frame having no counterparts in reference data frame.
It results with lack of curves fitted for these groups.
The weighted curve is weighted only with amounts given for groups present also in reference.
You may use curve for other group present in reference as a substitute of curve for group not present in reference.
To do this use  cbg <- curves_by_groups(ref.df);  pcbg <- prediction(pcbg,val.df,regroup=TRUE)
or  rgr <- regroup(pcbg)  where  pcbg <- prediction(pcbg,val.df)  and then again  prediction(pcbg,val.df,regroup=rgr).
Details in help for prediction.curves_by_groups()."
   )
   warning(warn)
   messages = c(messages,list(warn))
}

## calculating weighted curve with only amounts for these groups wich are common for both portfolios
## 

common = levels(g.ref) %in% intersect(unique(g.ref),unique(g.val))
if(any(common)){
   aval <- a.g.val
   aval[!common] <- 0
   aval_normal = aval/sum(aval)
   weighted_curve = as.vector(rbind(aval_normal)%*%curves)
}else{ ## if there are no common groups/levels
   aval_normal = 0
   weighted_curve = rep(0,ncol(curves))
   warn = Warning( "noCommonGroup" ,
"There are no groups common to both portfolios. Weighted curve is 0.
You may use curve for other group present in reference as a substitute of curve for group not present in reference.
To do this use  cbg <- curves_by_groups(ref.df);  pcbg <- prediction(pcbg,val.df,regroup=TRUE)
or  rgr <- regroup(pcbg)  where  pcbg <- prediction(pcbg,val.df)  and then again  prediction(pcbg,val.df,regroup=rgr).
Details in help for prediction.curves_by_groups()."
   )
   warning(warn)
   messages = c(messages,list(warn))
   comment(weighted_curve) <- warn
}

names(weighted_curve) = colnames(curves)

###################################################################################################—•°
## 3. final result
##
aval_normal <- as.vector(aval_normal)

y.gt.val = rbind(diag(a.g.val)%*%curves)
 rownames(y.gt.val) = rownames(curves) #c(rownames(curves),"sum")
 
y.t.val.wc  = sum(a.g.val)*weighted_curve
y.t.val.nec = sum(a.g.val)*eff.t.ref

###################—•°
## time should run vertically (tradition!)
## thus we need to transpose
curves <- t(curves)
y.tg.ref <- t(y.gt.ref) ; rm(y.gt.ref)
y.tg.val <- t(y.gt.val) ; rm(y.gt.val)
incidence <- t(incidence)
a.tg.refval <- t(a.gt.refval) ; rm(a.gt.refval)

names(attr(curves,"dimnames")) <- list("time","group")
names(attr(y.tg.ref,"dimnames")) <- list("time","group")
names(attr(y.tg.val,"dimnames")) <- list("time","group")
firstdim <- ifelse(is.null(horizon),"portfolio","time")
names(attr(incidence,"dimnames")) <- c(firstdim,"group")
names(attr(a.tg.refval,"dimnames")) <- c(firstdim,"group")


###################—•°
## margins   ## margins is attr. defined in  DM::attributes.R
             ##  — it shows which margin is sensible to be calculated, e.g. via addmargins()
margins(curves) <- 1       ## across time
margins(aval_normal) <- 2
margins(weighted_curve) <- 2
margins(eff.t.ref) <- 2

margins(y.t.val.wc)  <- 2
margins(y.t.val.nec) <- 2

margins(y.tg.ref) <- 1:2   ## across time & groups
margins(y.tg.val) <- 1:2   ## across time & groups
margins(incidence) <- 2    ## across groups
margins(a.tg.refval) <- 2   ## across groups

round.attr(y.tg.val) <- 0


result = list(   arguments                = arguments
               , call                     = match.call()
               , curves                   = curves             ## matrix
               , weights                  = aval_normal        ## numeric
               , weighted_curve           = weighted_curve     ## numeric
               , net_curve                = eff.t.ref          ## numeric
                , val_yields_from_weighted = y.t.val.wc        ## numeric
                , val_yields_from_net     = y.t.val.nec       ## numeric
               ##
               , ref_yields_groups        = y.tg.ref           ## matrix
               , val_yields_groups        = y.tg.val           ## matrix
               ##
               , groups                   = levels.as.numeric           ## table
               , counts                   = incidence          ## matrix
               , amounts                  = a.tg.refval         ## matrix
             )

messages(result) <- messages
class(result) <- c("curves_and_prediction", "prediction")

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
## 1. Data
## Ex. 1.1 simulation
## val
( val.df = sim_val( members = 20 , groups = letters[1:8] ) )
## ref
#( ref.df = sim_ref( members = 10 , groups = letters[2:10] , horizon = 4 , yield_par = .1 ) )
( ref.df = sim_ref( members = 100 , groups = letters[2:10]
                  , horizon = 5 + sample(4) #60     ## recycled to get length = members
                  , yield_par = c(.02,.05,.1,.15)    ## recycled to get length = groups
                  #, exp_down = FALSE
                  , amount = NULL
                  ) )

    ## to miss some levels from data  (run after 2.)
    val.df =subset(val.df,group!="h")
    ref.df =subset(ref.df,group!="i")

    
###################################################################################################—•°
## Ex. 1.2 other data frame
val.df = data.frame(amount = Dane.wyc[,"saldo_pocz"],group = rep(1,nrow(Dane.wyc)))
ref.df = Dane.MOref[,c("saldo_pocz","group","okres_obslugi","suma_wplat_new","id_produkt")] 
###################•°
##  variables assigning special for this Ex.
a.val = val.df$amount
g.val = val.df$group
##
a.ref = ref.df$saldo_pocz
g.ref = ref.df$group
t.ref = ref.df$okres_obslugi
y.ref = ref.df$suma_wplat_new
id.ref = ref.df$id_produkt

###################################################################################################—•°
## Ex. 1.3  varying horizon
( val.df = sim_val( members = 20 , groups = letters[1:4] ) )
( ref.df = sim_ref( members = 50 , groups = letters[2:5] , horizon = sample(8:11,10,replace=TRUE) , yield.par = .1 ) )
## horizon for a single member indicates what maximum time value we consider for this member,
## i.e. records having time value greater then horizon are neglected.

   with( ref.df[ref.df$time<=ref.df$horizon,] , {
                     print(ref.df)
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


###################################################################################################—•°
## 2. variables assigning

## not for Ex. 1.2
a.val = val.df$amount
g.val = val.df$group
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

###################################################################################################—•°
## some check out
unique(g.val)
unique(g.ref)
c(g.val,g.ref)
# now you can go to the main function code    STAGE 2

###################################################################################################—•°
## 3. function in action

## comment and uncomment horizon
cap = curves_and_prediction( val.df, ref.df ,horizon = "horizon"
                           )

cap

print(cap,as_rows=FALSE)
print(cap,as_rows=TRUE)

summary(cap)
summary(cap,0)
summary(cap,2)
summary(cap,2,3)
summary(cap,2,10)  ## no more then 7 signif. numbers
summary(cap,as_rows=FALSE)     ## in this summary  as_rows = TRUE by default as it gives more compact view
summary(cap,comments=FALSE)

names(cap)


class(src)
class(src$curves)                        ## matrix
class(src$weighted_curve)                ## numeric
class(src$val_yields_from_weighted)      ## numeric
class(src$ref_net_eff_curve)             ## numeric
class(src$val_yields_from_net_eff)       ## numeric
class(src$ref_yields_groups)             ## matrix
class(src$val_yields_groups)             ## matrix
class(src$levels_as_numeric)             ## table
class(src$counts)                        ## table
class(src$amounts)                       ## table
class(src$summary_df)                    ## matrix

dimnames(src$counts)
attr(src$counts,"dimnames")

plot(src)
plot(src,,10,15)
plot(src,"normal")
plot(src,"normal",10,8)


curtab <- t(src$ref.yields.groups) ; curtab <- curtab[,-ncol(curtab)]
curtab <- curtab[,colSums(curtab) != 0]
   ##
smooth_curves_gam( curtab , horizon = 20 , graphics = TRUE)
smooth_curves_gam( curtab , horizon = 20 , graphics = TRUE
                 , weights.curves = c(1,1,1,1)
                 , lengthen = function(x){x[1]*.9}
                 )

}
rm(dummy)



