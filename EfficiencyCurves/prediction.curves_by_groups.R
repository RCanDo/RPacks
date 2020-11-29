## ---------------------------------------------------------------------------------------------------------------------—•°
## prediction() method for "curves_by_groups".
## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE       {EfficiencyCurves}
##  prediction.curves_by_groups()
##
## DEPENDENCIES         {EfficiencyCurves}
##  prediction()        {DM}
##  merge_factors()     {DM} merge_factors.R
##  margins()           {DM} attributes.R
##  round.attr()            "
##  Warning()           {DM} conditions.R
##  Message()               "
##  messages()              "
##
## REMARKS/WARNINGS
##
## TODO
## •
##
## DESCRIPTION
##  The aim, the idea, problems, solutions... briefly!, concerning the whole set of functions
##
## -------------------------------------------------------------------------------------------------—•°
## AUTHOR: Arkadiusz Kasprzyk; akasp@...
## ver. 1.0
## PATH: D:\ROBOCZY\...
## start: 2017-03-01    last: 2017-09-26
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
prediction.curves_by_groups <- function( cbg
                                    , newdata = NULL
                                    #, curves  = c("net","groups") , values  = "efficiency"
                                    , group  = NULL , amount = NULL
                                    , regroup = NULL
                                    , weights = NULL
                                    , ... ){
## ---------------------------------------------------------------------------------------------------------------------—•°
## We are given two investment portfolios divided in groups (sub-portfolios) of the same types (e.g. balance groups):
##   • reference for which historical yields/payments are given,
##   • valuated for which prediction (within respective groups) is needed on the basis of reference data.
## Calculating this prediction is the aim of this function.
##
##		Arguments
##  cbg           object of class "curves_by_groups"
##  newdata       data frame having at least 2 variables: "group", "amount";
##                'amount' is summed up within each group to obtain total amount for each group and by this
##                the contribution of each group into the whole portfolio.
##                In case of unnamed vector 'newdata' is treated as a vector of amounts for each group,
##                recycled if necessary.
##                If vector is named then it is not recycled and weights are taken for the groups according to names.
#  ##  curves=c("net","groups")##  values="efficiency"
##
## ---------------------------------------------------------------------------------------------------------------------—•°

## ------------------------------------------------------------------------------------------------—•°
## 0. Recording arguments
## ------------------------------------------------------------------------------------------------—•°
arguments = structure(list( cbg = deparse(substitute(cbg))
                , newdata = deparse(substitute(newdata))
                #, curves  = curves , values  = values
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
## newdata
if(is.null(newdata)){
   stop("'newdata' cannot be NULL.
Minimal input is a vector of total weights/amounts for groups in refrerence
which is internaly replicated to the proper length if necessary
(thus even one number may be passed what gives equal weight for all groups).")
}

if(is.null(dim(newdata))){  ## we treat newdata as weights for curves <=> total amounts for groups in val
   if(is.null(names(newdata))){
      newdata = rep(newdata,length=ncol(cbg$curves))
      names(newdata) = colnames(cbg$curves)
   }
   newdata = data.frame( a = newdata , g = names(newdata) )
}else{
   if(length(dim(newdata))!=2){
      stop("'newdata' must be a vector or a data.frame or 2d matrix.")
   }
}

## ------------------------------------—•°
##

if(is.null(amount)){
   a.val = newdata[,1] ## valuated_df[,1]
  # a.ref = reference_df[,1]       ## index must be integer
}else if(is.character(amount)||(is.numeric(amount)&(floor(amount)==amount))){
   a.val = newdata[,amount]
  # a.ref = reference_df[,amount]
}else{
   stop("Incorrect 'amount' identifier: must be character or integer or left NULL.
         If left NULL then the 1st variable of valuated and reference data frame will be taken as amount variable.")
}

if(is.null(group)){
   g.val = newdata[,2]
  # g.ref = reference_df[,2]          ## index must be integer
}else if(is.character(group)||(is.numeric(group)&(floor(group)==group))){
   g.val = newdata[,group]
  # g.ref = reference_df[,group]
}else{
   stop("Incorrect 'group' identifier: must be character or integer or left NULL.
         If left NULL then the 2nd variable of valuated and reference data frames will be taken as grouping variable.")
}
#if(!is.factor(g.val)){g.val = ordered(g.val)}else if(!is.ordered(g.val)){g.val = factor(g.val,levels=sort(unique(g.val)),ordered=TRUE)}
#if(!is.factor(g.ref)){g.ref = ordered(g.ref)}else if(!is.ordered(g.ref)){g.ref = factor(g.ref,levels=levels(g.ref),ordered=TRUE)}
   g.val <- as.factor(g.val); #g.val<-ordered(g.val)
## We end up with both factors ordered, but their levels may differ both in values and/or in order.
## This is to be solved later on.
## Levels are ordered alphabeticaly if not inherited.



## ------------------------------------------------------------------------------------------------—•°
## 2. calculations
##
## a. - amount,   g. - group,    t. - time,  y. - yield,    id. - id,   h. - horizon
## .val - valuated,   .ref - reference


## --------------------------------------------------------—•°
## group factors merging
##
## table of numerical representation of groups (factor) levels for both portfolios (data frames)
#   lv = length(levels(g.val))
#   lr = length(cbg$groups)

nv = length(g.val)
nr = length(cbg$groups)

## groups from both data frames must inherit levels values and order from the common factor variable
g.fm = merge_factors(g.val,cbg$groups)   ## DM::merge_factors.R
group = g.fm$factor
levels.as.numeric = g.fm$order_table   ## by the way we create groups incidences for each data frame
   #
names(attr(levels.as.numeric,"dimnames")) = c("portfolio","group")
attr(levels.as.numeric,"dimnames")$portfolio = c("valuated","reference")

## proper ordering of factors
g.val = group[ 1:nv ]
g.ref = group[ nv + 1:nr ]

## --------------------------------------------------------—•°
## other group tables
##

a.g.val = tapply(a.val,g.val,sum);
a.g.val[is.na(a.g.val)] = 0;

## ------------------------------------—•°
## counts == incidence == nr. of unique id's for each group
##
id.g.val = table(g.val)

             ## matrix of zeros of proper size
incidence <- outer(rep(0,nrow(cbg$counts)),rep(0,length(levels(g.val)))) ## levels NOT values
rownames(incidence) <- rownames(cbg$counts)
colnames(incidence) <- levels(g.val)
incidence[,colnames(cbg$counts)] <- cbg$counts     ## ref counts
incidence <- rbind("valuated"=id.g.val,incidence)  ## val counts
margins(incidence) <- margins(cbg$counts)
names(attr(incidence,"dimnames")) <- c("portfolio","group")
#printtm(incidence)

## ------------------------------------—•°
## amounts
##
               ## matrix of zeros of proper size
a.tg.refval <- outer(rep(0,nrow(cbg$amounts)),rep(0,length(levels(g.val)))) ## levels NOT values
rownames(a.tg.refval) <- rownames(cbg$amounts)
colnames(a.tg.refval) <- levels(g.val)
a.tg.refval[,colnames(cbg$amounts)] <- cbg$amounts   ## ref amounts
a.tg.refval <- rbind("valuated"=a.g.val,a.tg.refval)  ## val amounts
margins(a.tg.refval) <- margins(cbg$amounts)
names(attr(a.tg.refval,"dimnames")) <- c("portfolio","group")
#printtm(a.tg.refval)

## ------------------------------------—•°
## yields for ref
##  groups from val must be added (as zeros)
##
            ## matrix of zeros of proper size
y.tg.ref <- outer(rep(0,nrow(cbg$yields)),rep(0,length(levels(g.val)))) ## levels NOT values
rownames(y.tg.ref) <- rownames(cbg$yields)
colnames(y.tg.ref) <- levels(g.val)
y.tg.ref[,colnames(cbg$yields)] <- cbg$yields   ## ref amounts
#a.g.refval <- rbind("valuated"=a.g.val,a.g.refval)  ## val amounts
margins(y.tg.ref) <- margins(cbg$yields)
names(attr(y.tg.ref,"dimnames")) <- c("time","group")
#printtm(y.tg.ref)


## --------------------------------------------------------—•°

## ------------------------------------—•°
##  weighted efficiency curve for valuated
##
gval.in.gref = unique(g.val) %in% unique(g.ref)
   if( any( !gval.in.gref )){
   warn = Warning( "NonMatchingGroups" ,
"There are groups in valuated data frame having no counterparts in reference data frame.
It results with lack of curves fitted for these groups.
The weighted curve is weighted only with amounts given for groups present also in reference.
You may use curve for other group present in reference as a substitute of curve for group not present in reference.
To do this use option  regroup=TRUE  or  regroup=rgr  where  rgr <- regroup(pcbg)
and  pcbg  is a result of this function. Details in help(prediction.curves_by_groups)."
   )
   #warning(warn)
   messages = c(messages,list(warn))
}

## calculating weighted curve with only amounts for these groups wich are common for both portfolios



if( is.null(regroup) || (is.logical(regroup) && !regroup ) ){  ## normal procedure

   common = levels(g.ref) %in% intersect(unique(g.ref),unique(g.val))   ## this is OK!!! cannot be simplified
   if(any(common)){
      ##  cbg$curves may not have all entries
      com_lev = levels(g.ref)[common]
      curves <- cbg$curves[,com_lev]

         aval <- a.g.val
         aval[setdiff(names(aval),com_lev)] <- 0    ## <=>  aval[!common] <- 0
         aval_normal = aval/sum(aval)
         weighted_curve <- as.vector(curves%*%cbind(aval_normal[com_lev]))
      ## ...
   }else{ ## if there are no common groups/levels
      curves <- cbg$curves
      aval_normal = 0
      weighted_curve = rep(0,nrow(cbg$curves))
      warn = Warning( "NoCommonGroup" ,
"There are no groups common to both portfolios. Weighted curve is 0.
You may use curve for other group present in reference as a substitute of curve for group not present in reference.
To do this use option  regroup=TRUE  or  regroup=rgr  where  rgr <- regroup(pcbg)
and  pcbg  is a result of this function. Details in help(prediction.curves_by_groups)."
      )
      #warning(warn)
      messages = c(messages,list(warn))
      comment(weighted_curve) <- warn
   }

}else{  ## reweight procedure

   ## original levels of "reference" in the same order as in cbg$curves
   or <- levels.as.numeric["reference",]
   or <- or[!is.na(or)]

   if(is.logical(regroup) && regroup ){
      regroup <- edit(as.data.frame(unclass(t(levels.as.numeric))))
      regroup <- t(regroup)
      names(attr(regroup,"dimnames")) = c("portfolio","group")
   }

   rgr <- regroup[,apply(regroup,2,function(x){!any(is.na(x))})]

   msg = Message( "Regrouping"
                , paste0(
"Curves has been regrouped as shown in $regroup entry of the  prediction(",arguments$cbg,")  output.
Notice that arrangement (selection and order) of  $curves  is that after regrouping.
Original arrangement of curves as aquired from reference data is in  ",arguments$cbg,"$curves."
                ))
   messages = c(messages,list(msg))

   rr <- rgr["reference",]
   curves <- cbg$curves[,names(or[rr])]

   com_lev = colnames(rgr)
   colnames(curves) <- com_lev

   aval <- a.g.val
   aval[setdiff(names(aval),com_lev)] <- 0
   aval_normal <- aval/sum(aval)
   weighted_curve <- as.vector(curves%*%cbind(aval_normal[com_lev]))
   ## ...
}

names(weighted_curve) = rownames(cbg$curves)

## ------------------------------------------------------------------------------------------------—•°
## 3. result
##

nams <- names(aval_normal)
aval_normal <- as.vector(aval_normal)
names(aval_normal) <- nams

## widening curves matrix to have all groups ref & val but curves will be non-zero for only common groups
## and those resulting from regrouping (if this is the case)
   #y.tg.val <- y.tg.ref   ## just to copy the shape
   #y.tg.val[] <- 0
y.tg.val <- outer(rep(0,nrow(curves)),rep(0,ncol(y.tg.ref)))
   dimnames(y.tg.val) <- list("time"=rownames(curves),"group"=colnames(y.tg.ref))
#   colnames(y.tg.val) <- colnames(y.tg.ref)
#   rownames(y.tg.val) <- rownames(curves)
y.tg.val[,colnames(curves)] <- curves
curves <- y.tg.val

y.tg.val <- rbind(y.tg.val%*%diag(a.g.val))
colnames(y.tg.val) <- colnames( y.tg.ref ) #c(rownames(curves),"sum")

y.t.val.wc  <- sum(a.g.val)*weighted_curve
y.t.val.net <- sum(a.g.val)*cbg$net_curve

#   ## original set of curves but matrix widened to set of all groups from ref and val;
#   ## for groups not present in ref curves will be 0
#   cs <- y.tg.ref       ## just to copy the shape
#   cs[] <- 0
#   cs[,colnames(cbg$curves)] <- cbg$curves

## -----------------—•°
##  time should run vertically (tradition!)
##  thus we need to transpose
#curves   <- t(curves)
#y.gt.ref <- t(y.gt.ref)
#y.gt.val <- t(y.gt.val)
#incidence <- t(incidence)
#a.g.refval <- t(a.g.refval)
#levels.as.numeric <- levels.as.numeric
## ----------------—•°

## margins   ## margins is attr. defined in  DM::attributes.R
             ##  —— it shows which margin is sensible to be calculated, e.g. via addmargins()
margins(curves)    <- 1     ## across time
#margins(cs)    <- 1     ## across time
margins(aval_normal) <- 2
margins(weighted_curve) <- 2

margins(y.t.val.wc)  <- 2
margins(y.t.val.net) <- 2

margins(y.tg.ref)  <- 1:2   ## across time & groups
margins(y.tg.val)  <- 1:2   ## across time & groups
margins(incidence) <- 2     ## across groups
margins(a.tg.refval) <- 2   ## across groups

round.attr(y.tg.val) <- 0

result = list(   call                     = match.call()
               , arguments                = arguments
               ##
               , counts                   = incidence          ## matrix
               , amounts                  = a.tg.refval        ## matrix
               , ref_yields_groups        = y.tg.ref           ## matrix
               ##
               , groups                   = levels.as.numeric  ## table
               , regroup                  = if(is.null(regroup)){regroup}else{as.table(regroup)}
               ##
               , weights                  = aval_normal        ## numeric
               , curves                   = curves  ## cs      ## matrix
               , weighted_curve           = weighted_curve     ## numeric
               , net_curve                = cbg$net_curve      ## numeric    #=eff.t.ref
               , val_yields_from_weighted = y.t.val.wc        ## numeric
               , val_yields_from_net      = y.t.val.net       ## numeric
               ##
               , val_yields_groups        = y.tg.val           ## matrix
             )

messages(result) <- messages
class(result) <- c("curves_and_prediction", "prediction")

result

## -------------------------------------------------------------------------------------------------—•°
}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
## EXAMPLES ############################################################################################################—•°
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
dummy = function(){
## This is dummy function —— it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## —— in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
## -------------------------------------------------------------------------------------------—•°

 ## RELOADER —— before it works you need to source("PacksAK.R"); it's best to use EfficiencyCurves.R within pack's dir.
 loadPacksAK("EfficiencyCurves")

## -------------------------------------------------------------------------------------------—•°


}
rm(dummy)
