########################################################################################################################—•°
## FUNCTIONS HERE
##  predict.curves_by_groups()
##
## DEPENDENCIES
##  merge_factors.R
##    merge_factors()
##  addid.r
##    var2vec()
##
## TODO
##
########################################################################################################################—•°

########################################################################################################################—•°
prediction.curves_by_groups <- function( cbg
                                    , newdata = NULL
                                    , curves  = c("net","groups")
                                    , values  = "efficiency"
                                    , group  = NULL , amount = NULL
                                    , ... ){
########################################################################################################################—•°
## We are given two investment portfolios divided in groups (sub-portfolios) of the same types (e.g. balance groups):
##   - reference for which historical yields/payments are given,
##   - valuated for which prediction (within respective groups) is needed on the basis of reference data.
## Calculating this prediction is the aim of this function.
##
##		Arguments
##  cbg           object of class "curves_by_groups"
##  newdata       data frame having at least 2 variables: "group", "amount";
##                'amount' is summed up within each group to obtain total amount for each group and by this
##                the contribution of each group into the whole portfolio.
##                In case of vector 'newdata' is treated as a vector of amounts for each group, recycled if necessary
##  curves=c("net","groups")
##  values="efficiency"
##
########################################################################################################################—•°


###################################################################################################—•°
## 0. Recording arguments
###################################################################################################—•°
arguments = list( cbg = deparse(substitute(cbg))
                , newdata = deparse(substitute(newdata))
                , curves  = curves
                , values  = values
                , group  = group , amount = amount , ... = ...
)

messages = structure(list(),class="messages")
#prints = list()

###################################################################################################—•°
## 1. Checking data integrity
###################################################################################################—•°

#######################################—•°
## curves
if(is.null(curves)){ curves <- c("net","groups") }
curves <- intersect(curves,c("net","groups"))
if(length(curves)==0){
   stop("'curves' must be a character vector containing \"net\" and/or \"groups\".
  It may be also left NULL in which case it is substituted with c(\"net\",\"groups\").")
}

#######################################—•°
## newdata
if(!is.null(newdata)){

   if(is.null(dim(newdata))){  ## we treat newdata as weights for curves <=> total amounts for groups in val
      if(is.null(names(newdata))){
         newdata = rep(newdata,length=ncol(cbg$curves))
         names(newdata) = names(cbg$curves)
      }
      newdata = data.frame( a = newdata , g = names(newdata) )
   }else{
      if(length(dim(newdata))!=2){
         stop("'newdata' must be a vector or a data.frame or 2d matrix.")
      }
   }


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
   if(!is.factor(g.val)){g.val = ordered(g.val)}else if(!is.ordered(g.val)){g.val = factor(g.val,levels=levels(g.val),ordered=TRUE)}
   #if(!is.factor(g.ref)){g.ref = ordered(g.ref)}else if(!is.ordered(g.ref)){g.ref = factor(g.ref,levels=levels(g.ref),ordered=TRUE)}
   ## We end up with both factors ordered, but their levels may differ both in values and/or in order.
   ## This is to be solved later on.
   ## Levels are ordered alphabeticaly if not inherited.

}else{
   #!??????
}

###################################################################################################—•°
## 2. calculations
##
## a. - amount,   g. - group,    t. - time,  y. - yield,    id. - id,   h. - horizon
## .val - valuated,   .ref - reference

if(!is.null(newdata)){

   ###########################################################—•°
   ## group factors merging
   ##
   ## table of numerical representation of groups (factor) levels for both portfolios (data frames)
      lv = length(levels(g.val))
      lr = length(cbg$groups)

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

#   if(!is.null(horizon)){
#         ## horizon
#         a <- tapply(a.ref,list(id.ref),max)
#         h <- tapply(h.ref,list(id.ref),max)
#         g <- var2vec(unique(data.frame(g.ref,id.ref)),names=2)[names(a)]    ## DM::addid.R
#         df <- data.frame(a,g,h)
#         a.gt.ref = cbind(tapply(a,g,sum))
#         id.gt.ref = cbind(tapply(names(a),g,length))
#            ##
#         for(t in 2:max(df$h)){
#            a.gt.ref <- cbind( a.gt.ref , with( df[df$h>=t,] , tapply(a,g,sum,na.rm=TRUE)  ) )
#           id.gt.ref <- cbind( id.gt.ref , with( df[df$h>=t,] , tapply(names(a),g,length)  ) )
#         }
#         colnames(a.gt.ref) = 1:max(df$h)
#         colnames(id.gt.ref) = 1:max(df$h)
#         a.gt.ref[is.na(a.gt.ref)] <- 0
#         id.gt.ref[is.na(id.gt.ref)] <- 0
#
      #incidence = rbind(cbg$counts,"valuated"=id.g.val)

      incidence <- outer(rep(0,nrow(cbg$counts)),rep(0,length(levels(g.val)))) ## matrix of zeros of proper size
      rownames(incidence) <- rownames(cbg$counts)
      colnames(incidence) <- levels(g.val)
      incidence[,colnames(cbg$counts)] <- cbg$counts     ## ref counts
      incidence <- rbind("valuated"=id.g.val,incidence)  ## val counts
      margins(incidence) <- margins(cbg$counts)
      names(attr(incidence,"dimnames")) <- c("portfolio","group")
      #printtm(incidence)


         ################—•°
         ## yields
#         df <- data.frame(g = g.ref, y = y.ref, h = h.ref, tt = t.ref)
#         y.gt.ref <- with( df[df$t==1,] , cbind(tapply(y,g,sum,na.rm=TRUE)))
#         for(t in 2:max(df$h)){
#               y.gt.ref <- cbind( y.gt.ref , with( df[df$h>=df$tt & df$tt==t,] , tapply(y,g,sum,na.rm=TRUE)  ) )
#         }
#         colnames(y.gt.ref) = 1:max(df$h)
#         y.gt.ref[is.na(y.gt.ref)] = 0     ##
#         y.t.ref  = colSums(y.gt.ref)
#            ##
#         eff.t.ref = y.t.ref/colSums(a.gt.ref)
#
#         ## efficiency curves within groups on reference
#         curves = y.gt.ref / a.gt.ref
#
#      a.g.refval = cbind(a.g.val,a.gt.ref)
#      colnames(a.g.refval)[1] <- "valuated"

      a.tg.refval <- outer(rep(0,nrow(cbg$amounts)),rep(0,length(levels(g.val)))) ## matrix of zeros of proper size
      rownames(a.tg.refval) <- rownames(cbg$amounts)
      colnames(a.tg.refval) <- levels(g.val)
      a.tg.refval[,colnames(cbg$amounts)] <- cbg$amounts   ## ref amounts
      a.tg.refval <- rbind("valuated"=a.g.val,a.tg.refval)  ## val amounts
      margins(a.tg.refval) <- margins(cbg$amounts)
      names(attr(a.tg.refval,"dimnames")) <- c("portfolio","group")
      #printtm(a.tg.refval)


#   }else{
#         ## for .ref without horizon  must be made in 2 steps!!!
#         a.g.ref = colSums( tapply( a.ref, list(id.ref,g.ref), max), na.rm=TRUE )   #!!!  max()
#         id.g.ref = tapply(id.ref,g.ref,function(x)length(unique(x)))  ## how many unique id's for each group
#
#      incidence = cbind("valuated"=id.g.val,"reference" = id.g.ref)
#
#      ####################################—•°
#      ## yields
#      ## aggregating yields within each time value for each group:
#      y.gt.ref = tapply(y.ref,list(g.ref,t.ref),sum)     ## yields for each group across time
#      y.gt.ref[is.na(y.gt.ref)] = 0     ##
#      y.t.ref  = colSums(y.gt.ref)
#         ##
#      eff.t.ref = y.t.ref/sum(a.g.ref)     ##
#
#      ## efficiency curves within groups on reference
#      curves = apply(y.gt.ref,2,function(x){x/a.g.ref})
#
#      a.g.refval = cbind("valuated"=a.g.val,"reference"=a.g.ref)
#   }

#   y.gt.ref = rbind(y.gt.ref,"sum"=y.t.ref)

      y.tg.ref <- outer(rep(0,nrow(cbg$yields)),rep(0,length(levels(g.val)))) ## matrix of zeros of proper size
      rownames(y.tg.ref) <- rownames(cbg$yields)
      colnames(y.tg.ref) <- levels(g.val)
      y.tg.ref[,colnames(cbg$yields)] <- cbg$yields   ## ref amounts
      #a.g.refval <- rbind("valuated"=a.g.val,a.g.refval)  ## val amounts
      margins(y.tg.ref) <- margins(cbg$yields)
      names(attr(y.tg.ref,"dimnames")) <- c("time","group")
      #printtm(y.tg.ref)


#   #incidence[incidence==0] <- NA
#   names(attr(incidence,"dimnames")) = c("group","portfolio")

#   ## efficiency curves
#   curves[is.nan(curves)] = 0
#   curves = rbind(curves)

#   names(attr(a.g.refval,"dimnames")) = c("group","portfolio")

#   incidence[is.na(incidence)] <- 0


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
Curves may be reweighted using function curves.reweight()."
      )
      warning(warn)
      messages = c(messages,list(warn))
   }

   ## calculating weighted curve with only amounts for these groups wich are common for both portfolios

   common = levels(g.ref) %in% intersect(unique(g.ref),unique(g.val))
   if(any(common)){
      com_lev = levels(g.ref)[common]
      aval = a.g.val[com_lev]
      weighted_curve = as.vector(cbg$curves[,com_lev]%*%cbind(aval)/sum(aval))
   }else{ ## if there are no common groups/levels
      # weighted.curve = rbind(rep(0,ncol(groups.curves)))
      weighted_curve = rep(0,nrow(cbg$curves))
      warn = Warning( "noCommonGroup" ,"There are no groups common to both portfolios. Weighted curve is 0.")
      warning(warn)
      messages = c(messages,list(warn))
      comment(weighted_curve) <- warn
   }
   # rownames(weighted.curve) = "weighted.curve"
   # colnames(weighted.curve) = colnames(groups.curves)
   names(weighted_curve) = rownames(cbg$curves)

}

###################################################################################################—•°
## 3. final result
##
#gw.curves = rbind( cbg$curves, weighted_curve )

## widening curves matrix to have all groups ref & val
curves = y.tg.ref   ## just to copy the shape
curves[] = 0
curves[,colnames(cbg$curves)] <- cbg$curves

y.tg.val = rbind(curves%*%diag(a.g.val))
colnames(y.tg.val) = colnames( y.tg.ref ) #c(rownames(curves),"sum")

y.t.val.wc  = sum(a.g.val)*weighted_curve
y.t.val.nec = sum(a.g.val)*cbg$net_curve

####################—•°
### time should run vertically (tradition!)
### thus we need to transpose
#curves   <- t(curves)
#y.gt.ref <- t(y.gt.ref)
#y.gt.val <- t(y.gt.val)
#incidence <- t(incidence)
#a.g.refval <- t(a.g.refval)
#levels.as.numeric <- levels.as.numeric
###################—•°
## margins   ## margins is attr. defined in  DM::attributes.R
             ##  —— it shows which margin is sensible to be calculated, e.g. via addmargins()
margins(curves) <- 1       ## across time
margins(y.tg.ref) <- 1:2   ## across time & groups
margins(y.tg.val) <- 1:2   ## across time & groups
margins(incidence) <- 2    ## across groups
margins(a.tg.refval) <- 2   ## across groups

result = list(   arguments                = arguments
               , call                     = match.call()
               , curves                   = curves             ## matrix
                  , weights                  = aval_normal        ## numeric
               , weighted_curve           = weighted_curve     ## numeric
               , net_curve                = cbg$net_curve    ## numeric    #=eff.t.ref
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
class(result) <- "curves_and_prediction"

result

####################################################################################################—•°
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
##############################################################################################—•°

 ## RELOADER —— before it works you need to source("PacksAK.R"); it's best to use EfficiencyCurves.R within pack's dir.
 loadPacksAK("EfficiencyCurves")

##############################################################################################—•°


}
rm(dummy)
