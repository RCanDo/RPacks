## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE          {SumMod}
##  panova.gam()
##
## DEPENDENCIES
##  signif_stars()
##  mean.panova.aic(datfram)   misc.gam.panova.R
##  mean.panova.mse(datfram)   misc.gam.panova.R
##  colnames.class()     {DM}
##  split.stats()     {Split}
##
## TODO
##
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
panova.gam = function( model
	, what = c("coef","pval","df","ess","mse","Fval") ## ess = "Estimated Sum of Squares"  --later
	, add = c("mse","aic","dev")	#,"gini","auc","p.mww","t.mww", ,"r2""cvr2")
	, sstars = TRUE
	, append_to = NULL
   , newdata = NULL       ## new data to calculate CV (cross validation)
	, envir = 1     ## .GlobalEnv
	, print = TRUE
){
## ------------------------------------------------------------------------------------------------—•°
##
## ------------------------------------------------------------------------------------------------—•°

## --------------------------------------------------------—•°
## Intro

if(is.null(what)){what=character(0)}
if(is.null(add)){add=character(0)}

if(length(what)==0 &  !sstars){
   stop("At least one element of 'what' must be in c(\"coef\",\"pval\",\"df\",\"ess\",\"mse\",\"Fval\")
  or at least 'sstars' must be TRUE.")
}

if( sum(class(model) != c("gam","glm","lm"))>0 ){
	stop("The argument must be of class c(\"gam\",\"glm\",\"lm\").")
}

## --------------------------------------------------------—•°
## MAIN

panova.df = summary(model)$parametric.anova
  # class(panova.df) = c("data.frame","panova","coefstars")   #???
nspa = nrow(panova.df)

   row.names.panova = rownames(panova.df)
   names.model.all = row.names.panova[-nspa]
   names.model.all.uns = gsub("\\)","",gsub("s\\(","",names.model.all))
   names.model.fac = colnames.class(model$data[,names.model.all.uns],"factor") ## my function
   names.model.nonfac = setdiff(names.model.all , names.model.fac)
   names.model.nonfac.uns = setdiff(names.model.all.uns , names.model.fac)

what.all = c("df","ess","mse","Fval","pval") ## simpler names
if("coef" %in% what){
   coeffs = numeric(length(row.names.panova))  ;  names(coeffs) = row.names.panova
   coeffs[names.model.nonfac] = model$coeff[names.model.nonfac]
   coeffs[c(names.model.fac,"Residuals")] = NA
   panova.df = cbind(coeffs,panova.df)
   what.all = c("coef",what.all)
}

if(sstars){
   panova.df = cbind(panova.df,"sstars"=signif_stars(panova.df[,"Pr(>F)"],if_na=""))
   what = c(what,"sstars")
   what.all = c(what.all,"sstars")
}

#names(what.all) = c("coef","Pr(>F)","Df","Sum Sq","Mean Sq","F value")
which.what = which(what.all%in%what)
chosen = what.all[which.what]
panova.df = panova.df[,which.what,drop=FALSE]


if(length(setdiff(chosen,"sstars"))>0 & length(add)>0){

   add.ons = numeric(0)
   names.add.ons = character(0)

   if( ("mse" %in% add) & !("mse"%in%chosen) ){
      mse =  summary(model)$parametric.anova[nspa,3]	#rownames( summary.df )[nspa] = "MSE"
      add.ons = c(add.ons,"MSE"=mse)
   }
   if( "aic" %in% add ){
      aic =  model$aic
      add.ons = c(add.ons,"AIC"=aic)
   }
   if( "dev" %in% add ){
      dev =  model$deviance
      add.ons = c(add.ons,"Deviance"=dev)
   }
   if(any(add %in% c("gini","auc","t.mww","p.mww"))){
      if( length(unique(model$y)) != 2 ){
         warning( "Coefficients \"gini\", \"auc\", \"t.mww\", \"p.mww\" are valid only for 2-valued response what is not the case for the given model.
  They won't be calculated then." )    #"
      }else{
         ss = split.stats( predict( model , type="response" ) , model$y , message = FALSE )
         if( "gini" %in% add ){
            add.ons = c(add.ons,"Gini" =unname(ss$gini.index))
         }
         if( "auc" %in% add ){
            add.ons = c(add.ons,"AUC"  =unname(ss$auc))
         }
         if( "p.mww" %in% add ){
            add.ons = c(add.ons,"p.mww"=unname(ss$p.value))
         }
         if( "t.mww" %in% add ){
            add.ons = c(add.ons,"t.mww"=unname(ss$statistics))
         }
      }
   }
   if(any(add %in% c("r2","r2adj"))){
      yy = model$y ; mean_yy = mean(yy)
      R2 = sum((predict(model) - mean_yy)^2)/sum((yy - mean_yy)^2)
      if("r2" %in% add){
         add.ons = c(add.ons,"R2" = unname(round(R2,4)))
      }
#      if("r2adj" %in% add){
#         R2adj = 1 - (  (1-R2)/(length(yy)-1) / model$df.residual )
#         add.ons = c(add.ons,"R2adj" = unname(round(R2adj,4)))
#      }
   }
   if(any(add %in% c("cvr2","cvr2adj"))){  # other statistics
      if(!is.null(newdata)){
         yy = newdata[,as.character(model$formula)[2]] ;  mean_yy = mean(yy)
         pred = predict(model,type="response",newdata)
         CVR2 = sum((pred - mean_yy)^2)/sum((yy - mean_yy)^2)
         if("cvr2" %in% add){
            add.ons = c(add.ons,"CV_R2" = unname(round(CVR2,4)))
         }
#         if("cvr2adj" %in% add){
#            CVR2adj = 1 - (  (1-CVR2) / (length(yy)-1) / ( length(yy) - model$rank - 1 ) )
#            add.ons = c(add.ons,"CV_R2adj" = unname(round(CVR2adj,4)))
#         }
      }else{
         warning("Cross Validation cannot be performed as 'newdata' is NULL.")
      }
   }
   #if(){  # other statistics
   #}
   #add.ons.done = FALSE
   names.add.ons = names(add.ons)

   if(sstars){
      if(length(chosen)-2>0){
         nas.df = as.data.frame(matrix(rep(NA,length(add.ons)*(length(chosen)-2)),nrow=length(add.ons)))
         nas.df = cbind(nas.df,rep("",length(add.ons)))
      }else{
         nas.df = data.frame(rep("",length(add.ons)))
      }
   }else{
      nas.df = as.data.frame(matrix(rep(NA,length(add.ons)*length(chosen[-1])),nrow=length(add.ons)))
   }
   add.ons.df = cbind(add.ons,nas.df)
   names(add.ons.df) = names(panova.df)

   panova.df = rbind(panova.df,add.ons.df)
}

if(!any(c("df","ess","mse")%in%what)){
   k = which(rownames(panova.df)=="Residuals")
   panova.df = panova.df[-k,,drop=FALSE]
}

## --------------------------------------------------------—•°
## output

if( is.null(append_to) || !is.character(append_to) || append_to == "" ){

	result = panova.df

}else{

	if( !exists( append_to , envir = envir , inherits = TRUE) ){
		result = panova.df
	}else{
		result = get( append_to , envir = envir , inherits = TRUE )
		result = cbind(result,panova.df)
	}

}

class(result) = c("panova.gam","panova","data.frame")

if(!is.null(append_to)){
	if(print){print(result)}
	assign( append_to , result , pos = envir )
	result = TRUE
}

result

}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
## EXAMPLES ############################################################################################################—•°
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
dummy = function(){
## This is dummy function - it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## — in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
## ------------------------------------------------------------------------------------------------—•°

 ## RELOADER — before it works you need to source("PacksAK.R"); it's best to use PackName.R within pack's dir.
 loadPacksAK("SumMod")

## ------------------------------------------------------------------------------------------------—•°

   ##  model <- model_kj

   panova(model)
   panova(model,sstars=FALSE)
   panova(model,add=NULL)
   panova(model,add=NULL,sstars=FALSE)

   panova(model,what=NULL)   ## (1) no 'add' when only stars present - column 'sstars' is of character class
                                             ## while 'add's are numeric thus they cannot be added (without violating aestetics )
   panova(model,what=NULL,add=NULL)
   panova(model,sstars=FALSE,what=NULL)  ## error!

   panova(model,what=c("coef"))
   panova(model,what=c("coef","pval"))

   panova(model,what=c("coef"),append_to = "sum.df")
   sum.df
   panova(model,what=c("coef"),append_to = "sum.df")
   panova(model,what=c("coef"),append_to = "sum.df",add=NULL)  ## 'add' must be the same
   panova(model,what=c("coef","pval"),append_to = "sum.df")    ## 'what' may change ...
   panova(model,what=NULL,append_to = "sum.df")    ## unless there is possible to 'add' sth. (see (1))

   ## however, if you begin with no 'add's
   rm(sum.df)
   panova(model,what=c("coef"),add=NULL,append_to = "sum.df")
   panova(model,what=NULL,append_to = "sum.df")
}
## ---------------------------------------------------------------------------------------------------------------------—•°
rm(dummy)

