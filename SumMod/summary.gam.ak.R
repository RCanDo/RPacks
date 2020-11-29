## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE          {SumMod}
##
## 	summary.gam.panova()
##    summary.gam.coefstars(model_gam , append.to = NULL , envir = -1 , print = TRUE)
##    summary.gam.signs( coefstars.df , envir = -1 , append = FALSE , print = TRUE )
##    summary.gam.coefstars.agg( coefstars.df , FUN = mean , names.agg = NULL , envir = -1 , append = FALSE , print = TRUE )
##    summary.gam.signif( model_gam , append.to = NULL , envir = -1 , print = TRUE)
##    summary.gam.signif.agg( signif.df , FUN = mean )         ####, envir = -1 , append = FALSE , print = TRUE)
##    summary.gam.panova.agg.adds( signif.df , FUN = mean )
##
## DEPENDENCIES
##    signif_stars()
##    mean.panova.aic(datfram)   misc.gam.panova.R
##    mean.panova.mse(datfram)   misc.gam.panova.R
##    colnames.class()     {DM}
##    split.stats()     {Split}
##  ...
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
summary.gam.panova = function(
	  model_gam
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

if( sum(class(model_gam) != c("gam","glm","lm"))>0 ){
	stop("The argument must be of class c(\"gam\",\"glm\",\"lm\").")
}

## --------------------------------------------------------—•°
## MAIN

panova.df = summary(model_gam)$parametric.anova
  # class(panova.df) = c("data.frame","panova","coefstars")   #???
nspa = nrow(panova.df)

   row.names.panova = rownames(panova.df)
   names.model.all = row.names.panova[-nspa]
   names.model.all.uns = gsub("\\)","",gsub("s\\(","",names.model.all))
   names.model.fac = colnames.class(model_gam$data[,names.model.all.uns],"factor") ## my function
   names.model.nonfac = setdiff(names.model.all , names.model.fac)
   names.model.nonfac.uns = setdiff(names.model.all.uns , names.model.fac)

what.all = c("df","ess","mse","Fval","pval") ## simpler names
if("coef" %in% what){
   coeffs = numeric(length(row.names.panova))  ;  names(coeffs) = row.names.panova
   coeffs[names.model.nonfac] = model_gam$coeff[names.model.nonfac]
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
      mse =  summary(model_gam)$parametric.anova[nspa,3]	#rownames( summary.df )[nspa] = "MSE"
      add.ons = c(add.ons,"MSE"=mse)
   }
   if( "aic" %in% add ){
      aic =  model_gam$aic
      add.ons = c(add.ons,"AIC"=aic)
   }
   if( "dev" %in% add ){
      dev =  model_gam$deviance
      add.ons = c(add.ons,"Deviance"=dev)
   }
   if(any(add %in% c("gini","auc","t.mww","p.mww"))){
      if( length(unique(model_gam$y)) != 2 ){
         warning( "Coefficients \"gini\", \"auc\", \"t.mww\", \"p.mww\" are valid only for 2-valued response what is not the case for the given model.
  They won't be calculated then." )    #"
      }else{
         ss = split.stats( predict( model_gam , type="response" ) , model_gam$y , message = FALSE )
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
      yy = model_gam$y ; mean_yy = mean(yy)
      R2 = sum((predict(model_gam) - mean_yy)^2)/sum((yy - mean_yy)^2)
      if("r2" %in% add){
         add.ons = c(add.ons,"R2" = unname(round(R2,4)))
      }
#      if("r2adj" %in% add){
#         R2adj = 1 - (  (1-R2)/(length(yy)-1) / model_gam$df.residual )
#         add.ons = c(add.ons,"R2adj" = unname(round(R2adj,4)))
#      }
   }
   if(any(add %in% c("cvr2","cvr2adj"))){  # other statistics
      if(!is.null(newdata)){
         yy = newdata[,as.character(model_gam$formula)[2]] ;  mean_yy = mean(yy)
         pred = predict(model_gam,type="response",newdata)
         CVR2 = sum((pred - mean_yy)^2)/sum((yy - mean_yy)^2)
         if("cvr2" %in% add){
            add.ons = c(add.ons,"CV_R2" = unname(round(CVR2,4)))
         }
#         if("cvr2adj" %in% add){
#            CVR2adj = 1 - (  (1-CVR2) / (length(yy)-1) / ( length(yy) - model_gam$rank - 1 ) )
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

class(result) = c("data.frame","panova")

if(!is.null(append_to)){
	if(print){print(result)}
	assign( append_to , result , pos = envir )
	result = TRUE
}

result

} ##----END----##
## ------------------------------------------------------------------------------------------------—•°
## EXAMPLE
dummy  = function(){
   summary.gam.panova(model_gam)
   summary.gam.panova(model_gam,sstars=FALSE)
   summary.gam.panova(model_gam,add=NULL)
   summary.gam.panova(model_gam,add=NULL,sstars=FALSE)

   summary.gam.panova(model_gam,what=NULL)   ## (1) no 'add' when only stars present - column 'sstars' is of character class
                                             ## while 'add's are numeric thus they cannot be added (without violating aestetical )
   summary.gam.panova(model_gam,what=NULL,add=NULL)
   summary.gam.panova(model_gam,sstars=FALSE,what=NULL)  ## error!

   summary.gam.panova(model_gam,what=c("coef"))
   summary.gam.panova(model_gam,what=c("coef","pval"))

   summary.gam.panova(model_gam,what=c("coef"),append.to = "sum.df")
   sum.df
   summary.gam.panova(model_gam,what=c("coef"),append.to = "sum.df")
   summary.gam.panova(model_gam,what=c("coef"),append.to = "sum.df",add=NULL)  ## 'add' must be the same
   summary.gam.panova(model_gam,what=c("coef","pval"),append.to = "sum.df")    ## 'what' may change ...
   summary.gam.panova(model_gam,what=NULL,append.to = "sum.df")    ## unless there is possible to 'add' sth. (see (1))

   ## however, if you begin with no 'add's
   rm(sum.df)
   summary.gam.panova(model_gam,what=c("coef"),add=NULL,append.to = "sum.df")
   summary.gam.panova(model_gam,what=NULL,append.to = "sum.df")
}
## ---------------------------------------------------------------------------------------------------------------------—•°


## ---------------------------------------------------------------------------------------------------------------------—•°
summary.gam.coefstars = function(model_gam , add=c("mse","aic","dev"), append.to = NULL , envir = 1 , print = TRUE){
   summary.gam.panowa(model_gam,what=c("coef"),add=add,sstars=TRUE,append.to=append.to,envir=envir,print=print)
} ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°
summary.ak.gam = function(...){   ## for backward compatybility
	summary.gam.coefstars(...)
} ##----END----##
## ------------------------------------------------------------------------------------------------—•°
## EXAMPLE
dummy  = function(){
}
## ---------------------------------------------------------------------------------------------------------------------—•°



## ---------------------------------------------------------------------------------------------------------------------—•°
summary.gam.panova.agg = function( panova.df , FUN = list() , add_colnames = TRUE ){ #, envir = 1 , append = FALSE , print = TRUE ){
## ------------------------------------------------------------------------------------------------—•°
##
## ------------------------------------------------------------------------------------------------—•°
if( sum(class(panova.df) %in% c("data.frame","panova"))<2 ){
	stop("The argument must be of class c(\"data.frame\",\"panova\").")
}

panova.df0 = panova.df[setdiff(rownames(panova.df),c("Residuals","MSE","AIC","Deviance","Gini","AUC","p.mww","t.mww","R2","CV_R2")),]

colnames.result = character(0)
result = data.frame(row.names = rownames(panova.df0))

if(length(add_colnames)<length(FUN)){
   add_colnames = lengthen(add_colnames,length(FUN))
}

for(f in 1:length(FUN)){ # f=2
   fname = names(FUN)[f]
   which_f = grep(fname,colnames(panova.df0),fixed=TRUE)  #!#!#! fixed=TRUE  #!#!#!
   df0_f = panova.df0[,which_f]
   res_f = apply(df0_f,1,FUN[[f]])
   if(add_colnames[f]){
      colnames.res_f = paste( fname , names(FUN[[f]](c(.1,.9))) , sep = "." ) ## to get names of FUN result which are lost via apply()
   }else{
      colnames.res_f = names(FUN[[f]](c(.1,.9))) ## to get names of FUN result which are lost via apply()
   }

   colnames.result = c(colnames.result,colnames.res_f)
   if(!is.null(dim(res_f))){
      res_f = t(res_f)
   }
   result = cbind( result , res_f )
}

colnames(result) = colnames.result

class(result) = c("data.frame","panova.agg")

result

} ##----END----##
## ------------
sstars.table=function(x){ ## x is character vector
   c("***"=sum(x=="***"),"**"=sum(x=="**"),"*"=sum(x=="*"),"."=sum(x=="."),"<insig.>"=sum(x==""))
}
## ------------
## ------------
signs.table=function(x){ ## x is numeric
   y = c("x<0"=sum(x<0),"x>0"=sum(x>0))
   c(y,"min(+|-)"=min(y))
}
## ------------
## ------------------------------------------------------------------------------------------------—•°
## EXAMPLE
dummy  = function(){
   summary.gam.panova.agg(sum.df,FUN = list(coeffs=function(x){mean(x)},coeffs=function(x){sum(x)}))
   summary.gam.panova.agg(sum.df,FUN = list(coeffs=function(x){mean(x)},coeffs=function(x){sum(x)}
      ,sstars=function(x){as.vector(table(x))}))

}
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
summary.gam.panova.agg.adds = function( panova.df , FUN = list()
   , append_to = NULL , row = NULL   ## to which data frame append a result and for which row substitute (if not simply append it)
   , add_colnames = TRUE
	, envir = 1     ## .GlobalEnv
	, print = TRUE
   , rowname = NULL ){ #, envir = 1 , append = FALSE , print = TRUE ){
## ------------------------------------------------------------------------------------------------—•°
##
## ------------------------------------------------------------------------------------------------—•°
rnames = intersect(rownames(panova.df),c("Residuals","MSE","AIC","Deviance","Gini","AUC","p.mww","t.mww","R2","CV_R2"))
rnames = intersect(rnames,names(FUN))
FUN = FUN[names(FUN)%in%rnames]

panova.df0 = capply(panova.df[rnames,],class=c("numeric"))
arow = unlist(panova.df0[1,])
panova.df0 = panova.df0[,which(!is.na(arow)),drop=FALSE]

if(length(add_colnames)<length(FUN)){
   add_colnames = lengthen(add_colnames,length(FUN))
}

#names.result = character(0)
p.agg.adds = numeric(0) #data.frame(row.names = rownames(panova.df0))

for(f in 1:length(FUN)){ # f=1
   fname = names(FUN)[f]
      #which_f = grep(fname,colnames(panova.df0),fixed=TRUE)  #!#!#! fixed=TRUE  #!#!#!
   df0_f = panova.df0[fname,,drop=FALSE]
   res_f = apply(df0_f,1,FUN[[f]])
   res_f = as.vector(res_f)
   if(add_colnames[f]){
      names(res_f) = paste( fname , names(FUN[[f]](c(.1,.9))) , sep = "." )
   }else{
      names(res_f) =  names(FUN[[f]](c(.1,.9)))
   }
   #   if(is.null(dim(res_f))){
   #      names.result = c(colnames.result,fname)
   #   }else{
   #      res_f = t(res_f)
   #      colnames.result = c(colnames.result,colnames(res_f))
   #   }
   p.agg.adds = c( p.agg.adds , res_f )
}

   #colnames(result) = colnames.result

   #class(result) = c("data.frame","panova.agg.adds")


## --------------------------------------------------------—•°
## output

if( is.null(append_to) || !is.character(append_to) || append_to == "" ){

	result = rbind(p.agg.adds)

}else{

	if( !exists( append_to , envir = envir , inherits = TRUE) ){
		result = rbind(p.agg.adds)
	}else{
		result = get( append_to , envir = envir , inherits = TRUE )
      if( is.null(row) ){
	      result = rbind( result , p.agg.adds )
      }else{
         row = (round(row))
         if( is.numeric(row) && row>0 && row<=nrow(result)+1 ){
            result[row,] <- p.agg.adds
         }else{ stop("'row' must be integer between 1 and nrow(get(append_to))+1 or NULL.") }
      }
	}

}

if(is.null(rowname)){
   rowname = nrow(result)
}
rownames(result)[nrow(result)] = rowname
result = as.data.frame(result)

class(result) = c("data.frame","panova.agg.adds")

if(!is.null(append_to)){
	if(print){print(result)}
	assign( append_to , result , pos = envir )
	result = TRUE
}

result

} ##----END----##
## ------------
#means.panova=function(x){ ## synonym
#}
## ------------
## ------------------------------------------------------------------------------------------------—•°
## EXAMPLE
dummy  = function(){
  #  panova.df = summary.df_k
summary.gam.panova.agg.adds( panova.df , FUN = list( "AIC" = function(x){c("min"=min(x),"max"=max(x))} , "AIC" = function(x){c("mean"=mean(x))} ,
                                                     "MSE" = function(x){c("min"=min(x),"max"=max(x))} , "MSE" = function(x){c("mean"=mean(x),"sd"=sd(x))} )
   )

}
## ---------------------------------------------------------------------------------------------------------------------—•°





## ---------------------------------------------------------------------------------------------------------------------—•°
## ---------------------------------------------------------------------------------------------------------------------—•°
## ARCHIV
## ---------------------------------------------------------------------------------------------------------------------—•°
summary.gam.signs = function( coefstars.df , envir = -1 , append = FALSE , print = TRUE ){
## ------------------------------------------------------------------------------------------------—•°
##
## ------------------------------------------------------------------------------------------------—•°

	## coefstars.df = summary.coefstars

name.df = deparse(substitute(coefstars.df))

if( sum(class(coefstars.df) != c("data.frame","coefstars"))>0 ){
	stop("The argument must be of class c(\"data.frame\",\"coefstars\").")
}

## ------------------------
idx = (1:(ncol(coefstars.df)/2))*2 - 1
coefstars.df.agg = apply( coefstars.df[,idx] , 1
			, FUN = function(x){ sigs = c(sum(x<0),sum(x>0)) ;
										sigs = c(sigs,min(sigs))    ;
										names(sigs) = c("-","+","min(+/-)") ;
										sigs
									 }
								)

	# class(coefstars.df.agg)

coefstars.df.agg = as.data.frame(t(coefstars.df.agg))

## -----------------------------------------
## output

if(envir == -1){

	if(append){
	   coefstars.df = cbind(coefstars.df,coefstars.df.agg)
		assign( "result" , coefstars.df , pos = -1 )
	}else{
		assign( "result" , coefstars.df.agg , pos = -1 )
	}

}else{

	if(append){
	   coefstars.df = cbind(coefstars.df,coefstars.df.agg)
		assign( name.df , coefstars.df , pos = envir )
		if(print){print(coefstars.df.agg)}
	}else{
		assign( paste0(name.df,".agg") , coefstars.df.agg , pos = envir )
		if(print){print(coefstars.df.agg)}
	}
	result = TRUE

}

result

} ##----END----##
## ------------------------------------------------------------------------------------------------—•°
## EXAMPLE
dummy  = function(){
}
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
summary.gam.coefstars.agg = function( coefstars.df , FUN = mean , names.agg = NULL
				, envir = -1 , append = FALSE , print = TRUE ){
## ------------------------------------------------------------------------------------------------—•°
##
## ------------------------------------------------------------------------------------------------—•°

	## coefstars.df = summary.coefstars

if( sum(class(coefstars.df) != c("data.frame","coefstars"))>0 ){
	stop("The argument must be of class c(\"data.frame\",\"coefstars\").")
}

name.df = deparse(substitute(coefstars.df))


if(is.null(names.agg)){
	names.agg = if(is.character(FUN)){FUN}else{deparse(substitute(FUN))}
}


## ------------------------
idx = (1:(ncol(coefstars.df)/2))*2 - 1
coefstars.df.agg = apply( coefstars.df[ , idx , drop=FALSE] , 1 , FUN )

if(is.null(dim(coefstars.df.agg))){coefstars.df.agg = rbind(coefstars.df.agg)}

	# class(coefstars.df.agg)

coefstars.df.agg = as.data.frame( t(coefstars.df.agg) )
	if( sum(is.null(colnames(coefstars.df.agg))) > 0 | sum(is.na(colnames(coefstars.df.agg))) > 0 ){
		colnames(coefstars.df.agg) = names.agg
	}

## -----------------------------------------
## output

if(envir == -1){

	if(append){
	   coefstars.df = cbind(coefstars.df,coefstars.df.agg)
		assign( "result" , coefstars.df , pos = -1 )
	}else{
		assign( "result" , coefstars.df.agg , pos = -1 )
	}

}else{

	if(append){
	   coefstars.df = cbind(coefstars.df,coefstars.df.agg)
		assign( name.df , coefstars.df , pos = envir )
		if(print){print(coefstars.df.agg)}
	}else{
		assign( paste0(name.df,".agg") , coefstars.df.agg , pos = envir )
		if(print){print(coefstars.df.agg)}
	}
	result = TRUE

}

result

} ##----END----##
## ------------------------------------------------------------------------------------------------—•°
## EXAMPLE
dummy  = function(){
}
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
summary.gam.signif = function( model_gam , append_to = NULL , envir = -1 , print = TRUE){
## ------------------------------------------------------------------------------------------------—•°
##
## ------------------------------------------------------------------------------------------------—•°

summary.df = summary(model_gam)$parametric.anova
nspa = nrow(summary.df)
rownames( summary.df )[nspa] = "MSE"
pvals = summary.df[5];     ## data frame!

#	class(pvals) <- c("data.frame","signif")

pvals = rbind(pvals, AIC = NA)
pvals["MSE",] =  summary.df[nspa,3]
pvals["AIC",] =  model_gam$aic

# pvals

## -----------------------------------------
## output

if( !is.character(append_to) | is.null(append_to) | append_to == "" ){

	result = pvals

}else{

	if( !exists( append_to , inherits = TRUE) ){
		result = pvals
	}else{
		result = get( append_to ,inherits = TRUE )
		result = cbind(result,pvals)
	}

}

class(result) = c("data.frame","signif")

#if(envir == -1){
#   # if(print){print(result)}
#	# assign( "result" , result , pos = -1 )
#}else
if(envir != -1){
	if(print){print(result)}
	assign( append_to , result , pos = envir )
	result = TRUE
}

result

} ##----END----##
## ------------------------------------------------------------------------------------------------—•°
## EXAMPLE
dummy  = function(){
}
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
summary.gam.signif.agg = function( signif.df , FUN = mean , envir = -1 , append = FALSE , print = TRUE){
## ------------------------------------------------------------------------------------------------—•°
##
## ------------------------------------------------------------------------------------------------—•°

	## signif.df  = summary.signif

name.df = deparse(substitute(signif.df))

if( sum(class(signif.df) != c("data.frame","signif"))>0 ){
	stop("The argument must be of class c(\"data.frame\",\"signif\").")
}

signif.df.agg =  data.frame( "mean.pvalue" = apply(signif.df , 1 , FUN) )
signif.df.agg$sstars = character( nrow(signif.df.agg) )

#sstars = character( length(signif.df.agg) )
#names(signif.df.agg) = names(sstars)
for (k in rownames(signif.df.agg)){
                       if(k=="MSE" | k=="AIC"){ signif.df.agg[k,"sstars"] = noquote("")
                } else if(signif.df.agg[k,1]<=.001){ signif.df.agg[k,"sstars"] = noquote("***")
                } else if(signif.df.agg[k,1]<=.01) { signif.df.agg[k,"sstars"] = noquote("**")
                } else if(signif.df.agg[k,1]<=.05) { signif.df.agg[k,"sstars"] = noquote("*")
                } else if(signif.df.agg[k,1]<=.1)  { signif.df.agg[k,"sstars"] = noquote(".")
                } else { signif.df.agg[k,"sstars"] = noquote("") }
}

signif.df.agg #= data.frame( "mean.pvalue" = signif.df.agg , sstars )

## -----------------------------------------
## output


if(envir == -1){

	if(append){
	   signif.df = cbind(signif.df,signif.df.agg)
		assign( "result" , signif.df , pos = -1 )
	}else{
		assign( "result" , signif.df.agg , pos = -1 )
	}

}else{

	if(append){
	   signif.df = cbind(signif.df,signif.df.agg)
		assign( name.df , signif.df , pos = envir )
		if(print){print(signif.df.agg)}
	}else{
		assign( paste0(name.df,".agg") , signif.df.agg , pos = envir )
		if(print){print(signif.df.agg)}
	}
	result = TRUE

}

result

} ##----END----##
## ------------------------------------------------------------------------------------------------—•°
## EXAMPLE
dummy  = function(){
}
## ---------------------------------------------------------------------------------------------------------------------—•°


## ---------------------------------------------------------------------------------------------------------------------—•°
