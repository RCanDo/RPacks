## FUNCTIONS LIST:
## 	summary.gam.panova -- in preparation
##    summary.gam.coefstars(model.gam , append.to = NULL , envir = -1 , print = TRUE)
##    summary.gam.signs( coefstars.df , envir = -1 , append = FALSE , print = TRUE )
##    summary.gam.coefstars.agg( coefstars.df , FUN = mean , names.agg = NULL , envir = -1 , append = FALSE , print = TRUE )
##    summary.gam.signif( model.gam , append.to = NULL , envir = -1 , print = TRUE)
##    summary.gam.signif.agg( signif.df , FUN = mean , envir = -1 , append = FALSE , print = TRUE)

summary.gam.panova = function(
	  model.gam
	, what = c("coef","pval") ## c("coef","pval")
	, add = c("mse","aic")	#,"auroc","gini")
	, sstars = TRUE
	, appaned.to = NULL
	, envir = -1
	, print = TRUE
){

if( sum(class(model.gam) != c("gam","glm","lm"))>0 ){
	stop("The argument must be of class c(\"gam\",\"glm\",\"lm\").")
}

summary.df = summary(model.gam)$parametric.anova
nspa = nrow(summary.df)
if( "mse" %in% add ){
	rownames( summary.df )[nspa] = "MSE"
}else{
   summary.df = summary.df[-nspa,]
}

if( sstars ){
	pvals = summary.df[5];     ## data frame!
}

sstars = character(nspa+1);
names(sstars) = c( rownames( summary.df ), "AIC" )
for (k in names(sstars)){
                       if(k=="MSE" | k=="AIC"){ sstars[k] = noquote("")
                } else if(pvals[k,]<=.001){ sstars[k] = noquote("***")
                } else if(pvals[k,]<=.01) { sstars[k] = noquote("**")
                } else if(pvals[k,]<=.05) { sstars[k] = noquote("*")
                } else if(pvals[k,]<=.1)  { sstars[k] = noquote(".")
                } else { sstars[k] = noquote("") }
} #!!!!

MSE =  summary.df[nspa,3]
aic =  model.gam$aic
coefstars = cbind(  rbind(pvals, AIC = "") , #summary(log.reg.model.preliminary1_np)$parametric.anova,
                        "coef" = c(model.gam$coeff[-1], MSE, aic) ,
                        sstars
					)[,-1]

############################################
## output

if( !is.character(append.to) | is.null(append.to) | append.to == "" ){

	result = coefstars

}else{

	if( !exists( append.to , inherits = TRUE) ){
		result = coefstars
	}else{
		result = get( append.to ,inherits = TRUE )
		result = cbind(result,coefstars)
	}

}

class(result) = c("data.frame","coefstars")

#if(envir == -1){
#   #if(print){print(result)}
#	# assign( "result" , result , pos = -1 )
#}else
if(envir != -1){
	if(print){print(result)}
	assign( append.to , result , pos = envir )
	result = TRUE
}

result



}


########################################################################################################################
########################################################################################################################

summary.gam.coefstars = function(model.gam , append.to = NULL , envir = -1 , print = TRUE){
#
if( sum(class(model.gam) != c("gam","glm","lm"))>0 ){
	stop("The argument must be of class c(\"gam\",\"glm\",\"lm\").")
}

summary.df = summary(model.gam)$parametric.anova
nspa = nrow(summary.df)
rownames( summary.df )[nspa] = "MSE"
pvals = summary.df[5];     ## data frame!

sstars = character(nspa+1);
names(sstars) = c( rownames( summary.df ), "AIC" )
for (k in names(sstars)){
                       if(k=="MSE" | k=="AIC"){ sstars[k] = noquote("")
                } else if(pvals[k,]<=.001){ sstars[k] = noquote("***")
                } else if(pvals[k,]<=.01) { sstars[k] = noquote("**")
                } else if(pvals[k,]<=.05) { sstars[k] = noquote("*")
                } else if(pvals[k,]<=.1)  { sstars[k] = noquote(".")
                } else { sstars[k] = noquote("") }
} #!!!!
 
MSE =  summary.df[nspa,3]
aic =  model.gam$aic
coefstars = cbind(  rbind(pvals, AIC = "") , #summary(log.reg.model.preliminary1_np)$parametric.anova,
                        "coef" = c(model.gam$coeff[-1], MSE, aic) ,
                        sstars
					)[,-1]
                        
############################################
## output

if( !is.character(append.to) | is.null(append.to) | append.to == "" ){

	result = coefstars

}else{

	if( !exists( append.to , inherits = TRUE) ){
		result = coefstars
	}else{
		result = get( append.to ,inherits = TRUE )
		result = cbind(result,coefstars)
	}

}

class(result) = c("data.frame","coefstars")

#if(envir == -1){
#   #if(print){print(result)}
#	# assign( "result" , result , pos = -1 )
#}else
if(envir != -1){
	if(print){print(result)}
	assign( append.to , result , pos = envir )
	result = TRUE
}

result

}

########################################################################################################################

summary.ak.gam = function(...){   ## for backward compatybility
	summary.gam.coefstars(...)
}

########################################################################################################################
########################################################################################################################
summary.gam.signs = function( coefstars.df , envir = -1 , append = FALSE , print = TRUE ){

	## coefstars.df = summary.coefstars

name.df = deparse(substitute(coefstars.df))

if( sum(class(coefstars.df) != c("data.frame","coefstars"))>0 ){
	stop("The argument must be of class c(\"data.frame\",\"coefstars\").")
}

###########################
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

############################################
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

}

########################################################################################################################
########################################################################################################################
summary.gam.coefstars.agg = function( coefstars.df , FUN = mean , names.agg = NULL
				, envir = -1 , append = FALSE , print = TRUE ){

	## coefstars.df = summary.coefstars

if( sum(class(coefstars.df) != c("data.frame","coefstars"))>0 ){
	stop("The argument must be of class c(\"data.frame\",\"coefstars\").")
}

name.df = deparse(substitute(coefstars.df))


if(is.null(names.agg)){
	names.agg = if(is.character(FUN)){FUN}else{deparse(substitute(FUN))}
}


###########################
idx = (1:(ncol(coefstars.df)/2))*2 - 1
coefstars.df.agg = apply( coefstars.df[ , idx , drop=FALSE] , 1 , FUN )

if(is.null(dim(coefstars.df.agg))){coefstars.df.agg = rbind(coefstars.df.agg)}

	# class(coefstars.df.agg)

coefstars.df.agg = as.data.frame( t(coefstars.df.agg) )
	if( sum(is.null(colnames(coefstars.df.agg))) > 0 | sum(is.na(colnames(coefstars.df.agg))) > 0 ){
		colnames(coefstars.df.agg) = names.agg
	}

############################################
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

}


########################################################################################################################
########################################################################################################################
##  model.gam = log.reg.model.preliminary1_np

summary.gam.signif = function( model.gam , append.to = NULL , envir = -1 , print = TRUE){
#
summary.df = summary(model.gam)$parametric.anova
nspa = nrow(summary.df)
rownames( summary.df )[nspa] = "MSE"
pvals = summary.df[5];     ## data frame!

#	class(pvals) <- c("data.frame","signif")

pvals = rbind(pvals, AIC = NA)
pvals["MSE",] =  summary.df[nspa,3]
pvals["AIC",] =  model.gam$aic

# pvals

############################################
## output

if( !is.character(append.to) | is.null(append.to) | append.to == "" ){

	result = pvals

}else{

	if( !exists( append.to , inherits = TRUE) ){
		result = pvals
	}else{
		result = get( append.to ,inherits = TRUE )
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
	assign( append.to , result , pos = envir )
	result = TRUE
}

result

}

########################################################################################################################
########################################################################################################################


summary.gam.signif.agg = function( signif.df , FUN = mean , envir = -1 , append = FALSE , print = TRUE){

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

############################################
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

}

########################################################################################################################
########################################################################################################################
