## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE          {SumMod}
##  summary.panova.gam()
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

summary.panova.gam = function( panova.df    ## panova.gam
   ## for summary for coefficients
      , FUN.c = default.c , add_colnames.c = c(TRUE,FALSE,TRUE,TRUE,FALSE)
   ## for summary for gof (goodness of fit) measures (like R2, R2adj, AIC, MSE, Gini, ...)
      , FUN.m = default.m , add_colnames.m = TRUE
      , append_to = NULL , row = NULL   ## to which data frame append a result and for which row substitute (if not simply append it)
      , envir = 1     ## .GlobalEnv
      , print = FALSE
      , rowname = NULL){
## ------------------------------------------------------------------------------------------------—•°
##
## ------------------------------------------------------------------------------------------------—•°

default.c <- list( "coeffs" = function(x){c("mean" = mean(x))}
               , "coeffs" = function(x){signs.table(x)}
               , "Pr(>F)" = function(x){c("mean" = mean(x))}
               , "Pr(>F)" = function(x){c("sstars" = signif_stars(mean(x)))}
               , "sstars" = function(x){sstars.table(x) }
               )

default.m <- list( "MSE"  = function(x){c("min"=min(x),"mean"=mean(x),"max"=max(x))}
                  , "AIC"  = function(x){c("min"=min(x),"mean"=mean(x),"max"=max(x))}
                  , "Gini" = function(x){c("min"=min(x),"mean"=mean(x),"max"=max(x))}
                  , "AUC"  = function(x){c("min"=min(x),"mean"=mean(x),"max"=max(x))}
                  , "p.mww" = function(x){c("mean"=mean(x))}
                  )

sum.c <- summary.panova.gam.c( panova.df , FUN = FUN.c , add_colnames = add_colnames.c )
sum.m <- summary.panova.gam.m( panova.df , FUN = FUN.m , add_colnames = add_colnames.m
   , append_to = append_to , row = row   ## to which data frame append a result and for which row substitute (if not simply append it)
	, envir = envir     ## .GlobalEnv
	, print = print
   , rowname = rowname )

structure( list( coefficients = sum.c
               , measures     = sum.m
               )
         , class = c("summary.panova.gam","summary.panova","summary")
         )

} ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°


## ---------------------------------------------------------------------------------------------------------------------—•°
summary.panova.gam.c = function( panova.df , FUN = list() , add_colnames = TRUE ){ #, envir = 1 , append = FALSE , print = TRUE ){
## ------------------------------------------------------------------------------------------------—•°
##
## ------------------------------------------------------------------------------------------------—•°
if( sum(class(panova.df) %in% c("data.frame","panova"))<2 ){
	stop("The argument must be of class c(\"data.frame\",\"panova\").")
}

## excluding non-parameter rows
panova.df0 = panova.df[setdiff(rownames(panova.df),c("Residuals","MSE","AIC","Deviance","Gini","AUC","p.mww","t.mww","R2","CV_R2")),]

## ------------------------------------—•°
## if FUN is not defined/empty
##
if(is.null(FUN)||length(FUN)==0){
   ##
   colnames.unique = setdiff(unique(colnames(panova.df)),"sstars")
   n = length(colnames.unique)
   FUN <- list()
   add_colnames <- logical(0)
   ##
   ## we do mean(x) for all columns
   for( k in 1:n ){
      add_colnames = c(add_colnames,TRUE)
      FUN[[k]] <- function(x){c("mean" = mean(x))}
   }
   if( "Pr(>F)" %in%  colnames.unique ){
      n = n+1
      colnames.unique = c(colnames.unique,"Pr(>F)")
      add_colnames = c(add_colnames,FALSE)
      FUN[[n]] = function(x){c("sstars" = signif_stars(mean(x)))}
   }
   ##
   ## if  sstars  present we do  sstars.table(x)
   if( "sstars" %in%  colnames(panova.df) ){
      n = n+1
      colnames.unique = c(colnames.unique,"sstars")
      add_colnames = c(add_colnames,FALSE)
      FUN[[n]] = function(x){ sstars.table(x) }
   }
   names(FUN) <- colnames.unique
}
## ------------------------------------—•°

colnames.result = character(0)
result = data.frame(row.names = rownames(panova.df0))

if(length(add_colnames)<length(FUN)){
   add_colnames = lengthen(add_colnames,length(FUN))
}

for(f in 1:length(FUN)){ # f=2
   fname = names(FUN)[f]
   which_f = grep(fname,colnames(panova.df0),fixed=TRUE)  #!#!#! fixed=TRUE  #!#!#!
   df0_f = panova.df0[,which_f,drop=FALSE]
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

class(result) = c("summary.panova.gam.coeffs","data.frame")
result

} ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
summary.panova.gam.m = function( panova.df , FUN = list() , add_colnames = TRUE
   , append_to = NULL , row = NULL   ## to which data frame append a result and for which row substitute (if not simply append it)
	, envir = 1     ## .GlobalEnv
	, print = TRUE
   , rowname = NULL ){ #, envir = 1 , append = FALSE , print = TRUE ){
## ------------------------------------------------------------------------------------------------—•°
##
## ------------------------------------------------------------------------------------------------—•°

if(is.null(FUN)||length(FUN)==0){
   FUN <- list( "MSE"  = function(x){c("min"=min(x),"mean"=mean(x),"max"=max(x))}
              , "AIC"  = function(x){c("min"=min(x),"mean"=mean(x),"max"=max(x))}
              )
}

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

result = p.agg.adds

if( !is.null(append_to) && is.character(append_to) && append_to != "" ){

	if( exists( append_to , envir = envir , inherits = TRUE) ){
		result.app = get( append_to , envir = envir , inherits = TRUE )
      if( is.null(row) ){
	      result.app = rbind( result.app , p.agg.adds )
      }else{
         row = (round(row))
         if( is.numeric(row) && row>0 && row<=nrow(result.app)+1 ){
            result.app[row,] <- p.agg.adds
         }else{ stop("'row' must be integer between 1 and nrow(get(append_to))+1 or NULL.") }
      }
	}else{
      result.app = rbind(result)
   }


   if(is.null(rowname)){
      rowname = nrow(result.app)
   }
   rownames(result.app)[nrow(result.app)] = rowname
   result.app = as.data.frame(result.app)

   class(result.app) = c("summary.panova.gam.measures","data.frame")

 	assign( append_to , result.app , pos = envir )

   if(print){print(result.app)}

}

class(result) <- c("summary.panova.gam.measures")
result

} ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°



## ---------------------------------------------------------------------------------------------------------------------—•°
sstars.table=function(x){ ## x is character vector
   c("***"=sum(x=="***"),"**"=sum(x=="**"),"*"=sum(x=="*"),"."=sum(x=="."),"<insig.>"=sum(x==""))
}
## ---------------------------------------------------------------------------------------------------------------------—•°
signs.table=function(x){ ## x is numeric
   y = c("x<0"=sum(x<0),"x>0"=sum(x>0))
   c(y,"min(+|-)"=min(y))
}
## ---------------------------------------------------------------------------------------------------------------------—•°
## EXAMPLE
dummy  = function(){
   summary.panova.gam(sum.df,FUN = list(coeffs=function(x){mean(x)},coeffs=function(x){sum(x)}))
   summary.panova.gam(sum.df,FUN = list(coeffs=function(x){mean(x)},coeffs=function(x){sum(x)}
      ,sstars=function(x){as.vector(table(x))}))

}
## ---------------------------------------------------------------------------------------------------------------------—•°
## EXAMPLE
dummy  = function(){
  #  panova.df = summary.df_k
summary.panova.gam.measures( panova.df , FUN = list( "AIC" = function(x){c("min"=min(x),"max"=max(x))} , "AIC" = function(x){c("mean"=mean(x))} ,
                                                     "MSE" = function(x){c("min"=min(x),"max"=max(x))} , "MSE" = function(x){c("mean"=mean(x),"sd"=sd(x))} )
   )

}
## ---------------------------------------------------------------------------------------------------------------------—•°


## ---------------------------------------------------------------------------------------------------------------------—•°
