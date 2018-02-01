########################################################################################################################—•°
## FUNCTIONS HERE          {SumMod}
##    signif_stars(pvals,check_prob=TRUE,if_not_prob ="",if_na=NA,noquote=FALSE)
##
## DEPENDENCIES
##
## TODO
##
########################################################################################################################—•°

########################################################################################################################—•°
signif_stars = function(pvals,check_prob=TRUE,if_not_prob ="",if_na=NA,noquote=FALSE){
###################################################################################################—•°
##
###################################################################################################—•°
if(!is.numeric(pvals)){stop("'pvals' must be numeric vector.")}
if( check_prob & any( pvals[!is.na(pvals)]<0 | pvals[!is.na(pvals)]>1 ) ){stop("'pvals' must be vector of probabilities.")}
   #
p2sign = function(p,if_not_prob,if.na){
   if(is.na(p)){ if_na
   }else{
      if(p<0){ if_not_prob
      } else if(p<=.001){ "***"
      } else if(p<=.01) { "**"
      } else if(p<=.05) { "*"
      } else if(p<=.1)  { "."
      } else if(p<=1)   { ""
      } else { if_not_prob }
   }
}
if(noquote){
   result <- noquote(sapply(pvals,function(x){p2sign(x,if_not_prob,if_na)}))
}else{
   result <- sapply(pvals,function(x){p2sign(x,if_not_prob,if_na)})
}
class(result) <- union(class(result),"signif_stars")
result
} ##----END----##
########################################################################################################################—•°

########################################################################################################################—•°
## EXAMPLE
dummy  = function(){

pvals = c(-.1,.0005,.005,.02,.07,.1,.2,1,2,NA)
data.frame(pvals = pvals,sstars = signif_stars(pvals))
data.frame(pvals = pvals,sstars = signif_stars(pvals,check_prob=FALSE))
data.frame(pvals = pvals,sstars = signif_stars(pvals,check_prob=FALSE,noquote=TRUE))
## but
signif_stars(pvals,check_prob=FALSE)
signif_stars(pvals,check_prob=FALSE,noquote=TRUE)
data.frame(pvals = pvals,sstars = signif_stars(pvals,check_prob=FALSE,if_not_prob="err"))
data.frame(pvals = pvals,sstars = signif_stars(pvals,check_prob=FALSE,if_na="NA"))
##
signif_stars(pvals,check_prob=FALSE) ## NA returnes formal NA
signif_stars(pvals,check_prob=FALSE,if_na="NA") ## NA returns string "NA""
signif_stars(pvals,check_prob=FALSE,if_na="NA",noquote=TRUE) ## NA returns string "NA" but quotation marks are supressed
##
signif_stars(pvals,check_prob=FALSE,if_not_prob=NA) ##
signif_stars(pvals,check_prob=FALSE,if_not_prob="NA") ##
signif_stars(pvals,check_prob=FALSE,if_not_prob="NA",if_na="NA") ##
##
ss <- signif_stars(pvals,check_prob=FALSE)
ss
class(ss)
##
ss <- signif_stars(pvals,check_prob=FALSE,noquote=TRUE)
ss
class(ss)

}
rm(dummy)
########################################################################################################################—•°

