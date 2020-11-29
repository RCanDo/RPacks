## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {StatMath}
##  chisq.multi.test()
##
## DEPENDENCIES
##
## TODO
## •
##
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
chisq.multi.test=function( datfram
                         , only.factors = TRUE
                         , simulate.p.value=FALSE
                         , significance = .05
                         ){
## ------------------------------------------------------------------------------------------------—•°
## Multi chisq.test for all pairs of variables in datfram.
## By default only factors are considered.
## If  only.factors=FALSE  then all variables are taken.
##    Arguments
##  datfram                      data frame
##  only.factors = TRUE
##  simulate.p.value=FALSE
##  significance = .05
##
##    Result
## A list comprising
##  cor.pairs                    pairs of only correlated variables, i.e. for wich test gave  p.value < significance  or NaN
##  p.values.table               table of p.values for all pairs of variables; only lower-triangular is filled;
##                               the lower the p.value of chi^2 test the higher correlation between variables
##  variables                    names of variables chosen to be checked for chi^2 test;
##  variable.indices             indices of variables chosen to be checked for chi^2 test;
## ------------------------------------------------------------------------------------------------—•°

## datfram <- Data.ref

nams = names(datfram)
m = length(nams)
if(only.factors){
    f=sapply(datfram,is.factor)
    idx = (1:m)[f]
    nams = nams[f]
    datfram = datfram[,idx]
    m = sum(f) ## how many columns is in a data frame now
}
if(m<2){
stop("This function needs at least 2 variables in a given data frame to return meaningful results.
If 'only.factors = TRUE' (default) check if there are at least two factors in a data frame.")
}
cor.pairs = data.frame(var1="",var2="",p.value=significance)
chisq.table = numeric(0)
for(j in (2:m)){  ## j=2 ; nams[j]
  row_j = numeric(m-1)
    for( k in 1:(j-1) ){  ## k=1 ; nams[k]
            row_j[k] = chisq.test( table(datfram[,j],datfram[,k]),simulate.p.value=simulate.p.value )$p.value
            if( is.nan(row_j[k]) || row_j[k]<significance ){
               cor.pairs = rbind(cor.pairs ,data.frame( var1 = nams[j], var2 = nams[k] , p.value = row_j[k]  ) )
            }
    }
  chisq.table = rbind(chisq.table,row_j)
}
rownames(chisq.table) <- nams[-1]
colnames(chisq.table) <- nams[-m]

result = list( cor.pairs = cor.pairs[-1,], p.values.table = as.table(chisq.table), variables = nams, variable.indices = idx )

result

}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°


## ---------------------------------------------------------------------------------------------------------------------—•°
## EXAMPLES ############################################################################################################—•°
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
dummy = function(){
## This is dummy function — it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## — in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
## -------------------------------------------------------------------------------------------—•°

 ## RELOADER — before it works you need to source("PacksAK.R"); it's best to use StatMath.R within pack's dir.
 loadPacksAK("StatMath")

## -------------------------------------------------------------------------------------------—•°


}
## ---------------------------------------------------------------------------------------------------------------------—•°
rm(dummy)
