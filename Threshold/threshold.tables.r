threshold.tables = function(
                            datfram = Data.ref
                          , goodsallratio = .1
                          , efficiency.var = "efficiency"
                          , variables = NULL
                          , what = NULL       ## one of  "p.val.fac","p.val.num","gini","auc"
                          , graphics = FALSE
                          , coldef = "black"
                          , integer.as.factor = FALSE
){
##  Calculates tables of p.values for a series of splitting for each variable in data frame
##  indicated by 'variables'.
##   ......
##

variable = datfram[,efficiency.var]

if(!is.null(variables)){
    datfram = datfram[,variables]
}

if(is.null(what)){
   what = c("p.val.fac","p.val.num","gini","auc")
}

whatbool = c("p.val.fac","p.val.num","gini","auc") %in% what
pf = whatbool[1]
pn = whatbool[2]
g  = whatbool[3]
a  = whatbool[4]


nt = nulls.table(datfram)


if(pf){
  factors  = as.character( nt$name[  nt$class == "factor"   &   nt$unique_values>1   ] )

  if(integer.as.factor){
    integers  = as.character( nt$name[  nt$class == "integer"   &   nt$unique_values>1   ] )
    for(k in integers){
       datfram[,k] = factor(datfram[,k],ordered=TRUE)     #!!!!!!!
    }
    factors = c(factors,integers)
  }
  
  if( length(factors)==0 ){ pf=FALSE }else{ p.vals.fac.df = data.frame(factors = factors) }
}

if(any(c(pn,g,a))){
    if(!integer.as.factor){ clas = c("numeric","integer") }else{clas = "numeric"}
        numerics = as.character( nt$name[  nt$class %in% clas   &   nt$unique_values >1  ] )
    if(length(numerics)==0){ pn <- g <- a <- FALSE }else{
        if(pn)  p.vals.num.df = data.frame(numerics = numerics)
        if(a)   auc.df = data.frame(numerics = numerics)
        if(g)   gini.df = data.frame(numerics = numerics)
    }
}
  
for( alf in goodsallratio ){     ## petla po thresholdach
    print(alf)
    GoodsBads = set.threshold(  variable , goodsallratio = alf , message = FALSE )$GoodsBads


    if(pf){
        p.vals = numeric(0)
        for(k in factors){   ## petla po zmiennych
              NAs = is.na(datfram[ , k ])
            ss.pval = split.stats( datfram[ !NAs , k ] , GoodsBads[ !NAs ] , message = FALSE)$p.value
            p.vals = c( p.vals , if(is.null(ss.pval)){ NA }else{ ss.pval } )    #!!!!!!!!!!!!!!!!
        }
        p.vals = round(p.vals,4)
        colnams = colnames(  p.vals.fac.df )
        p.vals.fac.df = cbind(  p.vals.fac.df, p.vals)
        colnames( p.vals.fac.df ) = c( colnams , as.character(alf) )
    }

    if(any(c(pn,g,a))){
        if(pn) p.vals = numeric(0)
        if(a) aucs = numeric(0)
        if(g) ginis = numeric(0)
        for(k in numerics){   ## petla po zmiennych
              NAs = is.na(datfram[ , k ])
            ss = split.stats( datfram[ !NAs , k ] , GoodsBads[ !NAs ] , message = FALSE)
            if(is.null(ss)){                                                    #!!!!!!!!!!!!!!!!
                if(pn) p.vals = c( p.vals ,NA )
                if(a)  aucs   = c( aucs   ,NA )
                if(g)  ginis  = c( ginis  ,NA )
            }else{
                if(pn) p.vals = c( p.vals ,ss$p.value )
                if(a)  aucs   = c( aucs   ,ss$auc )
                if(g)  ginis  = c( ginis  ,ss$gini.index )
            }
        }
        if(pn){ p.vals = round(p.vals,4)
                colnams = colnames(  p.vals.num.df )
                p.vals.num.df = cbind(  p.vals.num.df, p.vals)
                colnames( p.vals.num.df ) = c( colnams , as.character(alf) )
        }
        if(a){  aucs = round(aucs,4)
                colnams = colnames(  auc.df )
                auc.df = cbind(  auc.df , aucs)
                colnames( auc.df ) = c( colnams , as.character(alf) )
        }
        if(g){  ginis = round(ginis,4)
                colnams = colnames(  gini.df )
                gini.df = cbind(  gini.df , ginis)
                colnames( gini.df ) = c( colnams , as.character(alf) )
        }
    }

}

result = list()
if(pf){
      rownames(p.vals.fac.df) = p.vals.fac.df[,1]
      p.vals.fac.df = p.vals.fac.df[,-1]
      p.vals.fac.df = rbind(p.vals.fac.df,mean.value = round(colSums(p.vals.fac.df)/nrow(p.vals.fac.df),4)  )
      result$p.vals.factors.df = p.vals.fac.df
}
if(pn){
      rownames(p.vals.num.df) = p.vals.num.df[,1]
      p.vals.num.df = p.vals.num.df[,-1]
      p.vals.num.df = rbind(p.vals.num.df,mean.value = round(colSums(p.vals.num.df)/nrow(p.vals.num.df),4) )
      result$p.vals.numerics.df = p.vals.num.df
}
if(g){
      rownames(gini.df) = gini.df[,1]
      gini.df = gini.df[,-1]
      gini.df = rbind(gini.df,mean.value = round(colSums(gini.df)/nrow(gini.df),4) )
      result$gini.df = gini.df
}
if(a){
      rownames(auc.df) = auc.df[,1]
      auc.df = auc.df[,-1]
      auc.df = rbind(auc.df,mean.value = round(colSums(auc.df)/nrow(auc.df),4) )
      result$auc.df = auc.df
}


if(graphics){
  xlimits  =  c( min(goodsallratio) , max(goodsallratio)+diff(range(goodsallratio))*.2 )
  if(coldef == "black"){ palette(rainbow(6)) ; gridcol = "darkgrey" }else{ gridcol = "lightgray" }
  if(pf){
     windows();margins.my();coldef(coldef)
      nr = nrow(p.vals.fac.df) ;  lwidth = c(rep(1,nr-1),2)
      matplot(  goodsallratio , t(p.vals.fac.df), type = "l" , xlab = "GoodsAllRatio" , lwd = lwidth , xlim = xlimits )
      #! grid and abline  erases the matplot and matplot erases grid and abline... !!!! SHIT!!!!!
       grid(nx = NULL, ny = NA, col = gridcol, lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
      #abline( v=goodsallratio , col = gridcol , lty = "dotted")
      #matplot(  goodsallratio , t(p.vals.fac.df), type = "l" , xlab = "goodsallratio" , lwd = lwidth , xlim = xlimits )
      title("p.values for factors")
      legend("topright",legend=rownames(p.vals.fac.df), lty = 1:5, col=1:6 , cex = .7 , lwd = lwidth)
  }
  if(pn){
     windows();margins.my();coldef(coldef)
      nr = nrow(p.vals.num.df) ;  lwidth = c(rep(1,nr-1),2)
      matplot(  goodsallratio , t(p.vals.num.df), type = "l" , xlab = "GoodsAllRatio" , lwd = lwidth , xlim = xlimits )
       grid(nx = NULL, ny = NA, col = gridcol, lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
      title("p.values for numerics")
      legend("topright",legend=rownames(p.vals.num.df), lty = 1:5, col=1:6 , cex = .7 , lwd = lwidth)
  }
  if(g){
     windows();margins.my();coldef(coldef)
      nr = nrow(gini.df) ;  lwidth = c(rep(1,nr-1),2)
      matplot(  goodsallratio , t(gini.df), type = "l"     , xlab = "GoodsAllRatio" , lwd = lwidth , xlim = xlimits )
       grid(nx = NULL, ny = NA, col = gridcol, lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
      title("Gini Index for numerics")
      legend("topright",legend=rownames(gini.df), lty = 1:5, col=1:6 , cex = .7 , lwd = lwidth)
  }
  if(a){
      windows();margins.my();coldef(coldef)
      nr = nrow(auc.df) ;  lwidth = c(rep(1,nr-1),2)
      matplot(  goodsallratio , t(auc.df), type = "l"      , xlab = "GoodsAllRatio" , lwd = lwidth , xlim = xlimits )
       grid(nx = NULL, ny = NA, col = gridcol, lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
      title("Area Under ROC Curve for numerics")
      legend("topright",legend=rownames(auc.df), lty = 1:5, col=1:6 , cex = .7 , lwd = lwidth)
  }
  palette("default")
}

result

}
