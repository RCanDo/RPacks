univariate.models.plots = function(
   data = dataref ## data frame containing proper variables which names are specified below
  , var.names = c( var.name, "CzyDobra", "skutecznosc", "saldo_pocz", "wplaty_new")
  , FUN=function(x)x
  , alpha = .05
  , coldef = ""
  , pch="." , cex = 1
  ,... ## passed to graphical functions
){
   if(any(!var.names %in% names(data))){
   stop("One of the variables from 'var.names' is not contained in 'data'.")
   }


  X = FUN(data[, var.names[1]])
  CzyDobra = data[, var.names[2]]
  skutecznosc = data[, var.names[3]]
  sp = round(data[, var.names[4]])
  wn = round(data[, var.names[5]])
  
   if(any(!unique(CzyDobra) %in% c(0, 1)) | length(unique(CzyDobra))==1){
   stop("The second variable on the list 'var.names' must be 0-1 valued and both values must be present.\n
         It is usually called 'GoodBad' and indicates 'good' and 'bad' members of the population.")
   }
   
   if( is.factor(skutecznosc) | is.factor(sp) | is.factor(wn) )
   {stop("The third, fourth and fifth variables on the list 'var.names' must be all numeric.")}

   if(any(skutecznosc < 0)){
   warning("The third variable on the list 'var.names' should be non-negative.\n
            It is usually called 'efficiency' and indicates efficiency of a member of the population.")
   }
   
   if(any(sp<=0)){
   warning("The fourth variable on the list 'var.names' should be strictly positive.")
   }
   
   if(any(wn<0)){
   warning("The fifth variable on the list 'var.names' should be non-negative.")
   }

   #############################################################################

windows(width=9, height=9);par(mfrow=c(2, 2), oma=c(0, 0, 1, 0)); margins.my()
coldef(coldef)

if(!is.factor(X)){

   X.gbs = split(X, CzyDobra)

   # 1 histogram
   hist.all = hist(X, plot = FALSE)
   hist.list = lapply( X.gbs, function(x){ hist(x, breaks = hist.all$breaks, plot = FALSE)$counts} )
   barplot( rbind(hist.all$counts, hist.list$`1`, hist.list$`0` ),
            beside = TRUE,
            col = c("darkgrey", "darkgreen", "red"),
            names.arg = signif(hist.all$breaks[-length(hist.all$breaks)], 3),
            xlab=var.names[1],
            ylab = "counts")
   legend("topright", legend=c("all", "goods", "bads"), col = c("darkgrey", "darkgreen", "red"), pch=15)
   title(paste0(var.names[1], " -- histogram"))



   # 2
   n = length(X);  d = (1:n)/n
   v1 = sort(X.gbs$`1`); n1 = length(v1); d1 = (1:n1)/n1
   v0 = sort(X.gbs$`0`); n0 = length(v0); d0 = (1:n0)/n0
   
   if(1){
      ks.pval = ks.test(v1, v0)$p.value
   }
   
   plot( d~sort(X), col="grey", pch=".", xlab=var.names[1], ylab="distribution")
   lines(d1~v1, col="darkgreen", pch=".")
   lines(d0~v0, col="red", pch=".")
   abline(h=c(0, 1), col="darkgrey")
   title(paste0(var.names[1], " ~ ", var.names[2]))
   
   #########
   ## models

   mod.l = glm(CzyDobra~X, family=binomial(link="logit"))
   mod.a = gam(CzyDobra~s(X), family=binomial(link="logit"))#, method="ML" )
       sucfail = cbind(wn, pmax(sp-wn, 0))
   mod.lsf = glm(sucfail ~ X, family=binomial(link="logit"))
   mod.asf = gam(sucfail ~ s(X), family=binomial(link="logit"))#, method="ML" )

   ## Inne modele; gdzie to przypiac ???
   #   mod.lp = glm( wn ~   X,  family=poisson(link="log") )
   #   mod.ap = gam( wn ~ s(X), family=poisson(link="log") )#, method="ML" )
   #   mod.lg = glm( I(wn+.01) ~  X,  family=Gamma(link="log") )
   #   mod.ag = gam( I(wn+.01) ~  X,  family=Gamma(link="log") )#, method="ML" )
   #   ### i jeszce:
   #   mod.lp = glm( wn ~   X + sp,  family=poisson(link="log") )
   #   mod.ap = gam( wn ~ s(X) + s(sp), family=poisson(link="log") )#, method="ML" )
   #   mod.lg = glm( I(wn+.01) ~  X + sp,  family=Gamma(link="log") )
   #   mod.ag = gam( I(wn+.01) ~  s(X) + s(sp),  family=Gamma(link="log") )#, method="ML" )

   
   # 3
      xx = order(X)
   plot(CzyDobra~X, pch=".", xlab=var.names[1], ylab=var.names[2])
   lines(predict(mod.l, type="response")[xx] ~ X[xx], col=5, lwd=2)
   lines(predict(mod.a, type="response")[xx] ~ X[xx], col=6)
   title(paste0(var.names[2], " ~ ", var.names[1]))

   # 4
   plot(skutecznosc~X, xlab=var.names[1], ylab=var.names[3], pch=pch , cex = cex);
   abline(h=c(0, 1), col="grey")
   lines(predict(mod.lsf, type="response")[xx] ~ X[xx], col=5, lwd=2)
   lines(predict(mod.asf, type="response")[xx] ~ X[xx], col=6)
   title(paste0(var.names[3], " ~ ", var.names[1]))

   mtext(var.names[1], outer=TRUE)
   
   
   
  ## Diagnosis:,
   models = list(mod.l=mod.l, mod.a=mod.a, mod.lsf=mod.lsf, mod.asf=mod.asf)#, mod.lp=mod.lp, mod.ap=mod.ap)
   anova = list(anova.gb = anova(mod.l, mod.a), anova.sf=anova(mod.lsf, mod.asf))#, anova.p = anova(mod.lp, mod.ap))
   aic = list(aic.gb = AIC(mod.l, mod.a), aic.sf=AIC(mod.lsf, mod.asf))#, aic.p = AIC(mod.lp, mod.ap))


  ## H0: linear model is enough. / H1: additive model fits signifficantly better
   anova.p.vals = unlist(lapply(anova, function(x){1 - pchisq(abs(x[2, 4]), df=x[2, 3])}))
   aic.diff = unlist(lapply(aic, function(x){diff(x[, 2])}))
   anova.choice = ifelse( anova.p.vals>alpha, rep("linear", 2), rep("additive", 2) )
   aic.choice = ifelse( aic.diff>0, rep("linear", 2), rep("additive", 2) )
   wiggliness = unlist(lapply(models[(1:2)*2], function(x){summary(x)$edf}))
   
   result.df = data.frame(anova.p.vals, anova.choice, aic.choice, aic.diff, wiggliness)
   rownames(result.df) = c("GoodBad", "SuccessFailures")#, "Poisson")
   
   print(output <- list(result = result.df, anova = anova, aic = aic, models = models))

## ----------------------------------------------------------
}else{  ## variable is factor

   nol = length(levels(X))      ## number of levels

   # 1
   gbt = table(CzyDobra, X)
   barplot(  rbind(table(X), gbt[c(2, 1),]),
            beside = TRUE,
            col = c("darkgrey", "green", "red"),
            names.arg = levels(X),
            xlab=var.names[1],
            ylab="counts" )
   legend( "topright" , legend=c("all", "goods", "bads") , col = c("darkgrey", "green", "red") , pch=15)
   title(paste0(var.names[2], " ~ ", var.names[1]), " -- histogram")

   # 2
   skut.d = density(skutecznosc)
   plot(skut.d$y~skut.d$x , col="darkgrey" , pch=pch , type="l"
         , xlab=var.names[3], ylab="density", lwd=2)
   #
   skut.s = split(skutecznosc, X)
      palette("default")
      colrs = rep(c(2, 3, 4, 5, 6), length=nol)       #!!!!
   for(j in 1:nol) {
      skut.d = density(skut.s[[j]])
      lines(skut.d$y~skut.d$x , col=colrs[j] , pch="." , type="l"
            , xlab="skutecznosc", ylab="density")
   }
   legend("topright", legend=c(names(skut.s)), col=1:length(skut.s)+1, lty=1)
   title(paste0(var.names[3], " ~ ", var.names[1]))

   ##########################
   ## models
   ## For factor variables (categhorical) gam models do not provide any new evidence
   # mod.l = glm(CzyDobra ~ X, family=binomial(link="logit"))
    mod.l = lm(CzyDobra ~ X)
       sucfail = cbind(wn, pmax(sp-wn, 0))
    mod.lsf = glm(sucfail ~ X, family=binomial(link="logit"))
    mod.ls = lm(skutecznosc ~ X)    ## we cannot do modelling completely analogous to the above
    

   ## Inne modele; gdzie to przypiac ???
   #   mod.lp = glm( wn ~   X,  family=poisson(link="log") )
   #   mod.ap = gam( wn ~ s(X), family=poisson(link="log") )#, method="ML" )
   #   mod.lg = glm( I(wn+.01) ~  X,  family=Gamma(link="log") )
   #   mod.ag = gam( I(wn+.01) ~  X,  family=Gamma(link="log") )#, method="ML" )
   #   ### i jeszce:
   #   mod.lp = glm( wn ~   X + sp,  family=poisson(link="log") )
   #   mod.ap = gam( wn ~ s(X) + s(sp), family=poisson(link="log") )#, method="ML" )
   #   mod.lg = glm( I(wn+.01) ~  X + sp,  family=Gamma(link="log") )
   #   mod.ag = gam( I(wn+.01) ~  s(X) + s(sp),  family=Gamma(link="log") )#, method="ML" )
   ##########################
   


   cols = c("cyan", "magenta", "red")
   legends = c( paste0("mean(", var.names[2], ")"),
                paste0("mean(", var.names[3], ")"),
                paste0("sum(", var.names[5], ")/sum(", var.names[4], ")"))
   ltypes = c(1, 1, 2)
   
   # 3
     #yy = mod.l$coef+c(0, rep(mod.l$coef[1], nol-1))
   plot.means.fit(mod.l, ylim = c(0, 1.35), type="+", wdth=.1
               , xlab=var.names[1] , rotate=TRUE , alpha = alpha)
   abline(h=mean(CzyDobra), col=cols[1], lty=ltypes[1])
   abline(h=mean(skutecznosc), col=cols[2], lty=ltypes[2])
   abline(h=sum(wplaty_new)/sum(saldo_pocz), col=cols[3], lty=ltypes[3])
   abline(h=c(0, 1), col="grey", lwd=2)
   legend("topright", col = cols, legend=legends, lty=ltypes)
   title(paste0(var.names[2], " ~ ", var.names[1]))

   # 4
   plot.means.fit(mod.lsf , type="+" , wdth=.1 , ylim = c(0, max(skutecznosc)*1.05)
                  , xlab=var.names[1] , ylab=var.names[3]
                  , rotate=TRUE , alpha = alpha )
   points(as.numeric(X), skutecznosc, type="n", pch=pch)
   for(j in 1:nol){ points( rep(j, length(skut.s[[j]])) , skut.s[[j]], col=colrs[j], pch=pch , cex = cex) }
   abline(h=mean(CzyDobra), col=cols[1], lty=ltypes[1])
   abline(h=mean(skutecznosc), col=cols[2], lty=ltypes[2])
   abline(h=sum(wplaty_new)/sum(saldo_pocz), col=cols[3], lty=ltypes[3])
   abline(h=0, col="darkgrey", lwd=1)
   title(paste0(var.names[3], " ~ ", var.names[1]))


   mtext(paste0(var.names[1], " -- factor, ", nol, " levels"), outer=TRUE)
   
   models = list(mod.l = mod.l, mod.ls = mod.ls)#, mod.lp = mod.lp)
   anova = list(anova(mod.l), anova(mod.ls))#, anova(mod.lp))
   aic = AIC(mod.l, mod.ls)#, mod.lp)
   
  ## H0: X has no effect on Y == NULL model is enough
  ## H1: effect of X is significant == NULL model is significantly worse then FULL
   anova.p.vals = unlist(lapply(anova, function(x){ x[1, 5] }))
   names(anova.p.vals) = c("mod.l", "mod.ls")

      print(output <- list(anova.p.vals=anova.p.vals, anova = anova, aic = aic, models = models))
   
   }

output
}   ## END
