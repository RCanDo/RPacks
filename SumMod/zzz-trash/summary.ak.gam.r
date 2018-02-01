summary.ak.gam = function(model.gam){
#
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
summary.ak.df = cbind(  rbind(pvals, AIC = "") , #summary(log.reg.model.preliminary1_np)$parametric.anova,
                        "coef" = c(model.gam$coeff[-1], MSE, aic) ,
                        sstars
							)
                        
summary.ak.df[,-1]

#

}


##  model.gam = log.reg.model.preliminary1_np