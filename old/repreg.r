repreg = function(regdat,sb){
# bootsrap for regression data
n=dim(regdat)[1]
m=dim(regdat)[2]
boot.coef = NULL
for (i in 1:sb) {
boot = regdat[sample(n,replace=T),]
model = lm(boot[,1] ~ boot[,-1])
boot.coef = rbind(boot.coef,model$coeff)
}
par(fg="yellow",bg="black",col.axis="white",col.lab="grey",col.main="white")
par(mar=c(2.2,2,2,1),tck=.01,mgp=c(1,.1,0))
par(mfrow=c(m,1))
for (i in 1:m)
hist(boot.coef[,i],main=i-1)
par(mfrow=c(1,1))
boot.q = apply(boot.coef,2,function(x)quantile(x,c(0.025,0.975)))
t(boot.q)
}
