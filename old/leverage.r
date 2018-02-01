leverage = function(xt,X){
# leverage at point xt for data X  (explanatory variables)
# MSE*leverage = Variance for reggression function at xt.
# xt is an argument of yhat(xt) -- the regression function
# X = [ 1 x ] -- design matrix (with 1's) ie.
# X = model.matrix(model).
# If one of the expl.variable is factor then 
# (assume treatment contrasts is set up)
# coordinates of xt corresponding to the levels of this factor
# should encode which level is in demand, ie. there must be 1 for that level
# and the other coordinates must be 0
# (thus if we need the baseline/reference/1-st level then there must be all zeros)
 K = solve(crossprod(X))
 xx = cbind(1,xt)
 # n=length(xt)
 # varyhat = numeric()
 lever = diag( xx %*% K %*% t(xx) )  ### improve it!
 lever
}

confreg = function(modlin,xrange,alpha=.05){
### only for one-dim regression
data = modlin[[12]]
n=dim(data)[1]
p=dim(data)[2]-1
X=as.matrix(cbind(1,data[,-1]))
y=data[,1]
s2 = summary.aov(modlin)[[1]][2,3]
xmin = min(xrange)
xmax = max(xrange)
xx = seq(xmin,xmax,(xmax-xmin)/100)
pow=rep(1:p,each=101)
xxpow = matrix(xx^pow,ncol=p) 
errs = sqrt(s2*leverage(xxpow,X))  # std.err. for regression line
yhat = cbind(1,xxpow) %*% modlin$coeff
## confidence intervals for regression line
tau = 1-alpha/2
up0 = yhat + errs*qt(tau,n-p-1)
dn0 = yhat - errs*qt(tau,n-p-1)     
ymin = min(dn0)
ymax = max(up0)
par(fg="yellow",bg="black",col.axis="white",col.lab="grey",col.main="white")
par(mar=c(2.2,2,2,1),tck=.01,mgp=c(1,.1,0))
plot(data[,2],data[,1],xlim=c(xmin,xmax),ylim=c(ymin,ymax),
    pch=20,xlab=names(data)[2],ylab = names(data)[1],main="Confidence and Prediction Intervals for ...")
lines(xx,yhat,col="orange")
lines(xx,up0,col="red")
lines(xx,dn0,col="red")
## prediction intervals for indyvidual observation
errspred = sqrt(s2*(1 + leverage(xxpow,X)))  # std.err. for individual value of yt given xt  
up1 = yhat + errspred*qt(tau,n-p-1)
dn1 = yhat - errspred*qt(tau,n-p-1)
lines(xx,up1,lty=2,col="grey")  # conf. int. for regression line
lines(xx,dn1,lty=2,col="grey")
                            
}