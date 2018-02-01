ma=function(x,w=rep(1,3)/3){
## x is vector or matrix!
h=length(w)
k=ceiling(h/2)
n=length(x)
X=x[1:(n-h+1)]
for (i in 2:h)
{X=cbind(X,x[i:(n-h+i)])}
dim(w) = c(h,1)
movav=ts(X %*% w)
# tsp(movav)=c(k,(n-h+k),1)
movav
}
