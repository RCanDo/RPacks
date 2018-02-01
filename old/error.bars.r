error.bars=function(yv,z,labs){
xv = barplot(yv,ylim=c(0,max(yv)+max(z)),names=labs,ylab=deparse(substitute(yv)))
g = diff(range(xv))/40
for(i in 1:length(xv)){
lines( c(xv[i],xv[i]),     c(yv[i]+z[i],yv[i]-z[i]) ,col="red")
lines( c(xv[i]-g,xv[i]+g), c(yv[i]+z[i],yv[i]+z[i]) ,col="red")
lines( c(xv[i]-g,xv[i]+g), c(yv[i]-z[i],yv[i]-z[i]) ,col="red")
}
}