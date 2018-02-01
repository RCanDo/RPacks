moving.var = function(x,window=3){
n=length(x)
h=ceiling(window/2)
mv.x = numeric(n)
mv.x[1:(h-1)] = NA
mv.x[(n-h+2):n] = NA
for(t in h:(n-h+1)) mv.x[t]=var(x[(t-h+1):(t+h-1)])
mv.x
}