polynom= function(x,cf){
# cf=(c1,c2,...,ck)
# polynom(x,cf) = c1 + c2*x + ... ck*x^(k-1)
k=length(cf)
v=cf[k]
if(k>1){
for (j in 1:(k-1)){
v=cf[k-j] + v*x
}}
v
}