skew = function(x){
m3 = sum((x-mean(x))^3)/length(x)
s3 = sqrt(var(x))^3
m3/s3
}

kurtosis = function(x){
m4 = sum((x-mean(x))^4)/length(x)
s4 = var(x)^2
m4/s4 - 3
}

sign.test = function(x,y){
if(length(x)!=length(y)) stop("The two variables must be the same length.")
d = x-y
binom.test(sum(d>0),length(d))
}

stdize = function(x){
y = (x-mean(x))/sqrt(var(x))
y
}

