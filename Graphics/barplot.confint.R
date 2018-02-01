barplot.confint = function(x,y,main=NULL,namx=NULL,namy=NULL,...){
########################################################################################################################
## Draws a barplot with confidence intervalse for the values of coefficients in the model
##    lm( y ~ x )
## where y is supposed to be a numeric variable and x a factor.
## x and y may be passed as names (charavters of length 1) of variables in parent env.
########################################################################################################################

if(is.null(namx)){namx = deparse(substitute(x))}
if(is.null(namy)){namy = deparse(substitute(y))}

if(length(x)==1 & is.character(x)){
namx = x
x = get(x)
}
if(!is.factor(x)){
stop("'x' must be a factor.")
}

if(length(y)==1 & is.character(y)){
namy = y
y = get(y)
}
if(!is.numeric(y)){
stop("'y' must be numeric.")
}


if(is.null(main)){
   main = paste(namy,namx,sep=" ~ ")
}

model = lm(y~x)
   coeff = model$coeff ; coeff = coeff[1] + c(0,coeff[-1])
   stderrs = summary(model)$coeff[,2]
   upper = coeff+2*stderrs
   lower = coeff-2*stderrs

m = length(coeff)

   ylim = c(min(0,lower),max(0,upper))


mp = barplot( tapply(y,x,mean) , main = main , xlab = namx , ylab = namy
             , ylim = ylim , ... )

mp = as.vector(mp)
span = diff(mp[1:2])/2

lines( c(mp[1]-span,mp[m]+span) , rep(mean(y),2) , col = "red" )

points( mp , lower , pch = "-" , cex=2 , col="red" )
points( mp , upper , pch = "-" , cex=2 , col="red" )

for( k in 1:length(coeff) ){

   lines( rep(mp[k],2) , c(lower[k],upper[k]) , col="red" )

}

}