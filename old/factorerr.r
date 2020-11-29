factorerr = function(p,m){
# Hat-matrix for the factor variable.
# The diag(factorerr) are the coefficients to myltiply by the s2=MSE/dfe
# to obtain the estimate of the variance of the estimator of the coefficient 
# for corresponding factor level.
# They are always reciprocals of integers.
# see mmcc(model)
n=p*m
X=matrix(rep(1,n),nrow=n)
a=rep(1,m)
L=diag(p-1)
L=kronecker(L,a)
U=matrix(rep(0,n-m),nrow=m)
X=cbind(X,rbind(U,L))
solve(t(X)%*%X)
}
