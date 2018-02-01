mmcc=function(model){
# Model Matrix Covariance Coefficient
M=model.matrix(model)
aa=solve(t(M)%*%M)
colnames(aa)=NULL
row.names(aa)=NULL
aa
}
