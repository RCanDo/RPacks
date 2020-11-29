REdf=function(obj.mer){
## obj.mer = object of class "mer" obtained by lmer() from 'lme4' library
sumer = summary(obj.mer)@REmat   ## RANDOM EFFECTs estimates of variances and covariances
d1 = dim(sumer)[1]
d2 = dim(sumer)[2]
re = matrix(as.numeric(sumer[1:d1,3:d2]),nrow=d1)
w=apply(re,2,function(x)any(!is.na(x)))
re=re[,w]
re.df = data.frame(re);
re.df = data.frame(cbind(sumer[,1:2],re.df))
colnames(re.df) = colnames(sumer)[c(TRUE,TRUE,w)]
re.df
}

REtheta=function(obj.mer){   #! not good, needs corrections, but what for? slot @ST gives the same
## obj.mer = object of class "mer" obtained by lmer() from 'lme4' library
re.df = REdf(obj.mer)
d1 = dim(re.df)[1]
d2 = dim(re.df)[2]
rse = re.df[d1,4]
groups = as.character(re.df[,1])
   for(i in 2:d1)if(groups[i]==""){groups[i]=groups[i-1]}
   re.df[,1]=groups
   groups = as.factor(unique(groups))
g = length(groups)
theta = as.list(groups)
for(i in 1:g){
theta[[i]] = re.df[re.df$Groups==groups[i],4:d2]/rse
rownames(theta[[i]]) = re.df[re.df$Groups==groups[i],2]
}
names(theta) = groups
theta
}

#obj.mer@ST  ## sth wrong with correlations... BLEEE....
