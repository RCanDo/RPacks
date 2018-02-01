aa = matrix(1:12,ncol=3)
aa
bb = matrix(sample(1:12),ncol=3)
bb
outer(1:(bb)) [which(aa<bb)]

outer(1:4,1:3,function(x,y){c(x,y)})
aa[as.matrix(expand.grid(1:4,1:3))[which(aa<bb),]]

  combn(1:4,2)