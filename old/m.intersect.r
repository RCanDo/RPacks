m.intersect = function(set.list){
n=length(set.list)
inter = set.list[[1]]
for (j in 2:n) inter = intersect(inter,set.list[[j]])
inter
}