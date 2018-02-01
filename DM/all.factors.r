all.factors = function(datfram){
m=dim(datfram)[2]
f=sapply(datfram,is.factor)     ## logical
NAs=integer(0)
levs=character(0)
unique.values=integer(0)
for (j in (1:m)[f]) {
  NAs = c(NAs,sum(is.na(datfram[,j])))
  ls = levels(datfram[,j])
  levs_j = paste( ls, collapse=", " )
  if(nchar(levs_j)>70) {
    levs_j = substr(levs_j,1,70)
    levs_j = paste( c(levs_j,"..."), collapse="" )
  }
  levs = c( levs , levs_j )
  unique.values = c(unique.values , length(unique(datfram[!is.na(datfram[,j]),j])))
}

fact.info = cbind((1:m)[f] , names(datfram)[f], NAs , unique.values ,levs)
colnames(fact.info) = c("id","name", "NAs" ,"unique_values","levels_names")
fact.info = as.data.frame(fact.info)     ## 'nr' and 'levels.nr' becomes factors...

fact.info$id = as.numeric(as.character(fact.info$id))
fact.info$unique_values = as.numeric(as.character(fact.info$unique_values))
fact.info
}
