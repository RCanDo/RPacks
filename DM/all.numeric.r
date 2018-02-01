all.numeric = function(df){
m=dim(df)[2]
f=!sapply(df,is.factor)
NAs=integer(0)
uniqs = integer(0)
summaries = character(0)
for (j in (1:m)[f]){
  NAs = c(NAs, sum(is.na(df[,j])))
  uniqs=c( uniqs ,length(unique(df[!is.na(df[,j]),j])))
  summaries = c( summaries , paste(summary(df[,j])[1:6],collapse=", "))
}
numeric.info = cbind( (1:m)[f] , names(df)[f] , NAs , uniqs , summaries )
colnames(numeric.info) = c( "id", "name" , "NAs" ,"unique_values" , "summary" )
numeric.info = as.data.frame(numeric.info)     ## 'nr' and 'unique.values' becomes factors...
numeric.info$id = as.numeric(as.character(numeric.info$id))
numeric.info$unique_values = as.numeric(as.character(numeric.info$unique_values))
numeric.info
}
