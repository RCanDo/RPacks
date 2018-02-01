all.of = function(class.name,datfram = NULL){
## Finding all variables in given data frame 'datfram' being of a class 'class.name'.
if(is.null(datfram)){
   stop("Data frame must be given.")
}

m = dim(datfram)[2]
classes = sapply(datfram,class)
of.class = unlist(lapply(classes,function(x){class.name%in%x}))


NAs=integer(0)
uniqs = integer(0)
summaries = character(0)

for (j in (1:m)[of.class]){
  NAs = c(NAs, sum(is.na(datfram[,j])))
  uniqs=c( uniqs ,length(unique(datfram[!is.na(datfram[,j]),j])))
  summaries = c( summaries , paste(summary(datfram[,j])[1:6],collapse=", "))
}

numeric.info = cbind( (1:m)[of.class] , names(datfram)[of.class] , NAs , uniqs , summaries )
colnames(numeric.info) = c( "id", "name" , "NAs" ,"unique_values" , "summary" )
numeric.info = as.data.frame(numeric.info)     ## 'nr' and 'unique.values' becomes factors...
numeric.info$id = as.numeric(as.character(numeric.info$id))
numeric.info$unique_values = as.numeric(as.character(numeric.info$unique_values))
numeric.info
}
