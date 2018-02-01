nulls.imput = function(datfram
   ,vars.to.fill       ## full variables
   ,vars.full          ## variables to fill
   ,ntree = 50
   ,data.portion = 0.2){
## datfram = Data.all; vars.to.fill = do.uzupelnienia ; vars.full = variables.full.rf ;ntree = 50 ;data.portion = 0.2

nams.df = names(datfram)
lns = length(nams.df)
if(is.numeric(vars.to.fill)){
   if(max(vars.to.fill)>lns){
      stop("Some of the indices in 'vars.to.fill' are greater then number of variables in 'datfram'.")
   }
   vars.to.fill = nams.df[vars.to.fill]}
if(is.numeric(vars.full)){
   if(max(vars.full)>lns){
      stop("Some of the indices in 'vars.full' are greater then number of variables in 'datfram'.")
   }
   vars.full = nams.df[vars.full]}

if( any(!vars.to.fill %in% nams.df) ){
   stop("Some of the variables listed in 'vars.to.fill' are not present in data frame.")
}
if( any(!vars.full %in% nams.df) ){
   stop("Some of the variables listed in 'vars.full' are not present in data frame.")
}

X = datfram[,vars.full]
Y = datfram[,vars.to.fill]

for(vtf in vars.to.fill){

   y = Y[,vtf]
   class.y = class(y)
   na.idx = is.na(y)
   
   yy = y[!na.idx]
   xx = X[!na.idx,]
   names(yy) = rownames(xx)
   
   n = floor(length(yy)*data.portion)
   model.idx = sample(names(yy),n)
   

   model.rf = randomForest( y = yy[model.idx] ,x = xx[model.idx,] ,ntree=ntree ,importance=TRUE ,proximity=FALSE)

   y[na.idx] = predict( model.rf, X[na.idx,], type = "response")
   if(class.y == "integer"){ y[na.idx] = round(y[na.idx]); class(y) = class.y }
   
   Y[,vtf] = y

   cat("Model dla ", vtf ," zbudowano na ",  n, " obserwacjach. \n")
}

datfram[,vars.to.fill] = Y

datfram

}