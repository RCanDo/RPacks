########################################################################################################################•°
moditem.gam = function(
  model_name    ## model
#, mod.name.new = mod.name
#, pars = list( fun = character(0) , args = list() )  ## ready to use in   do.call(fun,args)
#, regs = list( x = character(0) , s = character(0) )
#, data_name = character(0)  ## nazwa ramki danych na których zbudowano model -- ta RD powinna byæ w œrodowisku
#, formula = NA
, qpred  = NULL    ## prognoza na referencji ale przekszta³cona np. pomno¿ona przez saldo
, qres   = NULL   ## np. Y - pred
, breaks = NULL  ## podzia³ wyników prognozy (zazwyczaj scory na przedzia³y ~= klasy
, dataref = NULL
){
########################################################################################################################•°
##
########################################################################################################################•°
##   model_name = "model.glm_logit_binary_1"

model = get(model_name)

call.list = as.list(model$call)
call.list$formula = model$formula

regs = as.character(model$formula)
regs[[2]]
regs.s = strsplit( regs[[3]] , split=" + " , fixed=TRUE )[[1]]
regs.x = gsub(')$','',gsub('^s\\(','',regs.s ))

###################################################################################################•°
data_name = as.character(call.list$data)
if(is.null(dataref)){
   dataref = get(data_name)
}

predictions = predict(model,type="response")



###################################################################################################•°
if(!is.null(qpred)){
   if(length(qpred)==1 && is.character(qpred)){
      pred.covar = dataref[,qpred]
   }else{
      pred.covar = qpred
   }
   qpred = pred.covar*predictions
}else{
   qpred = predictions
}

###################################################################################################•°

if(!is.null(qres)){
   if(length(qres)==1 && is.character(qres)){
      res.covar = dataref[,qres]
   }else{
      res.covar = qres
   }
   qres = res.covar-qpred
}else{
   qres = residuals(model,type="response")
}

###################################################################################################•°
moditem = list(  mod = model     ## model
               , pars = list( FUN = call.list[[1]] , args = call.list[-1] )  ## ready to use in   do.call(FUN,args)
               , regs = list( x = regs.x , s = regs.s )
               , y_name = regs[[2]]
               , data_name = data_name  ## nazwa ramki danych na których zbudowano model -- ta RD powinna byæ w œrodowisku
               , qpred     = qpred      ## prognoza na referencji ale przekszta³cona np. pomno¿ona przez saldo
               , qres      = qres       ## np. saldo - pred*saldo = (1 - pred)*saldo
               , breaks    = breaks     ## podzia³ wyników prognozy (zazwyczaj scory na przedzia³y ~= klasy
              )

moditem

}



   do.call(mean,list(c(1:10,NA),na.rm=T))