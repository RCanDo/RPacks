########################################################################################################################•
## FUNCTIONS HERE
##  logx()
##
## DEPENDENCIES
##  [plik].r
##    [funkcja]() , ...
##  [plik].r
##    ...
##
########################################################################################################################•
get.used.pred.terms = function(mod){
   names(which(apply(mod$dirs[mod$selected.terms,,drop=FALSE],2,function(x){any(x!=0)})))
}
########################################################################################################################•
## Gets names of prediction terms of mars model.
########################################################################################################################•

########################################################################################################################•
models.table.update = function(mod){
########################################################################################################################•
## Builds table of mars mdeles. Table has name 'model.table' and resids in .GlobalEnv.
########################################################################################################################•
   if(is.character(mod)){
      name.mod = mod
      mod = get(mod,pos=".GlobalEnv")
   }else{
      name.mod = deparse(substitute(mod))
   }
   ##
   names.pars = c("rss","gcv","rsq","grsq")##names(mod)[c(8,6,7,9)]
   mtabrow = as.data.frame(rbind(c(unlist(mod[names.pars]), n.pred = length(get.used.pred.terms(mod)),n.terms=length(mod$selected.terms))))
   rownames(mtabrow) = name.mod
   if(!exists("models.table")){
      #rnams = character(0)
      assign("models.table",mtabrow,pos=".GlobalEnv")
   }else{
      #rnams = rownames(mod.tab)
      models.table <<- rbind(models.table,mtabrow)
   }
   ##
   ##rownames(mod.tab) <<- c(rnams,name.mod)
   models.table
}
########################################################################################################################•