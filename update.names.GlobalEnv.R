########################################################################################################################•°
## FUNCTIONS HERE
##  update.names.GlobalEnv()
##
## DEPENDENCIES
##
##
########################################################################################################################•°
update.names.GlobalEnv = function(datfram){  ##
########################################################################################################################•°

   names.datfram = colnames(datfram)
   ref.names.vec <<- ls(pattern='^names\\.',pos=.GlobalEnv)  ## to s¹ grupy zmiennych z etapu obróbki danych  FILEdataref.Rdata;
                        ## zmienne w nich zarejestrowane mog¹ ju¿ nie wystêpowaæ w Data.ref

   ref.names.list <<- mget(ref.names.vec,envir=.GlobalEnv)

   ref_not_mod.names.list <<- lapply(ref.names.list,function(x)setdiff(x,names.datfram))  ## zmienne brakuj¹ce (po etapie 1)
   mod.names.list <<- lapply(ref.names.list,function(x)intersect(x,names.datfram))  ## to co zostalo

   for(k in ref.names.vec){assign(k,mod.names.list[[k]],pos=.GlobalEnv)}
}
########################################################################################################################•°