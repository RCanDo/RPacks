########################################################################################################################���
## LOADER OF ALL PacksAK to the separate environments
########################################################################################################################���
#rm(list=ls())     ## czyszczenie przestrzeni roboczej
########################################################################################################################���
#!   ################################################################################################���
########################################################################################################################���

## LOADER FUNCTION #####################################################################################################���
source_to_env <- function(files,path,env){
   if(any(search()==env)) detach(env, unload=TRUE, character.only=TRUE)
   env <- attach(NULL, name = env)
   sapply( files, FUN = function(x) sys.source(paste(path,x,sep='/'),env) )
   print(ls(env))
}
########################################################################################################################���

########################################################################################################################���
loadPacksAK <- local({

## the main suffix for the names of packs (proto-pack_names)
sig = "AK"
sep = "_"

## FOLDERS #############################################################################################################���
PATH$functions = list()
PATH$functions[[sig]] = PATH$packs  ## from .GlobalEnv   �� demanded main packs directory

if(!exists("pack_names",where=.GlobalEnv)){
pack_names <- c( "ScoringOld"
                , "DM"
                , "StatMath"
                , "Graphics"
                , "Variable"
                , "Condapply"
                , "SumMod"
                , "EfficiencyCurves"
                , "RefineAssignment"
                , "Threshold"
                , "Split"
                #, ""
               ) }

for(nam in pack_names){
   PATH$functions[[nam]] <- paste(PATH$functions[[sig]],nam,sep="/")  ## specific packs subdirectories
}
PATH$functions

## PACKAGES ############################################################################################################���

files = list()


###########################################################���
pack <- "ScoringOld"
if(pack %in% pack_names){
cat(pack,"\n")
# cat(list.files(PATH$functions[[pack]]),sep="\"\n,\"")
files[[pack]] = c("graf.r","pomocnicze_a.r","skupienia.r")
source_to_env( files = files[[pack]] , path = PATH$functions[[pack]] , env = paste(pack,sig,sep=sep) )
}

###########################################################���
pack <- "DM"
if(pack %in% pack_names){
cat(pack,"\n")
# cat(list.files(PATH$functions[[pack]]),sep="\"\n,\"")
files[[pack]] = c(
"addid.R"
,"all.factors.r"
,"all.numeric.r"
,"all.of.r"
,"allnames.r"
,"arguments.R"
,"attributes.R"
,"capply.R"
,"colnames.by.R"
   ,"compact.R"
,"conditions.R"
,"efficiency.R"
,"fill.from.left.R"
,"head.array.R"
,"head.methods.R"
,"indent.R"
,"indentr.R"
,"infix.R"
,"last.R"
,"lengthen.R"
,"m.triangles.r"
,"merge.ordered.r"
,"merge_factors.r"
   ,"models_list.R"   ## originaly in SumMod where it should be (now cannot because it is not seen there by print() called from indentr())
,"monetary.r"
,"methods.tm.R"
,"nulls.by.id.r"
,"nulls.table.r"
,"options.R"
,"plus.epsilon.r"
,"prepapp.R"
   ,"print.condlist.R"
,"print.tm.R"
,"random.tables.r"
,"select_levels.R"
,"t.array.R"
,"tm.R"
,"tm.attributes.R"
,"unravel.r"
,"update.binary.df.R"
,"update.df.R"
,"whileif.R"
,"Write.R"
)
source_to_env( files = files[[pack]] , path = PATH$functions[[pack]] , env = paste(pack,sig,sep=sep) )
}

###########################################################���
pack <- "StatMath"
if(pack %in% pack_names){
cat(pack,"\n")
# cat(list.files(PATH$functions[[pack]]),sep="\"\n,\"")
files[[pack]] = c("chisq.multi.test.r"
,"classic.r"
,"earth.aux.R"
,"nulls.imput.r"
,"ranges.r"
,"transformations_a.R")
source_to_env( files = files[[pack]] , path = PATH$functions[[pack]] , env = paste(pack,sig,sep=sep) )
}

###########################################################���
pack <- "Graphics"
if(pack %in% pack_names){
cat(pack,"\n")
# cat(list.files(PATH$functions[[pack]]),sep="\"\n,\"")
files[[pack]] = c("barplot.confint.R"
,"parGraphical.R"
,"plot.means.r"
,"univariate.models.plots.r")
source_to_env( files = files[[pack]] , path = PATH$functions[[pack]] , env = paste(pack,sig,sep=sep) )
}

###########################################################���
pack <- "Variable"
if(pack %in% pack_names){
cat(pack,"\n")
# cat(list.files(PATH$functions[[pack]]),sep="\"\n,\"")
files[[pack]] = c("extract.variable.r"
,"plot.variable.r")
source_to_env( files = files[[pack]] , path = PATH$functions[[pack]] , env = paste(pack,sig,sep=sep) )
}

###########################################################���
pack <- "Condapply"
if(pack %in% pack_names){
cat(pack,"\n")
# cat(list.files(PATH$functions[[pack]]),sep="\"\n,\"")
files[[pack]] = c("condapply.R"
,"condlist.R"
,"create.logging.df.R"
,"transform.by.trans.list.r"
,"update.logging.df.R")
source_to_env( files = files[[pack]] , path = PATH$functions[[pack]] , env = paste(pack,sig,sep=sep) )
}

###########################################################���
pack <- "SumMod"
if(pack %in% pack_names){
cat(pack,"\n")
# cat(list.files(PATH$functions[[pack]]),sep="\"\n,\"")
files[[pack]] = c(
##"models_list.R"  ## moved to {DM}
"signif_stars.R"
,"panova.R"
,"panova.gam.R"
,"summary.panova.gam.R"
,"misc.gam.panova.R"
,"moditem.create.R"
,"summary.gam.ak.R")
source_to_env( files = files[[pack]] , path = PATH$functions[[pack]] , env = paste(pack,sig,sep=sep) )
}

###########################################################���
pack <- "EfficiencyCurves"
if(pack %in% pack_names){
cat(pack,"\n")
# cat(list.files(PATH$functions[[pack]]),sep="\"\n,\"")
files[[pack]] = c(
 #"curves_and_prediction.R" ,
 "curves_by_groups.R"
,"curves_reweight.R"
,"plot.curves_and_prediction.R"
,"plot.curves_by_groups.R"
,"plot.smooth_gam.R"
,"prediction.R"
,"prediction.curves_by_groups.R"
,"prediction.smooth_gam.R"
,"predict.R"
,"regroup.R"
,"sim_ref.R"
,"smooth_gam.R"
,"smooth_gam.curves_by_groups.R"
,"smooth_gam.curves_and_prediction.R"
,"summary.curves_and_prediction.R"
,"summary.curves_by_groups.R"
,"summary.smooth_gam.R"
,"tlm.R"
,"tlm_list.R"
,"tlm.curves_by_groups.R"
)
library(gam)
source_to_env( files = files[[pack]] , path = PATH$functions[[pack]] , env = paste(pack,sig,sep=sep) )
}

###########################################################���
pack <- "RefineAssignment"
if(pack %in% pack_names){
cat(pack,"\n")
# cat(list.files(PATH$functions[[pack]]),sep="\"\n,\"")
files[[pack]] = c(#"make_assignment.R"
#,"o2p0-sub1.R"
#,"o2p0.R"
 "o2p1_sub1.R"
,"o2p1_sub2.R"
#,"o2p1.R"
,"refine.assignment.R")
source_to_env( files = files[[pack]] , path = PATH$functions[[pack]] , env = paste(pack,sig,sep=sep) )
}

###########################################################���
pack <- "Threshold"
if(pack %in% pack_names){
cat(pack,"\n")
# cat(list.files(PATH$functions[[pack]]),sep="\"\n,\"")
files[[pack]] = c("set.threshold.r"
,"thresh.aggr.table.r"
,"threshold.tables.r")
source_to_env( files = files[[pack]] , path = PATH$functions[[pack]] , env = paste(pack,sig,sep=sep) )
}

###########################################################���
pack <- "Split"
if(pack %in% pack_names){
cat(pack,"\n")
# cat(list.files(PATH$functions[[pack]]),sep="\"\n,\"")
files[[pack]] = c("plot.split.r"
,"predict.df.list.r"
,"split.df.by.nas.r"
,"split.plot.r"
,"split.rand.nrows.R"
,"split.rand.portion.r"
,"split.stats.r")
source_to_env( files = files[[pack]] , path = PATH$functions[[pack]] , env = paste(pack,sig,sep=sep) )
}

###########################################################���
#cat(pack <- pack_names[11],"\n")  # = ""
#cat(list.files(PATH$functions[[pack]]),sep="\"\n,\"")
#files[[pack]] = c("")
#source_to_env( files = files[[pack]] , path = PATH$functions[[pack]] , env = paste(pack,sig,sep="") )

########################################################################################################################���
cat("\n")
print(search())

sapply(paste(names(files),sig,sep=sep),function(x)rm(list=intersect(ls(x),ls(pos=1)),pos=1))

function( pack ){
   source_to_env( files = files[[pack]] , path = PATH$functions[[pack]] , env = paste(pack,sig,sep=sep) )
}

})
########################################################################################################################���


#files = list(
#Variable = c("extract.variable.r"
#,"plot.variable.r")
#,Threshold = c("set.threshold.r"
#,"thresh.aggr.table.r"
#,"threshold.tables.r")
#,Split = c("plot.split.r"
#,"predict.df.list.r"
#,"split.df.by.nas.r"
#,"split.plot.r"
#,"split.rand.nrows.R"
#,"split.rand.portion.r"
#,"split.stats.r"))
#
#rm(list=intersect(ls(paste0(names(files),"_AK")),ls(pos=1)))
#
#rm(files)
