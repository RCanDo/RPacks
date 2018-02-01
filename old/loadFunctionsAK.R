########################################################################################################################•°
## LOADER OF ALL FunctionsAK to the separate environments
########################################################################################################################•°
#rm(list=ls())     ## czyszczenie przestrzeni roboczej
########################################################################################################################•°
#!   ################################################################################################•°
########################################################################################################################•°

## LOADER FUNCTION #####################################################################################################•°
source_to_env <- function(files,path,env){
   if(any(search()==env)) detach(env, unload=TRUE ,character.only=TRUE)
   env <- attach(NULL, name = env)
   sapply(files, FUN = function(x) sys.source(paste(path,x,sep='/'),env))
   print(ls(env))
}

## FOLDERS #############################################################################################################•°
#ROOT = "D:/ROBOCZY/PK_Analitycy" ;
ROOT = "//10.1.1.5/PK_Analitycy" ;
PATHfunctions = list()
PATHfunctions$AK = paste(ROOT,"R/FunctionsAK",sep="/")

packages <- c( "DM"
             , "StatMath"
             , "Graphics"
             , "Variable"
             , "Condapply"
             , "SumMod"
             , "Curves"
             , "RefineAssignment"
             , "Threshold"
             , "Split"
             #, ""
             )

for(nam in packages){
   PATHfunctions[nam] <- paste(PATHfunctions$AK,nam,sep="/")
}
PATHfunctions

## PACKAGES ############################################################################################################•°

files = list()
sig = "_AK"

###########################################################•°
cat(pack <- packages[1],"\n")  # = "DM"
# cat(list.files(PATHfunctions[[pack]]),sep="\"\n,\"")
files[[pack]] = c("addid.R"
,"all.factors.r"
,"all.numeric.r"
,"all.of.r"
,"capply.R"
,"colnames.by.R"
,"fill.from.left.R"
,"last.R"
,"lengthen.R"
,"m.triangles.r"
,"merge.ordered.r"
,"merge_factors.r"
,"monetary.r"
,"nulls.by.id.r"
,"nulls.table.r"
,"plus.epsilon.r"
,"random.tables.r"
,"unravel.r"
,"update.binary.df.R"
,"update.df.R")
source_to_env( files = files[[pack]] , path = PATHfunctions[[pack]] , env = paste(pack,sig,sep="") )

###########################################################•°
cat(pack <- packages[2],"\n")  # = "StatMath"
# cat(list.files(PATHfunctions[[pack]]),sep="\"\n,\"")
files[[pack]] = c("chisq.multi.test.r"
,"classic.r"
,"earth.aux.R"
,"nulls.imput.r"
,"ranges.r"
,"transformations_a.R")
source_to_env( files = files[[pack]] , path = PATHfunctions[[pack]] , env = paste(pack,sig,sep="") )

###########################################################•°
cat(pack <- packages[3],"\n")  # = "Graphics"
# cat(list.files(PATHfunctions[[pack]]),sep="\"\n,\"")
files[[pack]] = c("barplot.confint.R"
,"coldef.r"
,"graphical.parameters.R"
,"parGraphical.R"
,"plot.means.r"
,"univariate.models.plots.r")
source_to_env( files = files[[pack]] , path = PATHfunctions[[pack]] , env = paste(pack,sig,sep="") )

###########################################################•°
cat(pack <- packages[4],"\n")  # = "Variable"
# cat(list.files(PATHfunctions[[pack]]),sep="\"\n,\"")
files[[pack]] = c("extract.variable.r"
,"plot.variable.r")
source_to_env( files = files[[pack]] , path = PATHfunctions[[pack]] , env = paste(pack,sig,sep="") )

###########################################################•°
cat(pack <- packages[5],"\n")  # = "Condapply"
# cat(list.files(PATHfunctions[[pack]]),sep="\"\n,\"")
files[[pack]] = c("condapply.R"
,"condlist.print.R"
,"condlist.R"
,"create.logging.df.R"
,"transform.by.trans.list.r"
,"update.logging.df.R")
source_to_env( files = files[[pack]] , path = PATHfunctions[[pack]] , env = paste(pack,sig,sep="") )

###########################################################•°
cat(pack <- packages[6],"\n")  # = "SumMod"
# cat(list.files(PATHfunctions[[pack]]),sep="\"\n,\"")
files[[pack]] = c("moditem.create.R"
,"summary.ak.gam.r"
,"summary.gam.ak.R")
source_to_env( files = files[[pack]] , path = PATHfunctions[[pack]] , env = paste(pack,sig,sep="") )

###########################################################•°
cat(pack <- packages[7],"\n")  # = "Curves"
# cat(list.files(PATHfunctions[[pack]]),sep="\"\n,\"")
files[[pack]] = c("curves_and_prediction.r"
,"curves_by_groups.R"
,"curves_reweight.r"
,"plot.curves_and_prediction.r"
,"plot.curves_by_groups.R"
,"plot.smooth_gam.R"
,"smooth_gam.curves_by_groups.R"
,"smooth_gam.r"
,"summary.curves_by_groups.R")
source_to_env( files = files[[pack]] , path = PATHfunctions[[pack]] , env = paste(pack,sig,sep="") )

###########################################################•°
cat(pack <- packages[8],"\n")  # = "RefineAssignment"
# cat(list.files(PATHfunctions[[pack]]),sep="\"\n,\"")
files[[pack]] = c(#"make_assignment.R"
#,"o2p0-sub1.R"
#,"o2p0.R"
 "o2p1_sub1.R"
,"o2p1_sub2.R"
#,"o2p1.R"
,"refine.assignment.R")
source_to_env( files = files[[pack]] , path = PATHfunctions[[pack]] , env = paste(pack,sig,sep="") )

###########################################################•°
cat(pack <- packages[9],"\n")  # = "Threshold"
# cat(list.files(PATHfunctions[[pack]]),sep="\"\n,\"")
files[[pack]] = c("set.threshold.r"
,"thresh.aggr.table.r"
,"threshold.tables.r")
source_to_env( files = files[[pack]] , path = PATHfunctions[[pack]] , env = paste(pack,sig,sep="") )

###########################################################•°
cat(pack <- packages[10],"\n")  # = "Split"
# cat(list.files(PATHfunctions[[pack]]),sep="\"\n,\"")
files[[pack]] = c("plot.split.r"
,"predict.df.list.r"
,"split.df.by.nas.r"
,"split.plot.r"
,"split.rand.nrows.R"
,"split.rand.portion.r"
,"split.stats.r")
source_to_env( files = files[[pack]] , path = PATHfunctions[[pack]] , env = paste(pack,sig,sep="") )

###########################################################•°
#cat(pack <- packages[11],"\n")  # = ""
#cat(list.files(PATHfunctions[[pack]]),sep="\"\n,\"")
#files[[pack]] = c("")
#source_to_env( files = files[[pack]] , path = PATHfunctions[[pack]] , env = paste(pack,sig,sep="") )

########################################################################################################################•°
search()