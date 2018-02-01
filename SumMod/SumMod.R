########################################################################################################################—•°
*## This is tutorial for the package "SumMod" by ReX
########################################################################################################################—•°
rm(list=ls())

########################################################################################################################—•°
## 0. SetUp  ###########################################################################################################—•°

## A. KATALOGI #########################################################################################################—•°
ROOT = "D:/ROBOCZY/PK_Analitycy" ;
#ROOT = "//10.1.1.5/PK_Analitycy"
PATH = list() ;

## B. PACKS ############################################################################################################—•°
## where all the interesting packs lie
PACKS = "PacksAK"
PATH$packs = paste(ROOT,"R",PACKS,sep="/")

## B.1. Loading ###################################################################################—•°
## v1. If you wish all packs
source(paste(PATH$packs,paste0(PACKS,".R"),sep="/"))

   ## v2. If you wish only one (or few) pack
   pack_names = c("SumMod")
   ## remove it if you wish again all packs
   #>  rm(pack_names)

## B.2. ReLoading #################################################################################—•°
## now you may re-load any of the packs , e.g.
pack = "SumMod"
loadPacksAK(pack)

## C. Working Directory ################################################################################################—•°
## set working directory to the directory of this pack
PATH$wd = paste(PATH$packs,pack,sep="/")
setwd(PATH$wd)

###################################################################################################—•°
ls()
search()
########################################################################################################################—•°



########################################################################################################################—•°
## Example 1  ##########################################################################################################—•°

sumpan = summary( panova_k
   ## coeffs
#      , FUN.c = list( "coeffs" = function(x){c("mean" = mean(x))}
#                  , "coeffs" = function(x){signs.table(x)}
#                  , "Pr(>F)" = function(x){c("mean" = mean(x))}
#                  , "Pr(>F)" = function(x){c("sstars" = signif_stars(mean(x)))}
#                  , "sstars" = function(x){sstars.table(x) }
#                  )
      , add_colnames.c = c(TRUE,FALSE,TRUE,TRUE,FALSE)
    ## measures
#      , FUN.m = list( "MSE"  = function(x){c("min"=min(x),"mean"=mean(x),"max"=max(x))}
#                  , "AIC"  = function(x){c("min"=min(x),"mean"=mean(x),"max"=max(x))}
#                  , "Gini" = function(x){c("min"=min(x),"mean"=mean(x),"max"=max(x))}
#                  , "AUC"  = function(x){c("min"=min(x),"mean"=mean(x),"max"=max(x))}
#                  , "p.mww" = function(x){c("mean"=mean(x))}
#                  )
      , add_colnames.m = TRUE
      , append_to = "summary_panova_k" #, row = k
      , print=TRUE
)

sumpan


########################################################################################################################—•°
## Example 2  ##########################################################################################################—•°

summary.gam.panova.agg(sum.df,FUN = list(coeffs=function(x){mean(x)},coeffs=function(x){sum(x)}))
summary.gam.panova.agg(sum.df,FUN = list(coeffs=function(x){mean(x)},coeffs=function(x){sum(x)}
   ,sstars=function(x){as.vector(table(x))}))

#######################################
   data(airquality)
   dim(airquality)
   mod.gam = gam(Ozone^(1/3) ~ lo(Solar.R) + lo(Wind, Temp), data=airquality, na=na.gam.replace)
   summary(mod.gam)$parametric.anova
   names(airquality)
   cat(names(airquality),sep=") + s(")

rm(sum.gam_k)
for(k in 1:20){
airquality_k = split.rand.portion(airquality,split=TRUE,portion=.3)
mod.gam_k = gam(Ozone ~ s(Solar.R) + s(Wind) + s(Temp) + s(Month) + s(Day),data=airquality_k,na=na.gam.replace)
summary.gam.panova(mod.gam_k,append.to="sum.gam_k",print=FALSE)
}
#sum.gam_k

summary.gam.panova.agg(sum.gam_k,FUN = list(coeffs=function(x){mean(x)},coeffs=function(x){signs.table(x)}
   ,"Pr(>F)"=function(x){mean(x)} ,"Pr(>F)"=function(x){signif_stars(mean(x))}
   ,sstars=function(x){sstars.table(x)}))


panova.df = sum.gam_k

########################################################################################################################—•°


   ########################################################################################################################—•°
   ?gam
   data(kyphosis)
   dim(kyphosis)
   names(kyphosis)
   mod.gam = gam(Kyphosis ~ s(Age,4) + Number, family = binomial, data=kyphosis,trace=TRUE)
   summary(mod.gam)$parametric.anova

   #######################################
   data(airquality)
   dim(airquality)
   mod.gam = gam(Ozone^(1/3) ~ lo(Solar.R) + lo(Wind, Temp), data=airquality, na=na.gam.replace)
   summary(mod.gam)$parametric.anova
   names(airquality)
   cat(names(airquality),sep=") + s(")


   rm(sum.gam_k)
   for(k in 1:7){
   airquality_k = split.rand.portion(airquality,split=TRUE,portion=.2)
   mod.gam_k = gam(Ozone ~ s(Solar.R) + s(Wind) + s(Temp) + s(Month) + s(Day),data=airquality_k,na=na.gam.replace)
   summary.gam.panova(mod.gam_k,append.to="sum.gam_k")
   }
   sum.gam_k


   #######################################
   gam(Kyphosis ~ poly(Age,2) + s(Start), data=kyphosis, family=binomial, subset=Number>2)
   data(gam.data)
   gam.object <- gam(y ~ s(x,6) + z,data=gam.data)
   summary(gam.object)
   plot(gam.object,se=TRUE)
   data(gam.newdata)
   predict(gam.object,type="terms",newdata=gam.newdata)
   ########################################################################################################################—•°

########################################################################################################################—•°






########################################################################################################################—•°
#! KONIEC KONIEC KONIEC KONIEC KONIEC KONIEC KONIEC KONIEC KONIEC KONIEC KONIEC KONIEC KONIEC KONIEC KONIEC KONIEC #####—•°
########################################################################################################################—•°
