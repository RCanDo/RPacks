## ---------------------------------------------------------------------------------------------------------------------—•°
*## This is tutorial for the package "EfficiencyCurves" by RCanDo
## ---------------------------------------------------------------------------------------------------------------------—•°
rm(list=ls())

## ---------------------------------------------------------------------------------------------------------------------—•°
## 0. Do poprawnego dzialania TinnR (powinno ladowac się samo - trzeba naprawic jakies .ini)
#  .trPaths <- paste(paste(Sys.getenv('APPDATA'), '\\Tinn-R\\tmp\\', sep=''),
#            c('', 'search.txt', 'objects.txt', 'file.r', 'selection.r', 'block.r', 'lines.r','reformat-input.r','reformat-output.r'), sep='')
##  or
#  .trPaths <- paste( 'C:\\Users\\Arkadiusz\\AppData\\Local\\Temp\\Tinn-R\\',
#           c('', 'search.txt', 'objects.txt', 'file.r', 'selection.r', 'block.r', 'lines.r','reformat-input.r','reformat-output.r'), sep='')
## ---------------------------------------------------------------------------------------------------------------------—•°


## ---------------------------------------------------------------------------------------------------------------------—•°
## 1. SetUp  ###########################################################################################################—•°

## A. FOLDERS ##########################################################################################################—•°
ROOT = "D:/ROBOCZY/" ;
#ROOT = "//10.1.1.5/PK_Analitycy"
PATH = list() ;
   #
## B. PACKS ############################################################################################################—•°
## where all the interesting pack(age)s lie
PACKS = "PacksAK"  ## name of a folder and loader file in it
PATH$packs = paste(ROOT,"R",PACKS,sep="/")
   #
## C. Loading #####################################################################################—•°
## v1. If you wish all packs
source(paste(PATH$packs,paste0(PACKS,".R"),sep="/"))
   #
   ## v2. If you wish only one (or few) pack
   # pack_names = c("EfficiencyCurves")
   ## remove it if you wish again all packs
   ##  rm(pack_names)
   #
## D. Working Directory ################################################################################################—•°
## set working directory to the directory of this pack
pack = "EfficiencyCurves"
PATH$wd = paste(PATH$packs,pack,sep="/")
setwd(PATH$wd)
   #
## E. ReLoading ###################################################################################—•°
## now you may re-load any of the packs , e.g.
# pack = "..."
# loadPacksAK(pack)
   #
## ------------------------------------------------------------------------------------------------—•°
load(".RData")
ls()
search()
## ---------------------------------------------------------------------------------------------------------------------—•°



## ---------------------------------------------------------------------------------------------------------------------—•°
## Example 1  ##########################################################################################################—•°

## ------------------------------------------------------------------------------------------------—•°
## a. Data
## val
( val.df = sim_val( members = 20 , groups = letters[1:8] ) )
## ref
#( ref.df = sim_ref( members = 10 , groups = letters[2:10] , horizon = 4 , yield_par = .1 ) )
( ref.df = sim_ref( members = 100 , groups = letters[3:10]
                  , horizon = 5 + sample(4) #60     ## recycled to get length = members
                  , yield_par = c(.02,.05,.1,.15)    ## recycled to get length = groups
                  #, exp_down = FALSE
                  , amount = NULL
                  ) )
 head(ref.df)

    ## to miss some levels from data  (run after 2.)
    val.df =subset(val.df,group!="h")
    ref.df =subset(ref.df,group!="i")

## ------------------------------------------------------------------------------------------------—•°
## curves_by_groups()

( cbg = curves_by_groups( ref.df
                        , horizon = "horizon"
                       ) )

names(cbg)

plot(cbg)
plot(cbg,add="counts",what="normal")
plot(cbg,add="amounts",what="normal")

summary(cbg)

## ----------------—•°
## print() method for "curves_by_groups"
print(cbg,as_rows=TRUE)
print(cbg,round=2)
print(cbg,round=2,round.attr=FALSE)   ## none of the objects has round.attr so nothing changes
## see round.attr() in DM pack

## ----------------—•°
## elements of "curves_by_groups"
names(cbg)
class(cbg$call)                        ## call
class(cbg$arguments)                   ## arguments
class(cbg$counts)                      ## tm   matrix
class(cbg$amounts)                     ## tm   matrix
class(cbg$yields)                      ## tm   matrix
class(cbg$groups)                      ## ordered factor
class(cbg$curves)                      ## tm   matrix
class(cbg$net_curve)                   ## tm   numeric

class(summary(cbg))                    ## summary.curves_by_groups   matrix

cbg$amounts                            ## this is because it is of class "tm"
class(cbg$amounts)    ## "tm" !!!  see {DM}  tm.R
cbg$amounts[]         ## notice attr(,"margins")
   attributes(cbg$amounts)             ## 'margins' argument tells which margin should be calculated and displayed
   margins(cbg$amounts)                ##
unclass(cbg$amounts)                   ## but it works only if object is of class "tm"
as.table(cbg$amounts)

cbg$yields
class(cbg$yields)
cbg$yields[]         ## notice attr(,"margins")

cbg$curves
class(cbg$curves)
cbg$curves[]         ## notice attr(,"margins")


## ----------------—•°

(scbg <- summary(cbg))
(scbg <- summary(cbg,yields=TRUE))
(scbg <- summary(cbg,yields=2))
(scbg <- summary(cbg,yields=FALSE))   ## default is  yields=FALSE  what gives efficiencies

scbg[]
scbg
print(scbg,round=2)      ## "round.attr" prevails over 'round' option;
                         ## this is the only such attribute (and this is intentional)
print(scbg,round=2,round.attr=FALSE)   ## now it's turned off
print(scbg,head=2)       ## other attributes are less important then option to function
head(scbg,2)             ## there is a head() method for  "summary.curves_by_groups"
print(scbg,as_row=FALSE)

(scbg <- summary(cbg,as_rows=FALSE))
scbg[]   ## the same matrix but attributes differ
scbg
(scbg <- summary(cbg,round=1))
scbg[]
scbg
(scbg <- summary(cbg,head=2))
scbg[]
scbg
summary(cbg,4)
summary(cbg,4,2)
summary(cbg,4,5)

## ----------------—•°
## head() method for "summary.curves_by_groups"
head(scbg)
head(scbg,5)
head(scbg,5)[]

## We have seen that parameters  as_rows, round, head  are only printing parameters.
## Use apply=TRUE for this parameters take effect on the object itself (not only its printing).
(scbg <- summary(cbg,head=5,round=2,as_rows=TRUE,apply=TRUE))
scbg[]   ## all printing parameters applied thus  object  is exactly the same as it is printed
scbg

head(scbg,7)  ## there are only 5 times in scbg
head(scbg,3)
(scbg <- summary(cbg,round=2,as_rows=TRUE,apply=TRUE))
scbg[]   ## all printing parameters applied thus  object  is exactly the same as it is printed
scbg

Write(scbg)

## ----------------—•°
## predict() and prediction() methods
predict(cbg)                               ## by default it returnes "net_curve"
predict(cbg,type="curves")                 ## but you may change it
predict(cbg,type=c("curves","net_curve"))  ## you may get any subset of elements of 'cbg';  then it is a list!!!
predict(cbg,type=c("all"))                 ## the whole 'cbg' but with additional class;
class(predict(cbg,type=c("all")))

## when you add a newdata then predict() returns efficiency for new data predicted on the base of 'cbg'
predict(cbg,newdata=val.df)
class(predict(cbg,newdata=val.df))
class(predict(cbg,newdata=val.df,type="all"))

## all calcultaions are perforemd via prediction() method for "curves_by_groups" ...
prediction(cbg,newdata=val.df)     ## new data are necessary !!!
## ... which returnes object of class "curves_and_prediction".

## more on predict() and prediction() later...


## --------------------------------------------------------—•°
## plotting variants
plot(cbg)
plot(cbg,width=8)     ## height=width
plot(cbg,width=6)
plot(cbg,width=4)
plot(cbg,width=5)
plot(cbg,width=6)
plot(cbg,width=7)
plot(cbg,width=8)
plot(cbg,width=9)
plot(cbg,width=10)
plot(cbg,width=12)
plot(cbg,width=15)
plot(cbg,width=20)
plot(cbg,width=15,height=10)
plot(cbg,width=18,height=10)
plot(cbg,width=10,height=20)
plot(cbg,height=20)  ## width is still default 8 !
##! WARNING: By default plot (window) is a square, height = width,
## BUT it is set up somwhere (where???) that
##   height cannot exceed 10.5 inches (approx.)
##   width  cannot exceed 18
## (on the 24' FHD screen at least)
##! and if some of these parameters exceeds their limit then the plot is rescaled with RATIO MAINTAINED!!!
## This makes things very strange !!!

plot(cbg,width=6,height=10)
plot(cbg,width=6,height=8)
plot(cbg,,,8,12)
plot(cbg,,,8,10)
plot(cbg,,,10,15)
plot(cbg,,"normal")
#
plot(cbg,legend_pos="bottomright")
plot(cbg,legend_pos=c("bottomleft","bottomright"))
plot(cbg,legend_pos=NULL)
plot(cbg,legend_pos=FALSE)
plot(cbg,legend_pos=c(FALSE,TRUE))
plot(cbg,legend_pos=c("","bottomright"))

## --------------------------------------------------------—•°
## predict() and prediction()
   source(paste(PATH$packs,paste0(PACKS,".R"),sep="/"))

## ------------------------------------—•°
## prediction()

prediction(cbg)      ## ERROR ;
(pcbg<-prediction(cbg,newdata=val.df))  ## newdata are necessary
class(pcbg)
print(pcbg,messages=F)

   getOption("messages")
   options(messages=FALSE)

pcbg

print(pcbg,round=2)    ## by default round=3
print(pcbg,round=2,round.attr=FALSE)   ##  val_yields_groups  has round.attr = 0
                                       ## and "round.attr" prevails over 'round' option; this is the only such attribute;
                                       ## it must be turned off for 'round' option take effect
print(pcbg,round=6,round.attr=FALSE)   ##  val_yields_groups  has round.attr = 0
            ## this shows that for some data (like "val_yields_groups") 'round' option should rather not take effect
            ## so this is the reason the "round.attr" prevails over 'round'.
            ## it is reasonable to see more digits of some efficiencies while it's not for yields.

   options(messages=T)

(spcbg <- summary(pcbg))
(spcbg <- summary(pcbg,yields=T))
(spcbg <- summary(pcbg,yields=1))    ## yields rounded to 1 dp
(spcbg <- summary(pcbg,yields=5))    ## yields rounded to 5 dp but  round=3  hence only 3 dp are visible
(spcbg <- summary(pcbg,yields=5,round=7))

(spcbg <- summary(pcbg,yields=T,round_yields=1)) ##
(spcbg <- summary(pcbg,yields=2,round_yields=1)) ## numeric value of yields overwrites value of round_yields

(spcbg <- summary(pcbg,yields=F))    ## yields=FALSE is default


spcbg
spcbg[]
messages(spcbg)
print(spcbg,round=4)                        ## doesn't work because round.attr(x) is not NULL and it prevails!
print(spcbg,round=4,round.attr=F)           ## round.attr must be switched off
print(spcbg,as_rows=F)
head(spcbg)
head(spcbg,0)

Write(spcbg)

spcbg <- summary(pcbg,round=3,as_rows=T,apply=T)  ## apply all printing parameters i.e. round and as_rows
spcbg                                             ## ...
spcbg[]                                           ## what means that summary(pcbg) was calculated and written with them

Write(spcbg)

## you may pass 1 number as newdata and then
## amounts for valuated is 1 for all groups from reference
## what means equal weights to all curves
prediction(cbg,1)
print(prediction(cbg,1),round=3)            ##  val_yields_groups  has round.attr = 0
print(prediction(cbg,1),round.attr=FALSE)

## vector of numbers is replicated to the length = number of groups in reference
prediction(cbg,c(1,2))

prediction(cbg,c(a=1e3,b=2e3,c=3e3,d=2e3,f=1e3,g=2e3,h=3e3,i=700))
## Notice that weights (so curves too) are taken only for groups present in reference,
## and only these are rescaled to sum up to one.
## This problem is tackled by 'regroup' option, see below.

## ------------------------------------—•°
## predict()

predict(cbg,val.df)                ## only wighted curve
predict(cbg,val.df,type="all")     ## just the whole "curves_and_prediction" object
predict(cbg,val.df,type="val")     ## all elements concerning valuated
predict(cbg,val.df,type="ref")     ## all elements concerning reference
predict(cbg,val.df,type="val_yields_groups")
predict(cbg,val.df,type=c("curves","val_yields_groups"))

## ------------------------------------—•°
## prediction() with regrouping

## write a number of group to replicate in a 'reference' column where NAs present.
## DO NOT change anything in the 'valuated' column!!! (It is not mentioned to do so hence a result is unpredictable!!!)

(pcbg <- prediction(cbg,val.df,regroup=TRUE))
summary(pcbg)
print(summary(pcbg),FALSE)
summary(pcbg,head=3)

   getOption("messages")
   options(messages=FALSE)

pcbg
summary(pcbg)                      ## regrouping visible only for weights and groups_efficiency and times; IS THAT OK?   !!!!
print(pcbg,messages=T)
print(summary(pcbg),messages=T)

   options(messages=TRUE)

rgr <- regroup(pcbg)
   class(rgr)
(pcbg <- prediction(cbg,val.df,regroup=rgr))
(spcbg <- summary(pcbg))
print(spcbg,messages=F)
messages(pcbg)
messages(spcbg)

predict(pcbg)
print(predict(pcbg),messages=F)

(ppcbg <- predict(pcbg,type="all"))   ## not all classes are mentioned
class(ppcbg)                          ## all classes of the object
   class(ppcbg$val_yields_groups)
   attributes(ppcbg$val_yields_groups)
print(ppcbg,2,round.attr=FALSE)
print(ppcbg,2)
print(ppcbg,as_rows=TRUE)
print(ppcbg,messages=FALSE)
print(ppcbg,messages=TRUE)

predict(pcbg,type="val")
predict(pcbg,type="ref")

## --------------------------------------------------------—•°
## smooothed curves

   source( paste(PATH$packs,paste0(PACKS,".R"),sep="/") )

## ------------------------------------—•°
## from cbg

( sgcbg <- smooth_gam(cbg) )
print(sgcbg,as_rows=T)
plot(sgcbg)
plot(sgcbg,ylim=c(0,0.2))
summary(sgcbg)
   efficiency(sgcbg)
   efficiency(sgcbg,cum=T)    ## cumulated efficiencies
   efficiency(smooth_gam(cbg,method="cumulated"),cum=T)  ## the same, OK!
   efficiency(smooth_gam(cbg,method="cumulated"))
   models_table(sgcbg)
      sgcbg$models_list
      sgcbg$models_list[]
      summary(sgcbg$models_list$g)

Write(sgcbg)
Write(sgcbg,as_rows=T)
Write(sgcbg,as_rows=T,round=3)
Write(sgcbg,as_rows=T,round=3,head=9)

( sgcbg <- smooth_gam(cbg,future=0) )
plot(sgcbg)
summary(sgcbg)

( sgcbg <- smooth_gam(cbg,net_curve=F) )
summary(sgcbg)
   efficiency(sgcbg)
   efficiency(sgcbg,T)        ## cumulated efficiencies
   efficiency(smooth_gam(cbg,net_curve=F,method="cumulated"),T)  ## the same, OK!

( sgcbg <- smooth_gam(cbg,weights_times=1:9) )
summary(sgcbg)
   sgcbg$weights_times
   class(sgcbg$weights_times)     ## "compact"
   unclass(sgcbg$weights_times)


( sgcbg <- smooth_gam(cbg,weights_times=9:1) )
plot(sgcbg)
( sgcbg <- smooth_gam(cbg,weights_times=matrix(runif(9*7),nrow=9)) )

   sgcbg$trials
   sgcbg$trials[]
   class(sgcbg$trials)

   tm(sgcbg$trials)
   class(tm(sgcbg$trials))
   tm(sgcbg$trials)[]

( sgcbg <- smooth_gam(cbg,weights_groups=1) )
( sgcbg <- smooth_gam(cbg,weights_groups=1:3) )
summary(sgcbg)
   efficiency(sgcbg)
      sgcbg$efficiencies
   efficiency(sgcbg,T)       ## cumulated efficiencies
   efficiency(smooth_gam(cbg,weights_groups=1:3,method="cumulated"),T)  ## the same, OK!

( sgcbg <- smooth_gam(cbg,weights_groups=1:3,net_curve=F) )
summary(sgcbg)
   efficiency(sgcbg)
      sgcbg$efficiencies
   efficiency(sgcbg,T)
   efficiency(smooth_gam(cbg,weights_groups=1:3,net_curve=F,method="cumulated"),T)  ## the same, OK!

plot(sgcbg)
plot(sgcbg,ylim=c(0,.15))
plot(sgcbg,add_points=F)
plot(sgcbg,groups=c("c"))
plot(sgcbg,groups=c("c","e","j","net_curve"))
plot(sgcbg,groups=c("c","e","j","net_curve"),add_points=F)
plot(sgcbg,groups=c("c","e","j","weighted_curve"))
plot(sgcbg,groups=c("c","e","j","net_curve","weighted_curve"))
plot(sgcbg,groups=c("net_curve","weighted_curve"))

( sgcbg <- smooth_gam(cbg,lengthen = function(x){.6*x[1] + .1*x[2]}) )
summary(sgcbg)
  sgcbg$arguments$lengthen
  sgcbg$lengthen

( sgcbg <- smooth_gam(cbg,lengthen = function(x){.6*x[1] + .1*x[2]},weights_groups=1:3) )
summary(sgcbg)
plot(sgcbg,ylim=c(0,.2))

( sgcbg <- smooth_gam(cbg,method="cumulated") )
( sgcbg <- smooth_gam(cbg,method="cumulated",net_curve=F) )
( sgcbg <- smooth_gam(cbg,method="cumulated",weights_groups=1:3) )
( sgcbg <- smooth_gam(cbg,method="cumulated",weights_groups=1:3,net_curve=F) )
summary(sgcbg)
summary(sgcbg,round=3)

## lengthening
( sgcbg <- smooth_gam(cbg,method="cumulated",weights_groups=1:3,lengthen = function(x){.6*x[1]+.1*x[2]}) )
   print(sgcbg,as_rows=T)
   print(sgcbg,round=4)
   print(sgcbg,head=5)

( sgcbg <- smooth_gam(cbg,method="cumulated",weights_groups=1:3,lengthen = .6) )
  sgcbg$lengthen
  sgcbg$lengthen(1)
( sgcbg <- smooth_gam(cbg,method="cumulated",weights_groups=1:3,lengthen = c(.6,.1)) )
  sgcbg$arguments$lengthen    ## argument passed
  sgcbg$lengthen              ## the function created from it
  sgcbg$lengthen(c(1,1))
  environment(sgcbg$lengthen)$a  ## this is how "a" can be accessed

     fac <- function(a){function(x)a*x}
     (f <- fac(.6))
     f(1)

   ( sgcbg <- smooth_gam(cbg,method="cumulated",weights_groups=1:3,lengthen = "a") )   ## ERROR

## net_curve=FALSE  -- do not use this option! it couses problems and will be depricated
( sgcbg <- smooth_gam(cbg,net_curve=F,method="cumulated") )
( sgcbg <- smooth_gam(cbg,net_curve=F,method="cumulated",weights_groups=1:3) )
( sgcbg <- smooth_gam(cbg,net_curve=F,method="cumulated",weights_groups=1:3,lengthen = function(x){.6*x[1] + .1*x[2]}) )

   Write(sgcbg)
## ------------------------------------—•°
## from pcbg

( sgpcbg <- smooth_gam(pcbg) )
print(sgpcbg,as_rows=T)
plot(sgpcbg)
plot(sgpcbg,ylim=c(0,0.2))
summary(sgpcbg)
   efficiency(sgpcbg)
   efficiency(sgpcbg,T)
   efficiency(smooth_gam(pcbg,method="cumulated"),T)  ## the same, OK!
   models_table(sgpcbg)
      sgpcbg$models_list
      sgpcbg$models_list[]
      summary(sgpcbg$models_list$g)

## ------------------------------------------------------------------------------------------------—•°
## prediction using smooth_gam

(psg <- prediction(sgcbg,newdata=val.df))
(psg <- prediction(sgcbg,newdata=val.df,regroup=TRUE))

summary(psg)
summary(psg,round=5)

Write(summary(psg))

plot(psg)


## ------------------------------------------------------------------------------------------------—•°
## ------------------------------------------------------------------------------------------------—•°
## curves_and_prediction

cap  <- prediction(cbg,val.df)
sgcap <- smooth_gam(cap)


print(cap,round=2)
print(cap,round=3)      ## default rounding
print(cap,round=2,round.attr=FALSE)  ## "round" attribute switched-off;
                        ## it concernes only $val_yields_groups for this class
                        ## notice that entries having non-NULL "round" attribute ($val_yields_groups for this class)
                        ## uses value of this attribute if it is not switched off directly by  round.attr=FALSE
print(cap,round=NULL)   ## no rounding at all
print(cap,round=FALSE)  ## "
print(cap,round=TRUE)   ## rounding only entries having non-NULL "round" attribute ($val_yields_groups for this class)
                        ## and uses value of this attribute
print(cap,round=TRUE,round.attr=FALSE)   ## "round" attributes switched off, hence nothing is rounded

print(cap,as_rows=FALSE)    ## default
print(cap,as_rows=TRUE)

summary(cap)
summary(cap,0)     ## only header
summary(cap,2)     ## only header + 2 times
summary(cap,2,5)   ## only header + 2 times  rounded to 5 decimal digits
summary(cap,2,10)  ## no more then 8 signif. numbers
summary(cap,20)    ## no more times then are in data
summary(cap,as_rows=FALSE)     ## in this summary  as_rows = TRUE by default as it gives more compact view
                               ## (order of values is consistent within columns)
summary(cap,2,as_rows=FALSE)
summary(cap,messages=FALSE)

names(cap)

plot(cap)

## --------------------------------------------------------—•°
## smooothed curves
( sgcap <- smooth_gam(cap) )
sgcap
plot(sgcap)
summary(sgcap)
models_table(sgcap)


## ---------------------------------------------------------------------------------------------------------------------—•°
## Example 2


## ---------------------------------------------------------------------------------------------------------------------—•°
## Example ...

## c. PLIKI ############################################################################################################—•°

   #data.w = data_w ; rm(data_w)
   #data.wp = data.pw ; rm(data.pw)
   #data.p = data_p ; rm(data_p)

## --------------------------------------------------------
## LUB z pliku jeśli takowy już istnieje
  load( "EfficiencyCurves.Rdata" )

## --------------------------------------------------------
## podstawowe sprawdzenie danych

( nt.w = nulls0.table(data.w) )
   ( nt.wp = nulls0.table(data.wp) )
( nt.p = nulls0.table(data.p) )
( nt.wprz = nulls0.table(data.wprz) )
( nt.np = nulls0.table(data.np) )


## --------------------------------------------------------
## podstawowe obiekty

VAR = list()
VAR$id = "id_produkt"
VAR$debt = "saldo_start_komornik"
VAR$pays = "kwota_wplat"
VAR$vintage = "rok_mies_start_komornik"
VAR$time = "mk"
VAR$date = "rok_mies"

VARw = list()
VARwp = list()
VARp = list()
VARwprz = list()

## podstawowa zmienna identyfikująca
VARw$id = c("id_produkt","mk")
rownames(data.w)  = apply(data.w[,VARw$id],1,function(x)paste(x,collapse="."))  #! b.wazne

VARwp$id = c("id_produkt","mk")
rownames(data.wp) = apply(data.wp[,VARwp$id],1,function(x)paste(x,collapse="."))  #! b.wazne

VARp$id = "id_produkt"
rownames(data.p) = data.p[,VARp$id]  #! b.wazne

VARwprz$id = c("rok_mies_start_komornik","rok_mies","grupa_portfela")
rownames(data.wprz) = apply(data.wprz[,VARwprz$id],1,function(x)paste(x,collapse="."))  #! b.wazne

   ## variables
   variables = list()
   ##( variables$nonempty = tab.new0[tab.new0$OKs>0,"name"] )

 head(data.w)
 head(data.wp)
 head(data.p)
 head(data.wprz,20)

condlists = list()

## --------------------------------------------------------
## zapis danych do pliku -- później juz nie trzeba będzie laczyc sie z bazą
#!#! warto
save(list=c("data.w","data.wp","data.p","data.wprz","data.np"),file = FILEpred_variables.Rdata , compress="xz" )
## --------------------------------------------------------

      #!#############################
      #!#############################
        data.new = data.new0[,variables$nonempty]         #! od teraz pracujemy na data.new -- kopia oryginalnych danych do prognozy
        variables$new0 = names(data.new0)  ## moze się jeszcze przydać
        variables$new = names(data.new)    ## korygujemy nazwy zmiennych
        rownames(data.new) = data.new[,VARid]
      #!#############################

        dim(data.new)   ## 14349   197

      #####################################################

## ---------------------------------------------------------------------------------------------------------------------—•°
save.image(file=FILEpred.environment)#,compress="xz")
## ---------------------------------------------------------------------------------------------------------------------—•°


## ---------------------------------------------------------------------------------------------------------------------—•°
## ---------------------------------------------------------------------------------------------------------------------—•°
## II. MODEL -- krzywe
## ---------------------------------------------------------------------------------------------------------------------—•°

## ------------------------------------------------------------------------------------------------—•°
## 6. Badanie danych

dim(subset(data.wp,!grupa_portfela%in%c('MŚP','hipoteka') & mk_max>18))   ## 202399     54

max(data.wp$mk)      ## 58

with(data.wp, {tt <- table(rok_mies_start_komornik,mk_max) ; tt[nrow(tt):1,]} )
##  2016-09   2
##  ...
##  2016-01  10
##  ...
##  2015-02  21
##  2015-01  22
##  2014-12  23
##  ...
##  2012-01  58
## ------------

unique(data.wp$grupa_portfela)
table(data.wp$grupa_portfela,useNA="ifany")

nt.wp
## ----------------------------------------------------------------------------—•°
## data.wprz -- wplaty przedzialy

   #   with( data.wprz , {
   #      rmsk <- as.POSIXct( paste0(rok_mies_start_komornik,'-01') ) ;
   #      tapply( kwota_wplat , list( rok_mies_start_komornik , rok_mies ) , length  )
   #   } )  ## upper-triangular -- OK
   #
   #   ##!#!#!#!#!!#!#!#!#!
   #   tab.wprz <- with( data.wprz , as.data.frame(tapply( kwota_wplat , list( mk , rok_mies_start_komornik ) , sum )) )
   #   ##!#!#!#!#!!#!#!#!#!


## ----------------------------------------------------------------------------—•°
## B. Obserwacje odstające
extract = extract.variable( variable.name="kwota_wplat"
                , datfram=data.w
                , lft=0
                #, rgt=3e4
                , FUN=function(x){ifelse(x>3e4,3e4,x)}
                )
## names(extract)
plot.variable(kwota_wplat, breaks = c(0,10,50,100,200,500,1e3,5e3,1e4,3e4,1e6) )

with( data.w , tapply( kwota_wplat , cut(kwota_wplat,c(0,10,50,100,200,500,1e3,5e3,1e4,3e4,1e6)) , sum ) )


condlists$wp <- list(
     "kwota_wplat_0" = list( inputs = "kwota_wplat" , conditions = "all" , substitute = quote(a))  ## kopia oryginalnych danych
   , "kwota_wplat" = list( conditions = quote(x>3e4) , substitute = 3e4 )
)

## ----------------------------------------------------------------------------—•°
## C. Poszukiwanie optymalnego podziału na grupy

conditions0 = quote({saldo_start_komornik >0 & !grupa_portfela %in% c('MŚP','hipoteka') & czy_fraud == 0 & czy_przedawniona == 0 })

   with( subset( data.p , eval(conditions0) )
   , { table(saldo_start_kom_przedzialy,grupa_portfela,mk_max < 23) } )

   with( subset( data.p , eval(conditions0) )
   , { table( cut( saldo_start_komornik , c(0,1e3,2e3,5e3,1e4,3e4,10e4,1e6) , include.lowest = TRUE )
            , grupa_portfela , mk_max < 23 ) } )

   with( subset( data.p , eval(conditions0) )
   , { table( cut( saldo_start_komornik , c(0,1e3,2e3,5e3,1e4,3e4,10e4,1e6) , include.lowest = TRUE )
            , paste( grupa_portfela , ifelse( id_bank %in% c(60,90,142) , "BZ" , "inne" ) , sep = '.' )
            , ifelse( mk_max < 23 , "wyceniany" , "referencja" )
            )
     }
   )

   with( subset( data.p , eval(conditions0) )
   , { table( id_bank , grupa_portfela )  } )

   with( subset( data.p ,eval(conditions0) )
   , {   typ_bank <- as.factor(id_bank) ;
         levels(typ_bank) <- local({ BZ = c("60","90","142")
                                   ; telekom = c("46","55","62","80","86")
                                   ; bez = c("147","148","149","150","153","164"
                                             ,"100","103","110"   ## HOIST
                                             ##,"162","175"         ## NC
                                             , "73","105" , "98"        ## BankOK , BOŚ , CyfroPol
                                             , "1", "2","3","6","12"    ## Stare Portfele
                                             )
                                   ; inne = setdiff(unique(id_bank),c(BZ,telekom,bez))
                                   ; list( BZ = BZ, telekom = telekom , bez = bez , inne = inne)
                                   })
         table( cut( saldo_start_komornik , c(0,1e3,2e3,5e3,1e4,3e4,10e4,1e6) , c("1. 0-1k","2. 1-2k","3. 2-5k","4. 5-10k","5. 10-30k","6. 30-100k","7. 100k+") , include.lowest = TRUE )
            , typ_bank
            , ifelse( mk_max < 23 , "wyceniany" , "referencja" )
            )
     }
   )

## --------------------------------------------------------—•°
## D. Podział na grupy


condlists$p = list(
     "saldo_start_kom_p2" = list(
           conditions = "all"
         , inputs = "saldo_start_komornik"
         , substitute = quote( { cut( a , c(0,1e3,2e3,5e3,1e4,3e4,10e4,1e6) , c("1. 0-1k","2. 1-2k","3. 2-5k","4. 5-10k","5. 10-30k","6. 30-100k","7. 100k+") , include.lowest = TRUE) } )
      )
   , "typ_bank" = list(
           conditions = "all"
         , inputs = "id_bank"
         , substitute = quote( { a <- as.factor(a)
                               ; levels(a) <- local({ BZ = c("60","90","142")
                                                    ; telekom = c("46","55","62","80","86")
                                                    ; bez = c("147","148","149","150","153","164"
                                                               ,"100","103","110"   ## HOIST
                                                               ##,"162","175"         ## NC
                                                               , "73","105" , "98"        ## BankOK , BOŚ , CyfroPol
                                                               , "1", "2","3","6","12"    ## Stare Portfele
                                                               )
                                                    ; inne = setdiff(unique(a),c(BZ,telekom,bez))
                                                    ; list( BZ = BZ, telekom = telekom , bez = bez , inne = inne)
                                                   })
                                ; a
                              } )
      )
   , "grupa_saldo_portfel" = list(
           conditions = "all"
         , inputs = c("saldo_start_kom_p2" , "typ_bank")
         , substitute = quote( { gg <- paste( a , b )
                               ; gg <- ifelse( gg %in% c("6. 30-100k BZ","7. 100k+ BZ") , "6. 30+ BZ" , gg )
                               ; gg <- ifelse( gg %in% c("5. 10-30k telekom","6. 30-100k telekom","7. 100k+ telekom") , "5. 10k+ telekom" , gg )
                               ; gg <- ifelse( gg %in% c("5. 10-30k bez","6. 30-100k bez","7. 100k+ bez") , "5. 10k+ bez" , gg )
                               ; gg <- ifelse( gg %in% c("4. 5-10k inne","5. 10-30k inne","6. 30-100k inne","7. 100k+ inne") , "4. 5k+ inne" , gg )
                               ; factor(gg)
                               } )
      )
)
class(condlists$p) = c("condlist","list")
condlists$wp <- c(condlists$wp,condlists$p)
condlists$np <- c(condlists$p
                 , list( "grupa_saldo_portfel" = list(  conditions="all"
                              , substitute=quote({ levels(x)<-levels(get("data.p2",pos=1)$grupa_saldo_portfel) ; x })
                              )
                       )
                 )

## ! Basic conditions first !!!!
data.p2 <- subset( data.p , eval(conditions0) )
data.p2 <- condapply( data.p2 , condlists$p , isolate = FALSE )
with(  data.p2 , { table( grupa_saldo_portfel , ifelse( mk_max < 23 , "wyceniany" , "referencja") ) } )


## ! Basic conditions first !!!!
data.wp2 <- subset( data.wp , eval(conditions0) )
data.wp2 <- condapply( data.wp2 , condlists$wp , isolate = FALSE )
with( data.wp2 , { table( grupa_saldo_portfel , ifelse( mk_max < 23 , "wyceniany" , "referencja") ) } )

## Basic conditions irrelevant except one
data.np2 <- subset( data.np , saldo_start_komornik >= 200 )
data.np2 <- condapply( data.np2 , condlists$np , isolate = FALSE )
with( data.np2 , { table( grupa_saldo_portfel , ifelse( mk_max < 23 , "wyceniany" , "referencja") ) } )

## --------------------------------------------------------—•°
## E. RZECZYWISTE WPŁATY DLA PRZEKAZAŃ w okresie od daty przekazania do dziś

   range(data.wp2$kwota_wplat)
   with( data.wprz , as.data.frame(tapply( kwota_wplat , list( mk , rok_mies_start_komornik ) , sum )) )
   nt.wp

## WPŁATY
(pays.vintage.upper <- with( data.wp2 , as.data.frame(tapply( kwota_wplat , list( mk , rok_mies_start_komornik ) , sum )) ))
   write.table(pays.vintage.upper,file="pays.vintage.upper.csv", sep = ";", na="NA" ,dec = ",", qmethod = "double")
(pays.vintage.lower <- with( data.wp2 , as.data.frame(tapply( kwota_wplat , list( rok_mies , rok_mies_start_komornik ) , sum )) ))
   write.table(pays.vintage.lower,file="pays.vintage.lower.csv", sep = ";", na="NA" ,dec = ",", qmethod = "double")

## ILOŚĆ spraw płacących dla każdego przekazania i każdego miesiąca
t1 = with( data.wp2 , tapply( id_produkt , list( mk , rok_mies_start_komornik ) , function(x)length(unique(x)) ) )
## ilość spraw dla każdego przekazania
t2 = with( data.p2  , tapply( id_produkt , list( rok_mies_start_komornik ) , function(x)length(unique(x)) ) )
rbind(t2,t1)

##!#!#!#!#!!#!#!#!#!#!#!#!#!#!!#!#!#!#!

amount.var = "saldo_start_komornik"
 group.var = "grupa_saldo_portfel"
  time.var = "mk"
 yield.var = "kwota_wplat"
    id.var = "id_produkt"

## ------------------------------------------------------------------------------------------------—•°
## solution 1
##
## We take as reference all cases from having mk_max >=22 i.e. sent to bailiff before 2015-01.
## We aquire curve for 25 months (which we can lengthen later to 50/60 mths by *.95) calculated for every
## mk_max < 23 separately
##

## ------------------------------------—•°
## PARAMETERS
K = 25  ;  horizon = 60 ;

## ------------------------------------—•°
## OBJECTS

data.ref = subset( data.wp2  ##! Basic conditions taken above !!!
                 , mk_max >= K &  mk <= K
                 )

data.val = subset( data.p2 , mk_max == 2 )
(cbg = curves.by.groups( data.val , data.ref
      , amount.var
      , group.var
      , time.var
      , yield.var
      , id.var
      , file = NULL ))
plot(cbg,what="normal")

## V. IMPORTANT !!!
(curves.ref <- cbg$ref.yields.groups)
(ngr = nrow(curves.ref)-1)
curves.ref <- t(round(curves.ref[1:ngr,]))
write.table(curves.ref,file="curves.ref.csv", sep = ";", na="NA" ,dec = ",", qmethod = "double")

## NEEDED !!!
( amounts.ref <- cbg$amounts[,"reference"] )
any(names(amounts.ref) != colnames(curves.ref)) ## FALSE  OK!


## ------------------------------------—•°
## LOOP
scg.list <- list()
s_curves.list = list()
s_curves.list$gam.upper  <- s_curves.list$gam.lower <- s_curves.list$len.upper <- s_curves.list$len.lower  <-  cbind(1:horizon)[,-1]
names.vec <- character(0)
  #
for(k in 22:0){ #  k=11
     ##
   if(k==0){
      data.val = data.np2          ## new bailiff pass   2016-10
   }else{
      data.val = subset( data.p2 , mk_max == k )   ##! Basic conditions taken above !!!
   }
   amounts.val = with( data.val , tapply( saldo_start_komornik , grupa_saldo_portfel , sum , na.rm=TRUE) )
   amounts.val[is.na(amounts.val)] = 0
      #
   if( nrow(data.val) > 3 ){
         ##
      rok_mies = unique( as.character(data.val$rok_mies_start_komornik) )
      names.vec <- c(names.vec,rok_mies)
         #
      scg.list[[rok_mies]] = smooth.curves.gam(
                       successes = curves.ref  ## vector or matrix of successes
                     , trials    = amounts.ref
                     , weights.curves = amounts.val
                     , weights.times  = NULL
                     , horizon = 60
                     , lengthen = function(x){x[1]*.96}
                     #, wiggliness = 8
                     #, method =  "cumulated" #"normal" ##
                     , file = paste0("curves_csv/scg_",rok_mies,c("_nor","_len","_cum"),".csv")
                     , plot = TRUE , add_points = FALSE
                     , coldef = "black"
                     , ylim = NULL
                     , ylim.cum = NULL
                     , titles = rok_mies
                )
#   curves.list[[rok_mies]] = curves.by.groups( data.val, data.ref ,amount.var ,group.var ,time.var ,yield.var ,id.var
#      #,file = paste0("curve_",ifelse(k<10,paste0(0,k),k),".csv")) # ,file = paste0("curve_",rok_mies,".csv") )
         #
      s_curves.list$gam.lower = cbind( s_curves.list$gam.lower , sum(amounts.val) * c( rep(0,22-k) , scg.list[[rok_mies]]$smoothed.curves[,"weighted_smoothed_curve"] )  )
      s_curves.list$len.lower = cbind( s_curves.list$len.lower , sum(amounts.val) * c( rep(0,22-k) , scg.list[[rok_mies]]$smoothed.curves.len[,"weighted_smoothed_curve"] ) )
      s_curves.list$gam.upper = cbind( s_curves.list$gam.upper , sum(amounts.val) * c( rep(0,22-k) , scg.list[[rok_mies]]$smoothed.curves[,"weighted_smoothed_curve"] )  )
      s_curves.list$len.upper = cbind( s_curves.list$len.upper , sum(amounts.val) * c( rep(0,22-k) , scg.list[[rok_mies]]$smoothed.curves.len[,"weighted_smoothed_curve"] ) )
      colnames(s_curves.list$gam.upper) <- colnames(s_curves.list$len.upper) <- colnames(s_curves.list$gam.lower) <- colnames(s_curves.list$len.lower) <- names.vec
   }
}
write.table(s_curves.list$gam.lower, file="s_curves.gam.lower.csv", sep = ";", na="NA", dec = ",", qmethod = "double")
write.table(s_curves.list$len.lower, file="s_curves.len.lower.csv", sep = ";", na="NA", dec = ",", qmethod = "double")
write.table(s_curves.list$gam.upper, file="s_curves.gam.upper.csv", sep = ";", na="NA", dec = ",", qmethod = "double")
write.table(s_curves.list$len.upper, file="s_curves.len.upper.csv", sep = ";", na="NA", dec = ",", qmethod = "double")



## --------------------------------------------------------—•°
##  criticism of the result
   length(curves.list)  ## 20
   names(curves.list)   ##  "2015-01" ... "2016-09"
   names(curves.list[['2015-01']])




for( nam_k in names(curves.list)){

}


(scg <- curves.list[['2015-03']])
##plot(cbg,what="normal")
t(cbg$summary.df)

names(cbg)

plot(cbg$val.yields.from.weighted, type = "l")
smooth.curves.gam( successes = round(cbg$val.yields.from.weighted)  ## vector or matrix of successes
                           , trials = NULL
                           , weights.curves = NULL
                           , weights.times  = NULL
                           , horizon = 60
                           , lengthen = function(x){x[1]*.972}
                           #, wiggliness = 5
                           #, method =  "cumulated" #"normal" ##
                           , file = NULL
                           , graphics = TRUE
                           , coldef = "black"
                           , ylim = NULL
                           , ylim.cum = NULL
                           #, ...
                           )

## ----------------
## comparing with real performance
nt.wprz
tab.wprz

data.wprz$mk =

tab[,"2015-03"]

windows();par.my()
matplot( cbind(cbg$val.yields.from.weighted), type = "l")

class(cbg)




## ---------------------------------------------------------------------------------------------------------------------—•°
save.image(file=FILEpred.environment,compress="xz")
## ---------------------------------------------------------------------------------------------------------------------—•°



## ---------------------------------------------------------------------------------------------------------------------—•°
## ---------------------------------------------------------------------------------------------------------------------—•°
## V. ZAPIS
## ---------------------------------------------------------------------------------------------------------------------—•°

## --------------------------------------------------
## A. do .csv -- pózniej kopiujemy do .xls
  write.table(Prediction.df, FILEpred_results.csv , sep = ";", na="" ,dec = ",", qmethod = "double", row.names = F)
  ## pamietaj o przekopiowaniu wynikow do tabeli w pliku:
      FILEpred_results.xls

## --------------------------------------------------
## B. zapis do BD
ch <- odbcConnect("PostgreSQL35W",uid = "arkadiusz.kasprzyk", pwd = "akasprzyk123")

    ## z powodu problemów z zapisem dat do BD trzeba je zamienić na character
		Prediction.df.0 = capply( Prediction.df
										, class = "Date"
										, FUN = as.character
										, within = TRUE
										)    #!#!#! nowa moja funkcja !!!!
		nulls.table(Prediction.df.0)   ## nie powinno być juz dat tj. brak class(variable) -> data

	sqlQuery(ch,paste0("DROP TABLE IF EXISTS ",TABLEresults))
	sqlSave(channel = ch,tablename = TABLEresults , dat =  Prediction.df.0, verbose = TRUE)                  ##rownames = names(wynik1[[1]])
	rm(Prediction.df.0)

odbcClose(ch)

## --------------------------------------------------
## C. do .Rdata -- tylko model i prognoza i inne potrzebne obiekty, ale NIE cale srodowisko 
  save( list = c(  
      "packages"               ,
      "ROOT"  ,  "DOMAIN"      ,
      ls(pattern='^TABLE')     ,
      ls(pattern='^FILE')      ,
      ls(pattern='^PATH')      ,
      ls(pattern='^VAR')       ,
      ##
      ls(pattern='_variable$') ,
      "variables" ,
      ls(pattern='^breaks\\.') ,
      "Data.ref" , "data.new" , "Data.new" ,
      "order.data.new" , ### ls(pattern='^(D|d)ata\\.') ###
      ###
      names.models ,
      ###
      DFerrs , DFouts ,  "Data.new.outs" ,
      LISTtrans , # DFtrans ,
      ###
      "saldo_start_etap" ,
      "df.list" , "prediction.list" , "Prediction.df" 
      )
   , file = FILEpred_results.Rdata , compress = "xz")


## --------------------------------------------------
## D. do .csv -- wszystkie zmienne -- raczej niepotrzebne, zajmuje duzo miejsca
    names(Prediction.df)
    write.table(	update.df( Prediction.df[,c(1, 65:71 , 2:64)]  ## zachowamy porządek rekordów z Prediction.df 
                          ,  data.new   ## stąd bierzemy wszystkie pozostale zmienne, nawet te, które nie są w modelu,  
                          , id=0        ## lączymy tabele wg. nazw wierszy tj. rownames 
                          , action = "replace"  ## kolumny nie powtórzą się. 
                          ) 
  				  ##, c("id_produkt_k" , "glm_logit_binary_1" , "glm_logit_binary_1_wplaty" , "glm_logit_binomial_1" , "glm_logit_binomial_1_wplaty" , "glm_log_quasipoisson_1")] #,"glm_log_quasipoisson_1") ]
          			, FILEpred_variables.csv , sep = ";", na="" ,dec = ",", qmethod = "double", row.names = F )


## --------------------------------------------------
## E. zapis calego srodowiska -- raczej niepotrzebne, zajmuje duzo miejsca

      ## zapis calego srodowiska -- to nie jest konieczne i bez wyraźnej potrzeby lepiej tego nie robic
      ##    -- wielki plik i zapewne nie bedzie nigdy otwarty; wszystko co wazne zostalo zapisane powyzej
      #! LEPIEJ NIE!!!
        save.image(file=FILEpred.environment,compress="xz")
      ## jesli jednak powyzszy plik byl utworzony w trakcie robienia porgnozy to najlepiej go usunąć z w/w powodów.


## ---------------------------------------------------------------------------------------------------------------------—•°
#! Uzupelnij naglowek pliku o liste plikow (jesli jest taka potrzeba) 
## ---------------------------------------------------------------------------------------------------------------------—•°
     
## ---------------------------------------------------------------------------------------------------------------------—•°
#! KONIEC KONIEC KONIEC KONIEC KONIEC KONIEC KONIEC KONIEC KONIEC KONIEC KONIEC KONIEC KONIEC KONIEC KONIEC KONIEC #####—•°
## ---------------------------------------------------------------------------------------------------------------------—•°
