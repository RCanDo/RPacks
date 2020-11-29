## ---------------------------------------------------------------------------------------------------------------------•°
## DZIA£ ANALIZ -- ANALIZY -- MODELE STATYSTYCZNE -- A.K. -- 150325
## ---------------------------------------------------------------------------------------------------------------------•°
##
## ---------------------------------------------------------------------------------------------------------------------•°
   #  rm(list=ls())
## ---------------------------------------------------------------------------------------------------------------------•°
## 0. Do poprawnego dzialania TinnR (powinno ladowac siê samo - trzeba naprawic jakies .ini)
#  .trPaths <- paste(paste(Sys.getenv('APPDATA'), '\\Tinn-R\\tmp\\', sep=''),
#            c('', 'search.txt', 'objects.txt', 'file.r', 'selection.r', 'block.r', 'lines.r'), sep='')
## ---------------------------------------------------------------------------------------------------------------------•°

## ---------------------------------------------------------------------------------------------------------------------•°
#! I. KATALOGI, ZMIENNE, PAKIETY, SKRYPTY, £ADOWANIE DANYCH i MODELI ###################################################•°
## ---------------------------------------------------------------------------------------------------------------------•°

## ---------------------------------------------------------------------------------------------------------------------•°
## 1. KATALOGI I PLIKI #################################################################################################•°

#ROOT = "D:/ROBOCZY/PK_Analitycy"
##ROOT = "//10.1.1.5/PK_Analitycy"
#PATHwd = paste0(ROOT,"/R/FunctionsAK")
#PATHsource = paste0(ROOT,"/R/FunctionsAK")

## ---------------------------------------------------------------------------------------------------------------------•°
## 3. PAKIETY I SKRYPTY ################################################################################################•°

## PAKIETY
#packages = c( "adagio"
##  , "MASS", #  "RPostgreSQL","RODBC","vioplot"  #    , "rpart", "randomForest","car",
##  "ROCR","RODBC","igraph", #  "gam","rattle","igraph", #  "stats","graphics","grDevices",
##  "datasets","methods","base", #  "caret","gdata","sqldf" #  ,"data.table"
# )

## Instalacja pakietów jesli jeszcze nie zainstalowane
#  chooseCRANmirror()
#  for(x in packages)install.packages(x)

#! Pakiety nie laduja sie wraz z przestrzenia robocza!
#! Ladowanie pakietow - konieczne!
l = quote(library(a))
for(p in packages){ l[[2]] = as.name(p);  eval(l) }    #!!! YEAH!!!
  #!  l; as.list(l)   ## check it !!!

## 0. ## LOADER FUNCTION
#source_to_env <- function(files,path,env){
#   env <- attach(NULL, name = env)
#   sapply(files, FUN = function(x) sys.source(paste(path,x,sep='/'),env))
#   print(ls(env))
#}

## SKRYPTY 1  ## skrypty nowe
#source_to_env( files = c("o2p0-sub1.r","last.r","fill.from.left.r","addid.r","update.df.r" )
#             , path = PATHsource
#             , env = "FunctionsAK"
#)

## SKRYPTY 2  ## skrypty stare (do scoringu)
  #setwd(paste0(ROOT,"/R/FunctionsScoring"));   source("sourceFunctionsScoring.r");   setwd(PATHwd)

## SKRYPTY 3  ## ladowanie dodatkowych skryptów
  #source("")    ## moze jeszcze cos?



## ---------------------------------------------------------------------------------------------------------------------•°



## ---------------------------------------------------------------------------------------------------------------------•°
## SIMULATION
doo2pdf = function(N=30,ops=9,groups_per_op=5){
   datfram = data.frame(id_operator=numeric(0),group=character(0))
   for(k in 1:ops){
      datfram = rbind(  datfram ,
        data.frame(
             id_operator = rep(k,N)
          ,  group = sample( sample(letters[1:ops],groups_per_op) , N , replace=TRUE )
        )    ## N x 2
      )
   }
   datfram
}
datfram = doo2pdf(N=50,ops=9,groups_per_op=5) ## ops*N x 2
datfram = cbind(   id_case = sample(100:999,dim(datfram)[1]) , datfram , weight = sample(seq(0,5,.5),dim(datfram)[1],replace=TRUE) )
   #
## ---------------------------------------------------------------------------------------------------------------------•°
## PRE-CALCULATIONS
   #
tolerance = 2
   #
   #   with(datfram,addmargins(table(id_operator,feature_a)))
tab = with( datfram , { tab <- tapply(weight,list(id_operator,group),sum) ; tab <- ifelse(is.na(tab),0,tab) })
    tab = tab[ , names(gr.presence <- sort(apply(tab,2,function(x){sum(x>0)}),"decreasing"=TRUE))]     #!X!
( tab = tab[ names(op.weights <- sort(apply(tab,1,function(x){sum(x)}),"decreasing"=TRUE)) , ] )
      #
   ( tab.lumps.bin = (tab<0) ) ## shortcut to get only FALSEs #!#! this table will be UPDATED BELOW in  spread.method(...)
   ( tab.lumps = tab*0 )    ## shortcut to get only 0s #!#! this table will be UPDATED BELOW in  spread.method(...)
      #
( op.weights = op.weights - round(2*mean(op.weights))/2 )   ## OK!!!
( op.weights.up = op.weights[op.weights>tolerance] )
( op.weights.down = rev(-op.weights[op.weights < -tolerance]) )


## ---------------------------------------------------------------------------------------------------------------------•°
## testing function
## ---------------------------------------------------------------------------------------------------------------------•°
   ##
   datfram2 = refine.assignment( datfram , operator.var = "id_operator", group.var = "group" , weight.var = "weight" , tolerance = 2 )

      #   with(datfram,addmargins(table(id_operator,feature_a)))
   tab2 = with( datfram2 , { tab2 <- tapply(weight,list(id_operator,group),sum) ; tab2 <- ifelse(is.na(tab2),0,tab2) })
       tab2 = tab2[ , names(gr.presence.2 <- sort(apply(tab2,2,function(x){sum(x>0)}),"decreasing"=TRUE))]     #!X!
   ( tab2 = tab2[ names(op.weights.2 <- sort(apply(tab2,1,function(x){sum(x)}),"decreasing"=TRUE)) , ] )
         #
      ( tab2.lumps.bin.2 = (tab2<0) ) ## shortcut to get only FALSEs #!#! this table will be UPDATED BELOW
      ( tab2.lumps.2 = tab2*0 )    ## shortcut to get only 0s #!#! this table will be UPDATED BELOW
         #
   ( op.weights.2 = op.weights.2 - round(2*mean(op.weights.2))/2 )   ## OK!!!
   ( op.weights.up.2 = op.weights.2[op.weights.2>tolerance] )
   ( op.weights.down.2 = rev(-op.weights.2[op.weights.2 < -tolerance]) )


## ---------------------------------------------------------------------------------------------------------------------•°
## THE MAIN LOOP

stop_loop = FALSE
while( length(op.weights.up)>0 & length(op.weights.down)>0 & !stop_loop ){  ##  1. LOOP_1  ##

   ( tab.bin = tab > 0 )    ## which gr.op (group by operator -- groupo-operator) is present;
                            ## will be actualised with FALSEs in order to exclude further gr.ops
                            ## e.g. when gr.op contains some fat case for which there is no room anywhere
                            ## see (1) below

   ## upper operator i.e. with  largest amount of cases over the mean
   ( op.up = op.weights.up[1] )          ## overload of the most overloaded operator
    ( op.up.row = tab[names(op.up),] )



   if( group.to.spread( mode = "small" , spread = "all" , no_lumps = TRUE ) ){       ## mode.small.all.no_lumps
      spread.method( force_lumps = FALSE , rollback = TRUE )
      #
   }else if( group.to.spread( mode = "big" , spread = "all" , no_lumps = TRUE ) ){   ## mode.big.all.no_lumps
      spread.method( force_lumps = FALSE , rollback = TRUE)
      #
   }else if( group.to.spread( mode = "small" , spread = "all" , no_lumps = FALSE ) ){ ## mode.small.force_lumps
      spread.method( force_lumps = TRUE )
      #
   }else if( group.to.spread( mode = "big" , spread = "all" , no_lumps = FALSE ) ){  ## mode.big.all.force_lumps
      spread.method( force_lumps = TRUE )
      #
      #
   }else if( group.to.spread( mode = "big" , spread = "any" , no_lumps = TRUE ) ){   ## mode.big.any.force_lumps
      spread.method( force_lumps = FALSE )
      #
   }else if( group.to.spread( mode = "big" , spread = "any" , no_lumps = FALSE ) ){  ## mode.big.any.force_lumps
      spread.method( force_lumps = TRUE )
      #
   }else{ stop_loop = TRUE }




   ## recalculating  tab
      ( tab = with( datfram , { tab <- tapply(weight,list(id_operator,group),sum) ; tab <- ifelse(is.na(tab),0,tab) }) )
      ( tab = tab[ , names(gr.presence <- sort(apply(tab,2,function(x){sum(x>0)}),"decreasing"=TRUE))] )   #!X!
   ( tab = tab[ names(op.weights <- sort(apply(tab,1,function(x){sum(x)}),"decreasing"=TRUE)) , ] )
      #  tab
   ( op.weights = op.weights - round(2*mean(op.weights))/2 )   ## OK!!!
      # (weight.mean = round(sum(datfram$weight)/length(unique(datfram$id_operator)),1))                   #!X!
      gr.presence                                                                                          #!X!
   ( op.weights.up = op.weights[op.weights>tolerance] )
   ( op.weights.down = rev(-op.weights[op.weights < -tolerance]) )

   (tab.lumps_k = tab.lumps + tab.lumps.now[rownames(tab.lumps),colnames(tab.lumps)])
   stop_loop = ( ( sum(tab.lumps_k>1) == sum(tab.lumps_k>0) ) & sum(tab.lumps_k>0)>0 ) | stop_loop

} ##----END----##  of  group.to.spread()
## ---------------------------------------------------------------------------------------------------------------------•°


## ---------------------------------------------------------------------------------------------------------------------•°





## ---------------------------------------------------------------------------------------------------------------------•°
## EXAMPLES ############################################################################################################•°
## ---------------------------------------------------------------------------------------------------------------------•°

## -------------------------------------------------------------------------------------------•°
dummy = function(){
## This is dummy function - it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## - in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
## -------------------------------------------------------------------------------------------•°
# Example 1
p <- c(15, 100, 90, 60, 40, 15, 10,  1)
w <- c( 2,  20, 20, 30, 40, 30, 60, 10)
cap <- 102
(is <- knapsack(w, p, cap))
# [1] 1 2 3 4 6 , capacity 102 and total profit 280
## Example 2
p <- c(70, 20, 39, 37, 7, 5, 10)
w <- c(31, 10, 20, 19, 4, 3,  6)
cap <- 50
(is <- knapsack(w, p, cap))
# [1] 1 4 , capacity 50 and total profit 107

p = rep(1,10)
w = sample(seq(1,10,.5),10,replace=TRUE)
cap = 13.5
(is = knapsack(w,p,cap))

p = sample(1:10)
w = seq(1,10)
cap = 9
(is = knapsack(w,p,cap))


is[]
names(is)
attributes(is)
class(is)

## ---------------------------------------------------------------------------------------------------------------------•°
a = 4
ff = function(x){
  a <<- a+x
}


a
ff(1)
a

gg = function(y){
   ff = function(x){
     a <<- a+x      ## to parent env. !!!
   }
a = 1
ff(y)
a
}

a
gg(1)
a

hh = function(x){
   xx = list()
   xx$arg = x
   xx$bool = as.logical(floor(2*x))
   xx<<-xx
   xx$bool
}

hh(1)
xx

if(hh(runif(1))){
   print(xx)
}
xx

## ---------------------------------------------------------------------------------------------------------------------•°
}

rm(dummy)
