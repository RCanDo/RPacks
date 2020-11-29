## --------------------------------------------------------------------------------------------------------------------•°
## DZIA£ ANALIZ -- ANALIZY -- MODELE STATYSTYCZNE -- A.K. -- 150325
## --------------------------------------------------------------------------------------------------------------------•°
##
## --------------------------------------------------------------------------------------------------------------------•°
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

ROOT = "D:/ROBOCZY/PK_Analitycy"
#ROOT = "//10.1.1.5/PK_Analitycy"
PATHwd = paste0(ROOT,"/R/FunctionsAK")

## ---------------------------------------------------------------------------------------------------------------------•°
## 3. PAKIETY I SKRYPTY ################################################################################################•°

## PAKIETY
packages = c( "adagio"
#  , "MASS", #  "RPostgreSQL","RODBC","vioplot"  #    , "rpart", "randomForest","car",
#  "ROCR","RODBC","igraph", #  "gam","rattle","igraph", #  "stats","graphics","grDevices",
#  "datasets","methods","base", #  "caret","gdata","sqldf" #  ,"data.table"
 )

## Instalacja pakietów jesli jeszcze nie zainstalowane
#  chooseCRANmirror()
#  for(x in packages)install.packages(x)

#! Pakiety nie laduja sie wraz z przestrzenia robocza!
#! Ladowanie pakietow - konieczne!
l = quote(library(a))
for(p in packages){ l[[2]] = as.name(p);  eval(l) }    #!!! YEAH!!!
  #!  l; as.list(l)   ## check it !!!

## SKRYPTY 1  ## ladowanie skryptów do scoringu
  #setwd(paste0(ROOT,"/R/FunctionsScoring"));   source("sourceFunctionsScoring.r");   setwd(PATHwd)

## SKRYPTY 2  ## ladowanie dodatkowych skryptów
  setwd(paste0(ROOT,"/R/FunctionsAK"));      #source("sourceFunctionsAK.r");
   source("o2p0-sub1.r") ; source("last.r") ; source("fill.from.left.r") ; source("addid.r") ; source("update.df.r")
  setwd(PATHwd)

## SKRYPTY 3  ## ladowanie dodatkowych skryptów
  #source("")    ## moze jeszcze cos?

## ---------------------------------------------------------------------------------------------------------------------•°



## ---------------------------------------------------------------------------------------------------------------------•°
## SIMULATION
doo2pdf = function(N=30,ops=9,groups_per_op=5){
   aa.df = data.frame(id_operator=numeric(0),group=character(0))
   for(k in 1:ops){
      aa.df = rbind(  aa.df ,
        data.frame(
             id_operator = rep(k,N)
          ,  group = sample( sample(letters[1:ops],groups_per_op) , N , replace=TRUE )
        )
      )
   }
   aa.df
}

aa.df = doo2pdf(N=50,ops=9,groups_per_op=5)
aa.df = cbind(   id_case = sample(100:999,dim(aa.df)[1]) , aa.df , weight = sample(seq(0,5,.5),dim(aa.df)[1],replace=TRUE) )
   #  aa.df
   #
## ---------------------------------------------------------------------------------------------------------------------•°
## PRE-CALCULATIONS
   #
tolerance = 2
   #
   #   with(aa.df,addmargins(table(id_operator,feature_a)))
tab = with( aa.df , { tab <- tapply(weight,list(id_operator,group),sum) ; tab <- ifelse(is.na(tab),0,tab) })
    tab = tab[ , names(gr.presence <- sort(apply(tab,2,function(x){sum(x>0)}),"decreasing"=TRUE))]     #!X!
( tab = tab[ names(op.weights <- sort(apply(tab,1,function(x){sum(x)}),"decreasing"=TRUE)) , ] )
      #
   ( tab.lumps.bin = (tab<0) ) ## shortcut to get only FALSEs #!#! this table will be UPDATED BELOW
   ( tab.lumps = tab*0 )    ## shortcut to get only 0s #!#! this table will be UPDATED BELOW
      #
( op.weights = op.weights - round(2*mean(op.weights))/2 )   ## OK!!!
   # (weight.mean = round(sum(aa.df$weight)/length(unique(aa.df$id_operator)),1))                       #!X!
   gr.presence                                                                                          #!X!
( op.weights.up = op.weights[op.weights>tolerance] )
( op.weights.down = rev(-op.weights[op.weights < -tolerance]) )



## ---------------------------------------------------------------------------------------------------------------------•°
## THE MAIN LOOP
while( length(op.weights.up)>0 & length(op.weights.down)>0 ){  ##  1. LOOP_1  ##

   ( tab.bin = tab > 0 )    ## which gr.op (group by operator -- groupo-operator) is present;
                            ## will be actualised with FALSEs in order to exclude further gr.ops
                            ## e.g. when gr.op contains some fat case for which there is no room anywhere
                            ## see (1) below

   ##( tab.bin = tab.bin & tab.register )   #!#!#! NO!!!

   ## upper operator i.e. with  largest amount of cases over the mean
   ( op.up = op.weights.up[1] )          ## overload of the most overloaded operator
    ( op.up.row = tab[names(op.up),] )
     ( op.up.row = sort(op.up.row[ op.up.row>0 & !tab.lumps.bin[names(op.up),] ]) )

   ################################################################################################•°
   mode.small <- mode.big <- FALSE
   ##########################
   ## mode.small  <= op.up   ##  We try first to work in a  mode.small
   ( op.up.row.ok = op.up.row[ op.up.row <= op.up + tolerance ] )
   if(length(op.up.row.ok)>0){
       ( op.up.tab.bin.down.ok  = tab.bin[ names(op.weights.down) , names(op.up.row.ok) , drop = FALSE] )    ## never empty!   because of while() conditions
         #
       ## groups of which the whole amount may be moved from op.up (however, only one of them at time)
       ( op.up.row.ok.bin = op.up.row.ok < apply( op.up.tab.bin.down.ok , 2 , FUN = function(x){ sum(op.weights.down[x])} ) + tolerance )
       #!#!#!
       mode.small = any( op.up.row.ok.bin )
   }

   ################################################################################################•°
   if( mode.small ){   ##  2. IF_1  ##

      ## which group to move - the first == the smallest one (they were ordered already) ; we'd like to move it entirely...
      ( gr.up = op.up.row.ok[op.up.row.ok.bin][1] )

      ## which operators whith what capacities are available
      ( capacities = op.weights.down[op.up.tab.bin.down.ok[,names(gr.up)]] )
      ( ops.tofill = fill.from.left( capacities + round(2*(1+tolerance/length(capacities)))/2 , gr.up ) )   ## what weight should be thrown down to which operators <= theirs capacities

      ##########################################################################•°
      spread.method( force.lumps = FALSE )
      ##########################################################################•°

   ################################################################################################•°
   }else{ ##  if(!mode.small)#!
      warning("There is no possibility to move entire group... ")
   ##########################       ## ... then we try a mode.big
   ## mode.big   > op.up
      ( op.up.row.ok = op.up.row[ op.up.row > op.up + tolerance ] )
      if( length(op.up.row.ok)>0 ){
          ( op.up.tab.bin.down.ok  = tab.bin[ names(op.weights.down) , names(op.up.row.ok) , drop = FALSE] )    ## never empty
            #
                  ## groups of which the anything may be moved from op.up into operators having the same group in service
          ( op.up.row.ok.bin = apply( op.up.tab.bin.down.ok , 2 , any )  )
          #!#!#!
          mode.big = any( op.up.row.ok.bin )
      }

      if(!mode.big){
         op.up.row.ok = op.up.row
         ( op.up.tab.bin.down.ok  = tab.bin[ names(op.weights.down) , names(op.up.row.ok) , drop = FALSE] )    ## never empty
         ( op.up.row.ok.bin = !apply( op.up.tab.bin.down.ok , 2 , any )  )  ## groups absent in down operators
         op.up.tab.bin.down.ok[,op.up.row.ok.bin] <- TRUE                   ## we assume as if they were present for all down operators
          #!#!#!
          mode.big = FALSE  ## to assure -- it is already FALSE if got to this point !
      }

      ##########################################################################•°
      ## CHOOSING THE GROUP TO BE SPREAD
      ## A. with the smallest fat cases
      ##    or
      ## B. the biggest
      ## we take the biggest group (unlike in mode.small where we take the smallest group)
      if(mode.big){
            gr.up = rev(op.up.row.ok[op.up.row.ok.bin])[1]
      }else{
            gr.up = rev(op.up.row.ok)[1]
      }
      op.weights.down.ok = op.weights.down[op.up.tab.bin.down.ok[,names(gr.up)]]
      ( capacities = c( gr.up - min(op.up,sum(op.weights.down.ok))  ## > 0   always!  by definition if mode.big
                                                                    ## may be <= 0    otherwise
                        , op.weights.down.ok
                       ) )
        names(capacities)[1] = names(op.up)       ## we adjoin the source operator (from which we'd like to throw cases off)
            capacities = capacities[capacities>0]
      ( ops.tofill = rev(sort(fill.from.left( capacities + round(2*(1+tolerance/length(capacities)))/2, gr.up ))) )   ## what weight should be thrown to which operators <= theirs capacities
      ##########################################################################•°

      ##########################################################################•°
      spread.method( force.lumps = TRUE )
      ##########################################################################•°


   #   }
   #   else{
   #      tab.register[names(op.up),names(op.up.row.ok.bin)] <- FALSE
   }  ##  END OF  2. IF_1  ##
   ################################################################################################•°


   ## recalculating  tab
      ( tab = with( aa.df , { tab <- tapply(weight,list(id_operator,group),sum) ; tab <- ifelse(is.na(tab),0,tab) }) )
      ( tab = tab[ , names(gr.presence <- sort(apply(tab,2,function(x){sum(x>0)}),"decreasing"=TRUE))] )   #!X!
   ( tab = tab[ names(op.weights <- sort(apply(tab,1,function(x){sum(x)}),"decreasing"=TRUE)) , ] )
      #  tab
   ( op.weights = op.weights - round(2*mean(op.weights))/2 )   ## OK!!!
      # (weight.mean = round(sum(aa.df$weight)/length(unique(aa.df$id_operator)),1))                       #!X!
      gr.presence                                                                                          #!X!
   ( op.weights.up = op.weights[op.weights>tolerance] )
   ( op.weights.down = rev(-op.weights[op.weights < -tolerance]) )

}  ##  END OF  1. LOOP_1  ##
## ---------------------------------------------------------------------------------------------------------------------•°





## ---------------------------------------------------------------------------------------------------------------------•°
## ---------------------------------------------------------------------------------------------------------------------•°
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
