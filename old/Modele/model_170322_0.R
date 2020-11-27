########################################################################################################################—•°
*## DZIA£ ANALIZ -- ANALIZY -- MODELE STATYSTYCZNE -- A.K. -- 150202
########################################################################################################################—•°
  rm(list=ls()) ; ID = list()      ## czyszczenie przestrzeni roboczej
########################################################################################################################—•°
#! PROGNOZA ############################################################################################################—•°
########################################################################################################################—•°
## CEL:
##
## MODEL:
##
########################################################################################################################—•°
ID$. = "170322"   ## takie samo jak nazwa katalogu --  w przyblizeniu data powstania;
#####DATEprediction = as.POSIXct("2016-11-07") ## data prognozy, zazwyczaj taka jak wy¿ej
## ENTITY  = ""
ID$path = "170322"
##
ID$table = "170322"  # raporty.referencja_do_przypisu_dnt3
##
##
ID$model = "170322"                ## takie samo jak nazwa katalogu -- w przybli¿eniu data powstania;    ##  {"Wysy³ka do komornika"}
ID$dataref = "150223"						  ## katalog z danymi referencyjnymi na których by³ zudowany model
##
METHOD = "score"
#prediction.name = "score"              ## np: "score" w przypadku scoringu ; "prognoza"  dla innych metod


#
# Data otwarcia:
#[1] "2017-03-22 14:19:32 CET"
#
# Zmienne nazw tabel:
#                   TABLE
TABLE$selection =  "modele.prognoza_170322_selekcja"
   TABLE$variables =  "raporty.referencja_do_przypisu_dnt3"
TABLE$results =    "modele.prognoza_170322_wyniki"
#
# Zmienne nazw plików:
#                          FILE$model
FILE$model$environment =  "model_170322_environment.Rdata"
FILE$model$Rdata =        "model_170322.Rdata"
#
#                             FILE$pred
FILE$pred$environment =      "prognoza_170322_environment.Rdata"
FILE$pred$variables.Rdata =  "prognoza_170322_zmienne.Rdata"
FILE$pred$variables.sql =    "prognoza_170322_zmienne.sql"
FILE$pred$variables.xls =    "prognoza_170322_zmienne.xls"
FILE$pred$variables.csv =    "prognoza_170322_zmienne.csv"
FILE$pred$results.Rdata =    "prognoza_170322_wyniki.Rdata"
FILE$pred$results.csv =      "prognoza_170322_wyniki.csv"
FILE$pred$results.xls =      "prognoza_170322_wyniki.xls"
FILE$pred$outliers.csv =     "prognoza_170322_outliers.csv"
#
#              FILE$packs
FILE$packs =  "PacksAK.R"
#
# Zmienne nazw katalogów:
#                   PATH
#PATH$R =           "//10.1.1.5/PK_Analitycy/R"
#PATH$packs_dir =   "//10.1.1.5/PK_Analitycy/R/PacksAK"
#PATH$modfun =      "//10.1.1.5/PK_Analitycy/Modele/Funkcje"
#PATH$model =       "//10.1.1.5/PK_Analitycy/Modele/Modele/170322"
#PATH$prediction =  "//10.1.1.5/PK_Analitycy/Modele/Prognozy/170322"
PATH$wd =          "//10.1.1.5/PK_Analitycy/Modele/Modele/170322"
#
#
# Aktywnoœæ:
#[1] open  write
<0 wierszy> (lub 'row.names' o zerowej d³ugoœci)


########################################################################################################################—•°

########################################################################################################################—•°
## 0. Do poprawnego dzialania TinnR (powinno ladowac siê samo - trzeba naprawic jakies .ini)
#  .trPaths <- paste(paste(Sys.getenv('APPDATA'), '\\Tinn-R\\tmp\\', sep=''),
#            c('', 'search.txt', 'objects.txt', 'file.r', 'selection.r', 'block.r', 'lines.r'), sep='')
########################################################################################################################—•°


########################################################################################################################—•°
#! I. KATALOGI, ZMIENNE, PAKIETY, SKRYPTY, £ADOWANIE DANYCH i MODELI ###################################################—•°
########################################################################################################################—•°


########################################################################################################################—•°
## 1. KATALOGI I PLIKI #################################################################################################—•°

## a. KATALOGI #########################################################################################################—•°
#ROOT = "D:/ROBOCZY/PK_Analitycy"
ROOT = "//10.1.1.5/PK_Analitycy" ;
DOMAIN = paste(ROOT,"Modele",sep="/")
PACKS = "PacksAK"
source( paste0( DOMAIN, "/Funkcje/initiate_prediction.r" ) )



## b. PLIKI ############################################################################################################—•°
initiate_prediction()  ## tworzy zmienne nazw sciezek, plików i tabel
setwd(PATH$wd)

## c. ŒRODOWISKO #######################################################################################################—•°
## Na wypadek gdyby trzeba by³o przerwaæ / wznowiæ pracê prze ukoñczeniem pliku.

     #!###########################################################################
     ## Gdyby trzeba by³o przerwaæ pracê wróæ do tego miejsca i zapisz œrodowisko:
     # save.prediction()  ## save.image(file=FILE$pred.environment , compress="xz")	## NIE NADPISZ BEZMYŒLNIE!!!
     ## lub za³aduj œrodowisko gdy wznawiasz pracê
       load(file=FILE$pred$environment) ; detach("package:TinnRcom", unload=TRUE) ; library(TinnRcom) ## unloadNamespace("TinnRcom")  may work too
     #!###########################################################################

.GlobalEnv

########################################################################################################################—•°
#! 2. UWAGI WSTÊPNE ####################################################################################################—•°
########################################################################################################################—•°
##   A. PODSTAWOWYM ZADANIEM niniejszego pliku jest obliczenie prognozy na podstawie nowych danych i modelu:
##    1. pobranie danych z bazy do data.new
##    2. wczytanie modelu
##    3. uzgadnianie zmiennych tzn. obróbka data.new wg wytycznych z modelu:
##       transformacje i ograniczenie zakresu zmiennych tak jak danych referencyjnych
##    4. zapis przetworzonych danych do Data.new
##    5. prognoza, przegl¹d wyników, zapis do plików.
##
########################################################################################################################—•°
## B.
##    1. Niektóre komendy s¹ zakomentowane jako potencjalnie niebezpieczne.
##       Wywo³ujemy je stawiaj¹c kursor po znaku komentarza i klikajac "play" na pasku.
##       Czynimy to po upewnieniu siê, ¿e uruchomienie danej komendy jest dok³adnie tym czego w³aœnie chcemy.
##    2. Komendy niebezpieczne to te, ktore uruchomione nieprawid³owo bêd¹ trudne do korekty
##       i konieczne bêdzie przeprowadzenie od nowa niemal ca³ej analizy.
##    3. SKRÓTY:
##       • E0 - etap 0 tj. ³adowanie danych z bazy i ich wstêpna obróbka (eliminacja b³êdów; bez metod statystycznych)
##       • E1 - etap 1 tj. w³asciwa obróbka danych:
##              - eliminacja obserwacji odstajacych i przekszta³cenia zmiennych;
##              - uzupelnianie NULLi z u¿yciem metod statystycznych, np. RF;
##       • E2 - etap 2 tj. budowa modelu
##       • E3 - etap 3 tj. prognoza w oparciu o model z E2.
##    4. Podstawowe ramki danych:
##       • E0 - data.raw0 --> data.raw --> Data.raw
##       • E1 - Data.raw --> data.ref --> Data.ref
##       • E2 - Data.ref, Data.ref_y (zmienne Y, mog¹ ju byc w Data.ref)
##       • E3 - Data.ref;    data.new0 --> data.new --> Data.new --> Prediction.df
##
########################################################################################################################—•°

########################################################################################################################—•°
## 3. MODEL I PROGNOZA #################################################################################################—•°

#####################################################
## a. jeœli nowa prognoza ³adujemy model



########################################################################################################################—•°
## 4. PAKIETY I SKRYPTY ################################################################################################—•°

## PAKIETY
packages = c( "RODBC",'RPostgreSQL', #"vioplot", #"tree",
              "randomForest",#"car",
              "ROCR",
              "gam","rattle","igraph",
              "stats","graphics","grDevices",
              "datasets","methods","base",
              #"caret",
               "gdata","rpart" )

library("RODBC")
require("RODBC")

## Instalacja pakietów jeœli jeszcze nie zainstalowane
#  chooseCRANmirror()
#  for(x in packages)install.packages(x)

#! Pakiety nie ³aduj¹ siê wraz z przestrzeni¹ robocz¹!
#! £adowanie pakietów - konieczne!
l = quote(library(a))
for(p in packages){ l[[2]] = as.name(p);  eval(l) }    #!!! YEAH!!!
  #!  l; as.list(l)   ## check it !!!
## for(p in packages){library(p,character.only = FALSE)}  ## simpler but less sexy :)

   search()

###############################################################################—•°
## b. PACKs ###################################################################—•°
## proto-packages -- packages under construction, former FunctionsAK organised in bundels of common themes,
##          fully-fledged packages in the near future;
source(paste(PATH$packs,FILE$packs,sep="/"))

   ## now you may re-load any of the packs , e.g.
   pack = "Graphics"
   loadPacksAK(pack)
   ##PATHwd = paste(PATH$packs_dir,pack,sep="/") ; setwd(PATHwd)

### SKRYPTY 1  ## funkcje nowe by AK
#  setwd(paste0(ROOT,"/R/FunctionsAK"));    source("sourceFunctionsAK.r");  setwd(PATH$wd)
#
### SKRYPTY 2  ## funkcje stare (do scoringu)
#setwd(paste0(ROOT,"/R/FunctionsScoring"));   source("sourceFunctionsScoring.r");   setwd(PATH$wd)
#  # source("wszystkie_funkcje_kompilacja.r")
#
### SKRYPTY 3  ## mo¿e jeszcze coœ?
#  #source("")


###############################################################################—•°
## B. £ADOWANIE DANYCH

#####################################################—•°
## z BD
# ch = odbcConnect("PostgreSQL35W",uid="akasprzyk",pwd="akasprzyk") ## IKTORN

ch = odbcConnect("PostgreSQL35W3",uid="arkadiusz.kasprzyk",pwd="akasprzyk123")   ## TOUDI
    #! UWAGA: nie nadpisz danych!!!
    TABLE$variables
  data.new0 = sqlQuery(ch,paste0("SELECT * FROM ",TABLE$variables))       ##
	dim(data.new0)    ## 83585    67
odbcClose(ch)

         idx0 <- sample(nrow(data.new0))
    data.new0 <- data.new0[idx0,]
    data.new  <-  data.new[idx0,]

summary(data.new0)
nulls0.table(data.new0)

   head(data.new0["rka_d_przypisu"],30)
class(data.new0["rka_d_przypisu"])      # dataq.frame
   dim(data.new0["rka_d_przypisu"])
nulls0.table( data.new0[ data.new0["rka_d_przypisu"] =="R1" ,] )
class(data.new0["rka_d_przypisu"] == "R1")
   dim(data.new0["rka_d_przypisu"] == "R1")

   head(data.new0[["rka_d_przypisu"]],30)
class(data.new0[["rka_d_przypisu"]])
   dim(data.new0[["rka_d_przypisu"]])
nulls0.table( data.new0[ data.new0[["rka_d_przypisu"]] =="R1" ,] )
   dim(data.new0[["rka_d_przypisu"]] == "R1")

   head(data.new0$rka_d_przypisu,30)
class(data.new0$rka_d_przypisu)
nulls0.table( data.new0[ data.new0$rka_d_przypisu     =="R1" ,] )

   head(data.new0[,"rka_d_przypisu"],30)
class(data.new0[,"rka_d_przypisu"])
nulls0.table( data.new0[ data.new0[,"rka_d_przypisu"] =="R1" ,] )


idx.r1 <-  data.new0$rka_d_przypisu  =="R1"
idx.r1dm <- with( data.new0 ,
   {
      idx <- rka_d_przypisu  =="R1"  &  odpowiedzialnosc == "DETAL - MASS"
      print(table(czy_wizyta_w_przypisie[idx]))
      idx <- idx  &  czy_wizyta_w_przypisie == 1
         barplot(table(idx))
      idx
   }
)
nulls0.table( data.new0[ idx.r1dm ,] )

with( data.new0[ idx.r1dm ,] , {
   print( tt <- table(liczba_przypisow_przed ) )
   barplot(tt)
})




