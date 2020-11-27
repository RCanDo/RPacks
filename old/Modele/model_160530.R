########################################################################################################################•°
*## DZIA£ ANALIZ ## ANALIZY ## MODELE STATYSTYCZNE ## A.K. ## 160530
########################################################################################################################•°
  rm(list=ls())     ## czyszczenie przestrzeni roboczej
########################################################################################################################•°
#! MODEL ###############################################################################################################•°
########################################################################################################################•°
## CEL 0: model na przerwane wplaty
##
## OPIS: referencja to
##
## DOCELOWY MODEL:
##		- scoring na wplaty
##
#################################################################
########################################################################################################################•°
ID = "160530"   ## takie samo jak nazwa katalogu ##  w przybli¿eniu data powstania;
DATEmodel = as.POSIXct("2016-05-30") ## data modelu
ENTITY = ""
##
METHOD = "gam"
prediction.name = "score"            ## np: "score" w przypadku scoringu ; "prognoza"  dla innych metod
##
IDdataref = "160104"						 ## katalog z danymi referencyjnymi na których by³ zudowany model
IDref = "1"
Ysuffix = ""      		 ## przyrostek nazwy tabeli ze zmienn¹ objaœnian¹ ## TYLKO JEDEN W CA£YM PLIKU !!!  inny Y = inny model = inny plik i katalog modelu
##
##  BD :  toudi.modele.
##
##    Utworzono zmienne nazw tabel:
##                  TABLE
##   TABLEdataref   "modele.referencja_160104"
##   TABLEdataref_Y "modele.referencja_160104_"
##   TABLEmodel     "modele.referencja_160104_model_160530"
##
##    Utworzono zmienne nazw plików:
##                         FILE
##   FILEdataref.Rdata     "referencja_160104_1.Rdata"
##   FILEmodel_Y           "model_160530_Y.sql"
##   FILEmodel.environment "model_160530_environment.Rdata"
##   FILEmodel.Rdata       "model_160530.Rdata"
##   FILEmodel.sql         "model_160530.sql"
##   FILEmodel.xls         "model_160530.xls"
##   FILEmodel.csv         "model_160530.csv"
##
##    Utworzono zmienne nazw katalogów:
##               PATH
##   PATHdataref "//10.1.1.5/PK_Analitycy/Modele/Dane Referencyjne/160104"
##   PATHmodel   "//10.1.1.5/PK_Analitycy/Modele/Modele/160530"
##   PATHwd      "//10.1.1.5/PK_Analitycy/Modele/Modele/160530"
##
########################################################################################################################•°


########################################################################################################################•°
#! I. KATALOGI, ZMIENNE, PAKIETY I SKRYPTY #############################################################################•°
########################################################################################################################•°

########################################################################################################################•°
## 0. Do poprawnego dzialania TinnR (powinno ladowac siê samo - trzeba naprawic jakies .ini)
#  .trPaths <- paste(paste(Sys.getenv('APPDATA'), '\\Tinn-R\\tmp\\', sep=''),
#            c('', 'search.txt', 'objects.txt', 'file.r', 'selection.r', 'block.r', 'lines.r'), sep='')
########################################################################################################################•°

########################################################################################################################•°
#! I. KATALOGI, ZMIENNE, PAKIETY, SKRYPTY, £ADOWANIE DANYCH i MODELI ###################################################•°
########################################################################################################################•°


########################################################################################################################•°
## 1. KATALOGI I PLIKI #################################################################################################•°

## a. KATALOGI #########################################################################################################•°
#ROOT = "D:/ROBOCZY/PK_Analitycy"
ROOT = "//10.1.1.5/PK_Analitycy"
DOMAIN = paste(ROOT,"Modele",sep="/")
  source( paste0( DOMAIN, "/Funkcje/funkcje_modele.r" ) )
PATHdata = paste(DOMAIN,"Dane Referencyjne",IDdataref,sep="/")
PATHmodel = paste(DOMAIN,"Modele",ID,sep="/")
PATHwd = PATHmodel
setwd(PATHwd)

         ## Ladowanie przestrzeni roboczej (o ile jest co ladowac)
         #load( paste0( PATHwd, "{}.RData" ) )  #!!


## b. PLIKI ############################################################################################################•°

fun.names.model()  ## tworzy zmienne nazw plików, katalogów, tabel
  #ls()


## c. ŒRODOWISKO #######################################################################################################•°
## Na wypadek gdyby trzeba by³o przerwaæ / wznowiæ pracê prze ukoñczeniem pliku.

        #!###########################################################################
        ## Gdyby trzeba bylo przerwac prace wróc do tego miejsca i zapisz œrodowisko:
        #!   save.image(file=FILEmodel.environment , compress="xz")				## NIE NADPISZ BEZMYŒLNIE!!!
        ## lub zaladuj srodowisko gdy wznawiasz prace
         load(file=FILEmodel.environment) ; detach("package:TinnRcom", unload=TRUE) ; library(TinnRcom) ## unloadNamespace("TinnRcom")  may work too
        #!###########################################################################
        ## tylko modele (i dane)
        #!   load(file=FILEmodel.Rdata)


########################################################################################################################•°
#! 2. UWAGI WSTEPNE ####################################################################################################•°
########################################################################################################################•°
## A. PODSTAWOWYM ZADANIEM niniejszego pliku jest zbudowanie MODELU na danych z pliku  FILEdataref.Rdata (ale NIE  FILEdataref.environment !!!)
##    Dane FILEdataref.Rdata s¹ w 100% gotowe do analizy statystycznej: badania korelacji, wykluczania zmiennych, budowy modelu.
##    1. pobranie danych  Data.ref , Data.ref_y  z  FILEdataref.Rdata
##    2. badanie korelacji
##    3. wykluczanie zmiennych
##    4. ...
##    5. stabilny model (byc moze wiele wersji)
##
########################################################################################################################•°
## B.
##    1. Niektóre komendy sa zakomentowane jako potencjalnie niebezpieczne.
##       Wywo³ujemy je stawiaj¹c kursor po znaku komentarza i klikajac "play" na pasku.
##       Czynimy to po upewnieniu siê, ¿e uruchomienie danej komendy
##       jest dok³adnie tym czego w³aœnie chcemy.
##    2. Komendy niebezpieczne to te, ktore uruchomione nieprawid³owo bêd¹ trudne do korekty
##       i konieczne bêdzie przeprowadzenie od nowa niemal ca³ej analizy.
##    3. SKRÓTY:
##       • E0 - etap 0 tj. ³adowanie danych z bazy i ich wstêpna obróbka (eliminacja b³êdów; bez metod statystycznych);
##       • E1 - etap 1 tj. w³aœciwa obróbka danych (eliminacja obserwacji odstaj¹cych; z metodami statystycznych, np. uzupe³nianie NULLi z u¿yciem RF);
##       • E2 - etap 2 tj. budowa modelu;
##       • E3 - etap 3 tj. prognoza w oparciu o model z E2.
##    4. Podstawowe ramki danych:
##       • E0 - data.raw0 --> data.raw --> Data.raw
##       • E1 - Data.raw --> data.ref --> Data.ref
##       • E2 - Data.ref, Data.ref_y (zmienne Y, mog¹ ju byc w Data.ref)
##       • E3 - Data.ref;    data.new0 --> data.new --> Data.new --> Prediction.df
##
########################################################################################################################•°

########################################################################################################################•°
## 3. PAKIETY I SKRYPTY ################################################################################################•°

## PAKIETY
packages = c( "MASS",
			  	  "RPostgreSQL","RODBC"#,"vioplot"
                , "rpart",# "earth" ,
              "randomForest",#"car",
              "ROCR","RODBC","igraph",
              "gam","rattle","igraph",
              "stats","graphics","grDevices",
              "datasets","methods","base"
              #,"caret"
              #,"gdata"#,"sqldf"
              #,"data.table"
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
  setwd(paste0(ROOT,"/R/FunctionsScoring"));   source("sourceFunctionsScoring.r");   setwd(PATHwd)
  # source("wszystkie_funkcje_kompilacja.r")

## SKRYPTY 2  ## ladowanie dodatkowych skryptów
  setwd(paste0(ROOT,"/R/FunctionsAK"));    source("sourceFunctionsAK.r");  setwd(PATHwd)

## SKRYPTY 3  ## ladowanie dodatkowych skryptów
  #source("")    ## moze jeszcze cos?


########################################################################################################################•°
## 4. DANE I OBIEKTY ###################################################################################################•°
## Pomijamy jeœli wczeœniej za³adawaliœmy œrodowisko w linii 90 (oko³o), rozdzia³ 1.c

###########################################################•°
## A. OBIEKTY WYMAGANE - bed¹ póŸniej zapisane i przekazane do E2

OBJECTS = c(  ## z E1
              "Data.ref" , "Data.ref.nas" , "data.ref"   #, "data.ref.nas" , "data.ref.outs"
            ##
				, "VARid" , "VARdebt" , "VARY"
            ##
            , "variables"  ## wszystkie nazwy w jednej liœcie, której elemanetami s¹ wektory nazw, stare "names._"
            ##
            , "condlists"
            ##
				, "trans"      ## wszystkie obiekty "trans" (transformacje zmiennych) w jednej liœcie;
                           ## informacja o przekszta³ceniach cech : lista przekszta³ceñ i df z t¹ list¹ i innymi statystykami
            , "levels.raw" ## oryginalne wartoœci poziomów dla przekszta³conych danych kategorycznych; zob.
                           #     names(trans$toimprove.list) == names(levels.raw)
            #, ls(pattern='^VAR')
            , "OBJECTSref"
            ## E2
            , "regressors"
            , "summaries" , "summaries.agg"
            , "model.gam.binary" #, "model.gam.poisson"
            ##
            #, DFtrans )   ## informacja o przekszta³ceniach cech : lista przekszta³ceñ i df z t¹ list¹ i innymi statystykami
            )

#! Mo¿na w trakcie prac dodaæ inne obiekty do zapamiêtania
#  OBJECTS = union(OBJECTS,"object.xyz")


###############################################################################•°
## B. DANE REFERENCYJNE  ######################################################•°

## pierwsze ³adowanie danych referencyjnych z	DOMAIN/Dane Referencyjne/YYMMDD

## POBRANIE danych z .Rdata po E0 i E1
load(file = paste(PATHdataref,FILEdataref.Rdata,sep="/") )

ls(pattern='^(D|d)ata')   ## budujemy model na Data.ref;
   ##  data.ref do wgl¹du -- zawiera zmienna po etapie E0 - nieprzeksztalcone i z obs. odstaj¹cymi
   ## mog¹ slu¿yæ jako Ÿródlo dadatkowych danych w razie naglej potrzeby
   dim(data.ref)
   dim(Data.ref)   ## 48741   154
   (nt.all = nulls0.table(Data.ref))


###############################################################################•°
## C. PODSTAWOWE OBIEKTY -- przegl¹d  #########################################•°

         #!#!#!#! DO WYCIÊCIA !!!!
            load(file = paste(PATHdataref,"referencja_160104_1_appx.Rdata",sep="/") )
               names(variables.e1)
               names(condlists.e1)
               names(trans.e1)
           variables$ref0 = variables.e1$ref0
           variables$ref1 = variables.e1[setdiff(names(variables.e1),"ref0")]
                 rm("variables.e1","condlists.e1","trans.e1")

            names(variables)
            cat(names(variables),sep="\" , \"")
            variables.e2 = variables[c( "dates" , "full" , "id" , "notregressors" , "plot" , "poor" , "raw" , "ref"
                                       , "regressors" , "regressors_poor" , "remove" , "replacing" , "rm_na_0" , "tochange"
                                       , "tofill" , "toimprove" , "tosee" , "newmod" , "saldo" , "dt" , "dp" , "czy" , "kat"
                                       , "kwota" , "ile" , "factor" , "wplaty" , "skutecznosc" )]
            variables = variables[c("ref0" , "ref1" )]
            names(variables.e2)

            variables = variables0
            names(variables)
            ###########################
            condlists.e2  = condlists
               condlists = condlists.e1
            names(condlists.e2)
            ###########################

            ###########################
            rm(variables.e2, condlists.e2)
            rm(variables.e1, condlists.e1)
         ###########################

   names(variables)
variables$ref1 = variables[setdiff(names(variables),"ref0")]
variables = variables[c("ref0" , "ref1" , "ref", "id" , "dates" #, "notregressors" , "regressors_poor" , "full"
                        , "regressors"  )]

   names(condlists)
condlists$ref1 = condlists[setdiff(names(condlists),"ref0")]
condlists = condlists[ c( "ref0" , "ref1" ) ]


   variables$ref = names(Data.ref)
   (nt.regs = nulls0.table(Data.ref[,variables$regressors]))  ## 0 NA -- OK!!!


   ###########################################################•°
   ## trans

   names(trans)
   lapply(trans,class)
   levels.raw
      any(names(trans$toimprove.list) !=  names(levels.raw))   #! MUST BE FALSE


###########################################################•°
## VAR
## jakie zmienne s¹ potrzebne okazuje siê czêsto dopiero w trakcie budowy modelu;
## tutaj tylko lista najistotniejszych zmiennych, którym nadajemy VARznaczenie;
## ich tworzenie w paragrafie  6. ZMIENNE POCHODNE;
##

ls(pattern='^VAR')
cbind(unlist(mget(ls(pattern='^VAR'))))


VARdebt                "saldo_stop_0_k"          #! to jest logx(saldo_stop_0_k,.1)
VARdebt0               "saldo_stop_0_k.1"        #! to jest oryginalne  saldo_stop_0_k
VARdebt1               "saldo_stop_0_k.1"
VARefficiency          "skutecznosc"
VARefficiency_binomial "skutecznosc_binomial"
VARefficiency_groups   "skutecznosc_grupy"
VARid                  "id_produkt_k"
#########
VARY    = "czy_wplata_60"
   class(Data.ref[,VARY]) ; table(Data.ref[,VARY]) ;   table(as.numeric(Data.ref[,VARY]))
   #!#   Data.ref[,VARY] = as.numeric(Data.ref[,VARY]) - 1   ## czynimy to poni¿ej w par. 6 -- tutaj dla zwrócenia uwagi
#########
VARpays                "kwota_wplata"            #! to jest  log( kwota_wplata + 1 )
VARpays0               "kwota_wplata.0"          #! to jest oryginalna kwota_wplata z E0 -- jaka wplata (bez przeksztalceñ) jeœli byla po conajmniej 30 dniach przerwy (jesli brak wplaty to 0)
VARpays1               "kwota_wplata.1"          #! == kwota_wplata.0 * czy_wplata_60
VARY1                  "skutecznosc"
VARY1groups            "skutecznosc_grupy"       ## do utworzenia

OBJECTS = union(OBJECTS,ls(pattern='^VAR'))

###############################################################################•°
## D. POPRAWKI do danych z BD  ## oby jak najmniej!  ##########################•°


###########################################################•°
## DANE REFERENCYJNE Y ## zazwyczaj wp³aty w zadanym okresie

   ## TUTAJ NIEPOTRZEBNE - y jest w Data.ref

   ## pierwsze ³adowanie danych referencyjnych z	DOMAIN/Dane Referencyjne/YYMMDD
   ch <- odbcConnect("PostgreSQL35W", uid = "arkadiusz.kasprzyk", pwd = "akasprzyk123") ## TOUDI

      ( query = paste("SELECT * FROM ", TABLEdataref_Y , sep = " ") )
       data.ref_y <- sqlQuery(ch, query,  errors = TRUE)
        dim(data.ref_y)    ##

   #  dpd_k <- sqlQuery(ch, paste0("SELECT id_produkt , dpd_k from akasprzyk.",TABELA))
   #  dpd_sad <- sqlQuery(ch, paste0("SELECT id_produkt , dpd_sad from akasprzyk.",TABELA))
   #  dpd_kom <- sqlQuery(ch, paste0("SELECT id_produkt , dpd_kom from akasprzyk.",TABELA))
   #
   odbcClose(ch)

###########################################################•°
## INNE zmienne z BD

	#  ch <- odbcConnect("PostgreSQL35W2", uid = "akasprzyk", pwd = "akasprzyk") ## IKTORN
	#  ch <- odbcConnect("PostgreSQL35W3", uid = "arkadiusz.kasprzyk", pwd = "akasprzyk123") ## TOUDI
	#  ...



###############################################################################•°
## E. £¥CZENIE DANYCH : [ X , Y ] i nie tylko #################################•°
##
## Zanim po³¹czymy X z Y sprawdzamy czy wszystko w porzadku z Y (X jest ju¿ opracowany na etapie "Dane Referencyjne").
   #!
   ## TUTAJ NIEPOTRZEBNE - y jest w Data.ref

   	nulls.table(data.ref_y)
   	# nulls.table(data.ref_y)

   	## NIE USUWAMY NULLi z   data.ref_y   bowiem oznaczaj¹ one ¿e sprawa nie byla tak dlugo w obsludze po s¹dzie
   	##	data.ref_Y[ is.na(data.ref_Y[,"suma_wplat_mo"]), "suma_wplat_mo" ] = 0

   	# table(data.ref_Y[,"suma_wplat_mo"] == 0,useNA = "ifany")

   	#########################################•°
   	## zmienna nazwowa ID -- pozosta³e zmienne nazwowe w p.7.

      VARid

   	#########################################•°

      rownames(data.ref_y) = data.ref_y[,VARid]
   	names.ref_y = names(data.ref_y)

      Data.ref = update.df( Data.ref , data.ref_y , variables = names.ref_y , action = "append" )


########################################################################################################################•°
## 6. ZMIENNE POCHODNE #################################################################################################•°
##    Powinny byc przewidziane w E1 a nawet wczesniej (SQL), jednak nie wszystko da siê przewidziec...
##


#! WA¯NE!!!
#!    condlists$variables_new = list() ; class(condlists$variables_new) = c("condlist","list")
#!    variables$newmod = character(0)


   ###################################################################################################•°
   ## SZABLON, np. ###################################################################################•°
   with(Data.ref, plot.variable(v1/v2) )
   with(Data.ref, { windows();coldef.black(); plot(v2,v1)} )

condlists$variables_new[["vx"]] = list(  ## zob. pomoc w condapply()  (condapply.r)
                                 conditions = quote("all")
                               , inputs = c("v1","v2","..")
                               , substitute = quote( f(a,b,..) )
                               , plot = TRUE
                               )

   vx = condapply( Data.ref , condlists$variables_new["vx"] , return.all = FALSE )

      with(datfram, plot.variable(vx) )
      with(datfram, plot.variable(vx[vx < 10]) )

   #!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
   ## jeœli OK (po skonstruowaniu wszystkich dodatkowych zmiennych)
   Data.ref = condapply( Data.ref , condlists$variables_new ) ; rm(vx)
   #!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
   ###################################################################################################•°


###################################################################################################•°
## 6.1 VARdebt ####################################################################################•°

VARdebt = "saldo_stop_0_k"      #! to jest logx(saldo_stop_0_k,.1)
#!#!#!#!#!#!#!#!#!#
Data.ref = update.df(Data.ref,data.ref,id=0,variable="saldo_stop_0_k",action="append")
#!#!#!#!#!#!#!#!#!#
   VARdebt
   VARdebt0 <- VARdebt1 <- "saldo_stop_0_k.1"    #! to jest oryginalne  saldo_stop_0_k
      plot.variable(Data.ref[,VARdebt1])   ## OK

###################################################################################################•°
## 6.2 VARY jako numeric ##########################################################################•°

VARY    = "czy_wplata_60"
   class(Data.ref[,VARY])
   table(Data.ref[,VARY])
   table(as.numeric(Data.ref[,VARY]))

## mo¿na prosto:
##   Data.ref[,VARY] = as.numeric(Data.ref[,VARY]) - 1
## ale CHCEMY mieæ wszystkie transformacje w liœcie .condlist
condlists$variables_new[[VARY]] = list(  ## zob. pomoc w condapply()  (condapply.r)
                                 conditions = quote("all")
                               , substitute = quote( as.numeric(x) - 1 )
                               )

   vx = condapply( Data.ref , condlists$variables_new[VARY] , return.all = FALSE )
   plot.variable(vx,as.factor=TRUE)

###################################################################################################•°
## 6.3 VARpays , VARpays0, VARpays1 ###############################################################•°

#########
#VARid2  = "id_produkt_glowny" ;   VARid1 = VARid2
VARpays = "kwota_wplata"
      plot.variable(Data.ref[,VARpays])    #! to jest  log( kwota_wplata + 1 )
      plot.variable(data.ref[,VARpays])    #! to jest  log( kwota_wplata + 1 )
      addmargins(table(data.ref[,VARpays]>0,data.ref[,VARY]) )   #!
   #
   VARpays0 <- "kwota_wplata.0"    #! to jest oryginalna kwota_wplata z E0 -- jaka wplata (bez przeksztalceñ) jeœli byla po conajmniej 30 dniach przerwy (jesli brak wplaty to 0)
   VARpays1 <- "kwota_wplata.1"    #! == kwota_wplata.0 * czy_wplata_60
#!#!#!#!#!#!#!#!#!#
Data.ref = update.df(Data.ref,var2vec(data.ref,VARpays),id=0,variable=VARpays0,action="replace")#,action="append")
#!#!#!#!#!#!#!#!#!#
   plot.variable(Data.ref[,VARpays0])   ## OK
      addmargins(table(Data.ref[,VARpays]>0,Data.ref[,VARY]) )    #!
      addmargins(table(Data.ref[,VARpays0]>0,Data.ref[,VARY]) )   #!


## mo¿na prosto:  Data.ref[,VARpays1] = Data.ref[,VARpays0]*Data.ref[,VARY]
## ale CHCEMY mieæ .condlist
condlists$variables_new[[VARpays1]] = list(  ## zob. pomoc w condapply()  (condapply.r)
                                 conditions = quote("all")
                               , inputs = c(VARpays0,VARY)
                               , substitute = quote( a*b )
                               , isolate = FALSE    #!#!#!#!#!
                               , plot = TRUE
                               )

   datfram = condapply( Data.ref , condlists$variables_new , return.all = FALSE )

   plot.variable(Data.ref[,VARpays0])

###################################################################################################•°
## 6.4 VARefficiency ##############################################################################•°

VARefficiency = "skutecznosc"
VARY1   = VARefficiency

condlists$variables_new[[VARefficiency]] = list(
                     variables = c()
                   , conditions = quote("all")
                   , inputs = c(VARdebt1,VARpays1)
                   #, substitute = body(function(){ max.debt_pay <- pmax(a,b) ; fails <- max.debt_pay - b ; round(cbind(b,fails)) })
                   , substitute = quote(pmin(b/a,1))
                   , isolate = FALSE
                   , plot = TRUE
      )

   datfram = condapply( Data.ref , condlists$variables_new , return.all = FALSE )

###################################################################################################•°
## 6.5 VARefficiency_binomial #####################################################################•°

VARefficiency_binomial = "skutecznosc_binomial"

condlists$variables_new[[VARefficiency_binomial]] = list(
                     variables = c()
                   , conditions = quote("all")
                   , inputs = c(VARdebt1,VARpays1)
                   #, substitute = body(function(){ max.debt_pay <- pmax(a,b) ; fails <- max.debt_pay - b ; round(cbind(b,fails)) })
                   , substitute = quote({ max.debt_pay <- pmax(a,b) ; fails <- max.debt_pay - b ; round(cbind(b,fails)) })
                   , isolate = FALSE
                   #, plot = TRUE
      )

   datfram = condapply( Data.ref , condlists$variables_new , return.all = FALSE )
   head(datfram,50)

   with(datfram, {   vx <- skutecznosc_binomial ; skutecznosc <- vx[,1] / rowSums(vx)
                     plot.variable(skutecznosc)
                     plot.variable(skutecznosc[skutecznosc<.2])
                  } )

###################################################################################################•°
## 6.6 skutecznosc_grupy ##########################################################################•°

VARY1groups <- VARefficiency_groups <- "skutecznosc_grupy"

condlists$variables_new[[VARefficiency_groups]] = list(
                           variables = c()
                        ,  conditions = "all"
                        ,  inputs = c(VARefficiency)
                        ,  substitute = quote(
                                    cut( a , breaks = monetary(  max(a) , power.range = 3 , base = 1 )#, merge.last = 1 )
                                        , include.lowest = TRUE )
                                    )
                        , isolate = FALSE
                        , plot = TRUE
                        )

datfram = condapply( Data.ref , condlists$variables_new , return.all = FALSE) #, as.factor=TRUE)
        table(datfram[,VARefficiency_groups],useNA="ifany")
vx = datfram[,VARefficiency_groups]

 ( table.number.VARY1groups = as.table(tapply(Data.ref[,VARdebt0],vx,length)) )
 ( table.debt.VARY1groups = as.table(tapply(Data.ref[,VARdebt0],vx,sum)) )
 ( table.pay.VARY1groups = as.table(tapply(datfram[,VARpays1],vx,sum)) )

 windows(height=4,width=12); par.my(); par(mfrow=c(1,2))
 barplot(table.debt.VARY1groups);title("debt");
 barplot(table.pay.VARY1groups);title("payments")

## inaczej
agg.glm = aggregate( as.formula(paste0(VARdebt0,"~",VARY1groups))
				,FUN = function(x){ y = c( sum(x), length(x), max(x), mean(x) , min(x) ) ; names(y) = c("sum","number","max","mean","min") ; y }
 				,data = datfram
				#,simplify = FALSE
				)
format( agg.glm  , scientific = FALSE	 , digits = 6 	 #, nsmall = 1
		)

###################################################################################################•°
## NA KONIEC (po skonstruowaniu wszystkich zmiennych) #############################################•°

#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
## jeœli OK to
cbind(unlist(mget(ls(pattern='^VAR'))))
variables$newmod = setdiff(unlist(mget(ls(pattern='^VAR'))),variables$ref)  ##  0 ok!
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#

Data.ref = condapply( Data.ref , condlists$variables_new )
##                        id   sum
##   czy_wplata_60         1 48741
##   kwota_wplata.1        2 48741
##   skutecznosc           3 48741
##   skutecznosc_binomial  4 48741
##   skutecznosc_grupy     5 48741

rm(datfram,vx)

   save.image(file=FILEmodel.environment)           ## NIE NADPISZ BEZMYŒLNIE !!!

########################################################################################################################•°
## 7. GRUPY ZMIENNYCH ##################################################################################################•°
## dziedziczymy z etapu 1 ale musimy zaktualizowaæ (wykluczyæ zmienne odrzucone w etapie 1)  ###########################•°

(nt.tab = nulls0.table(Data.ref))
(variables$ref = names(Data.ref))
variables$newmod

   ## zarzucone, ale warto wróciæ i przemyœleæ
   #! update.names.ref2mod()  ## zdefiniowane w    DOMAIN/Funkcje/funkcje_modele.r
   ##
   ##   ref.names.vec           ## grupy nazw zmiennych z E1 (nazwy wektorów nazw)
   ##   ref.names.list          ## grupy (wektory) nazw z E1 zapisane do listy
   ##   ref_not_mod.names.list  ## zmienne (w odpowiednich grupach), które byly w E1 ale zostaly usuniête (bo byly slabe)
   ##   mod.names.list          ## zmienne (w odpowiednich grupach), które przeszly do E2

###############################################################################•°
## A. identyfikatory

   nt.tab[ nt.tab$class == "factor" , "name" ]

   cat(intersect(variables$ref1$id ,variables$ref), sep = "\"\n,\"")

variables$id = c(  "id_produkt_k"
,"id_produkt_bank_k"
,"id_dluznik_k"
,"id_bank_k"
,"id_paczka_k"
,"id_bank_paczka_k"
,"id_nazwa_finansowa_k"
,"nazwa_finansowa_k"
,"nazwa_biznesowa_k"
,"typ_klienta_k"
,"grupa_portfela"
,"podmiot_kupujacy"
)

  setdiff(variables$id,variables$ref)   ## 0

###############################################################################•°
## B. inne

variables$regressors  ## ustalone w E1 - w pliku z etapu obróbki danych  dane_referencyjne_YYMMDD_1.r
                  ## tak jak i poni¿sze grupy zmiennych:
   intersect(variables$regressors,variables$id)
   setdiff(variables$ref,union(variables$regressors,variables$id))


##
( variables$dates = grep('^data',variables$ref,value=TRUE) )    ## tu nie ma juz dat
( variables$saldo = grep('^saldo',variables$ref,value=TRUE) )
   variables$ref1$full
   variables$ref1$tofill

## pomocnicze grupy zmiennych
(variables$dt = grep('^dt',variables$ref,value=TRUE))
(variables$dp = grep('^dp',variables$ref,value=TRUE))
(variables$czy = grep('^czy',variables$ref,value=TRUE))
(variables$kat = grep('^kat',variables$ref,value=TRUE))
(variables$kwota = grep('^kwota',variables$ref,value=TRUE))

(variables$ile = grep('^ile',variables$ref,value=TRUE))
(variables$factor = colnames.class(Data.ref,"factor"))
   sapply(Data.ref[,intersect(variables$ile,variables$factor),drop=FALSE],function(x){length(unique(x))})
   ##########################
   ## ile_kancelaria_0_k
   ##              2

(variables$wplaty = grep('^wplaty',variables$ref,value=TRUE))
(variables$skutecznosc = grep('^skutecznosc',variables$ref,value=TRUE))

cbind(unlist(mget(ls(pattern='^VAR'))))


   save.image(file=FILEmodel.environment)           ## NIE NADPISZ BEZMYŒLNIE !!!

########################################################################################################################•°
## 7. SZYBKI PRZEGL¥D ##################################################################################################•°

	attach(Data.ref)     ## dla wygody, ALE nalezy uwazac by uzupelniac w Data.ref, tj.  Data.ref$zmienna[ index ] = ...
   ## po X zmiennych

   variables$tosee = setdiff( variables$ref , union(variables$id,variables$dates) )

rows = 3 ; cols =6  ## ile wierszy i kolumn wykresow w jednym oknie
pages = ceiling(length(variables$tosee)/rows/cols)
inches = 2.5           ## ile cali na jeden wykres
width = cols*inches + 1 ; height = rows*inches + 1
   #
#pdf(file = "zmienne.pdf", width , height, onefile=TRUE , pointsize = 12 )
#jpeg(filename = "zmienne_%01d.jpg", width , height , units = "in", pointsize = 12, quality = 100 , res = 100)
   #, family, title, fonts, version, paper, encoding, bg, fg, pagecentre, colormodel,
   # useDingbats, useKerning, fillOddEven, compress )
   for(pg in 1:pages){  ## pages
      #pg = 1               ## nr serii zmiennych
      ## lub zamiast numery specyficzne grupy nazw
         #
      #win.metafile( file = "", width , height, pointsize = 12 )
      windows( width , height )
      margins.my();coldef.black();par(mfrow=c(rows,cols))
      for(k in ((rows*cols*(pg-1))+1):min((rows*cols*pg),length(variables$tosee))){
         var.name = variables$tosee[k]
         plot.variable( var.name
               , what = c("cloud")     ## "bars", "ecdf", "hist", "density", "cvs", "hist",
               , pch = "." , cex = 1
               , user.window = TRUE
               #, col = referencja + 5   ## ref -> magenta, wyc -> cyan
               , as.factor = 9
               )
      }
   }
#

graphics.off()

   detach(Data.ref)

   save.image(file=FILEmodel.environment)           ## NIE NADPISZ BEZMYŒLNIE !!!

########################################################################################################################•°


########################################################################################################################•°
########################################################################################################################•°
## II. PRESELEKCJA ZMIENNYCH
########################################################################################################################•°

########################################################################################################################•°
## 8. Random Forest -- wstêpna ocena istotnosci zmiennych  #############################################################•°

   idx = sample(1:dim(Data.ref)[1],10000)

rf_y = randomForest( y = as.factor(Data.ref[idx,VARY]), x = Data.ref[idx,variables$regressors] , importance=TRUE)
windows(width=16,height=7);varImpPlot(rf_y)

rf_efficiency = randomForest( y = Data.ref[idx,VARY1], x = Data.ref[idx,variables$regressors] , importance=TRUE)
windows(width=16,height=7);varImpPlot(rf_efficiency)

names(rf_y)

varImp = data.frame(  y = I(apply(rf_y$importance,2,function(x){order(x,decreasing=TRUE)}))
                   , efficiency = I(apply(rf_efficiency$importance,2,function(x){order(x,decreasing=TRUE)}))
             )
varImp$y[,1]
varImp$eff[,1]

   save.image(file=FILEmodel.environment)           ## NIE NADPISZ BEZMYŒLNIE !!!

########################################################################################################################•°
## 9. Eliminacja zmiennych skorelowanych  ##############################################################################•°

###################################################################################################•°
## A. wektory zmiennych  ##########################################################################•°

regressors = list()

###############################################################################•°
## regressors$binary  - czy_wplata -- VARY = "czy_wplata"

#cat(variables$regressors[varImp$pay[,"IncNodePurity"]],sep="\"\n,\"")  ## the higer the more important (according to RF)  -- for numeric y, eg. payments
cat(variables$regressors[varImp$y[,"MeanDecreaseAccuracy"]],sep="\"\n,\"")  ## the higer the more important (according to RF)  -- for factor y, eg. {0,1} "czy_..."

regressors$binary = list()

# regressors.payments = c("")
regressors$binary[[1]] = c(  "ile_ugoda_rata_zaplacone_0_k"
#,"prop_ile_ugoda_rata_zaplacone_0_k"
      ,"ile_wplat_0_k"
#,"kwota_wplat_0_k"
   ,"dp_ugoda_0_k"
,"wartosc_sporu_0_k"
#,"saldo_start_sad_k"
,"czy_bez_k"
,"skutecznosc_0_k"
#,"saldo_start_kom_k"
#,"saldo_stop_0_k"
      ,"kwota_wplata_o_0_k"
#,"ile_ugoda_rata_0_k"
#,"kwota_sr_rata_ugoda_o_0_k"
,"dp_dekl_0_k"
,"ile_dekl_zaplacone_0_k"
#,"kwota_ugoda_0_k"
,"dp_tel_0_k"
#,"kwota_wplata_p_0_k"
,"czy_kom_k"
#,"saldo_start_0_k"
#,"kwota_ugoda_o_0_k"
#,"saldo_start_pol_k"
#,"saldo_start_ugoda_o_0_k"
,"dzien_list_0_k"
      ,"dt_0_k"
,"dzien_wplata_0_k"
#,"dt_ugoda_0_k"
,"grupa_portfela"
#,"kwota_wplat_dekl_0_k"
   ,"dzien_ugoda_0_k"
#,"kwota_wplat_ugoda_0_k"
#,"dt_stop_0_k"
#,"prop_ile_dekl_zaplacone_0_k"
,"dzien_dekl_0_k"
#,"prop_kwota_wplat_dekl_0_k"
,"saldo_start_k_kapital"
,"prop_wartosc_sporu_saldo_stop_0_k"
#,"koszt_0_k"
#,"etap_w_wplata_o_0_k"
,"dp_list_0_k"
,"prop_kwota_wplat_dekl_saldo_start_0_k"
      ,"czas_polaczen_sek_razem_0_k"
#,"prop_kwota_wplat_ugoda_0_k"
#,"prop_ile_dekl_dt_0_k"
      ,"dp_wiz_0_k"
,"saldo_start_k_odsetki"
#,"prop_kwota_wplat_ugoda_saldo_start_0_k"
#,"prop_kwota_ugoda_saldo_start_0_k"
,"prop_koszt_do_saldo_start_0_k"
#,"kat_proces_kom_0_k"
#,"dzien_wiz_0_k"
#,"dpd_stop_0_k"
,"dtu"
      ,"ile_pglos_0_k"
      ,"ile_list_0_k"
#,"ile_tel_0_k"
#,"dt_pol_k"
#,"ile_sztuk_list_0_k"
#,"nr_ugoda_0_k"
,"prop_wartosc_sporu_saldo_start_pol_k"
#,"prop_kwota_dekl_saldo_start_0_k"
#,"ile_telw_0_k"
,"saldo_start_k_koszty"
,"dpo_start_k"
#,"czy_ugoda_0_k"
#,"kwota_dekl_0_k"
      ,"dpd_start_0_k"
,"dzien_tel_0_k"
#,"dpd_start_k"
,"dt_dekl_0_k"
,"wiek"
#,"kat_rodzaj_postepowania_0_k"
#,"kwota_sr_dekl_0_k"
      ,"dt_sad_k"
#,"ile_dekl_0_k"
,"ile_telp_0_k"
#,"kwota_dekl_o_0_k"
#,"kat_tryb_postepowania_0_k"
#,"czy_epu_0_k"
#,"czy_list_0_k"
#,"czy_sad_k"
,"id_status_produkt_0_k"
#,"opis_status_produkt_0_k"
,"dt_start_0_k"
,"dt_kom_k"
#,"ile_sztuk_list_przed_0_k"
      ,"prop_koszt_do_saldo_start_przed_0_k"
#,"czy_dekl_0_k"
#,"koszt_przed_0_k"
#,"kwota_wplat_przed_0_k"
#,"ile_list_przed_0_k"
#,"ile_wiz_0_k"
#,"skutecznosc_przed_0_k"
#,"czy_tel_0_k"
,"dt_bez_k"
#,"czy_wiz_0_k"
,"kwota_wplata_o_przed_0_k"
#,"rank_rodzaj_telefon_0_k"
#,"czy_emeryt"
#"rank_rodzaj_adres_0_k"
#,"opis_rodzaj_adres_0_k"
#,"id_rodzaj_adres_0_k"
#,"ile_wplat_przed_0_k"
#,"ile_kancelaria_0_k"
#,"wojewodztwo_0_k"
#,"czy_aktywny_telefon_0_k"
#,"plec"
#,"czy_powtorne_0_k"
#,"czy_wplata_0_k"
)    ## ...


###################################################################################################•°
## b. Korelacje (grafy) - zmienne ci¹g³e ##########################################################•°
rho =.3 #  =.5  =.3   # wazne jest tylko r=.7
 #windows()
 #! ponizsza funkcja znajduje korelacje jedynie dla zmiennych liczbowych !!!   I dobrze!
 (graf0 <- graf(zmienne.graf = regressors$binary[[1]], dane = Data.ref,
      r=rho,
      zmienne.bez.null = regressors$binary[[1]],
      target = VARY, ver = 2,               #! bledy dla  ver=1 ## wysypuje sie gdy nie ma zadnych korelacji (powyzej zadanego r)
      metoda = list( c(1) , c("pearson", "kendall", "spearman") ) ))

 graf0$`korelacja zmiennych zaleznych`[c(5,6,8,9),c(5,6,8,9)]


#######################################•°
## sprz¹tanie
rm(Data.graf)

###################################################################################################•°
## c. chi^2 - zmienne kategoryczne ################################################################•°
##
     colnames.class( Data.ref[,variables$regressors] , "factor")
     colnames.class( Data.ref[,regressors$binary[[1]]] , "factor")
## Ponizej tesujemy hurtowo hipoteze ze zmienne kategoryczne (factors) sa niezalezne.
## Odrzucamy ja dla p.value<0.05.
## Postêpujemy tak jak dla zmiennych numerycznych tj. odrzucamy zmienne a¿ bêdzie brak 'korelacji'
##  na poczatek te ktore koreluja z duza iloscia innych
chisqs = chisq.multi.test(Data.ref[,regressors$binary[[1]]])
  ## nie przejmujemy sie ostrzezeniami
chisqs$cor.pairs ## "skorelowane" (a raczej zalezne) pary zmiennych kategorycznych, tj. tylko te pary dla których p<0.05 (tak male P(para zmiennych niezale¿na))

#######################################•°
## dokladniej

with(Data.ref,table(id_status_produkt_0_k,id_bank_glowny))
with(Data.ref,table(id_status_produkt_0_k,wojewodztwo_0_k))
with(Data.ref,table(id_bank_glowny,wojewodztwo_0_k))

with( Data.ref ,{ var.name = "wojewodztwo_0_k" ; yy = "wplaty_suma"
   formula_0 = paste0(yy ," ~ ",var.name)
   model = lm(formula(formula_0))
   summary(model)
   ##
   windows();par.my();par(mfrow = c(2,2))
   plot(model,main = formula_0)
   par(mfrow = c(1,1))
##
table(get(var.name),useNA="ifany")
##
#windows(width = 4  , height = 8);par.my();par(mfrow = c(2,1))
windows(width = 4  , height = 4);par.my()
plot.means.fit(model , cex.means = .7 , main = formula_0 , ylab = yy , xlab=var.name , labels = levels(get(var.name))) ## my function
windows(width = 4  , height = 4);par.my()
barplot.confint(get(var.name),get(yy))   ## my function -- independent of external model thus creates its own
})

###################################################################################################•°


###############################################################################•°
## regressors$binomial - efficiency -- VARY1 = "skutecznosc"

regressors$binomial[[1]] = list()

cat(variables$regressors[varImp$eff[,"IncNodePurity"]],sep="\"\n,\"")  ## the higer the more important (according to RF)

regressors$binomial[[1]] = c( ""
)

###################################################################################################•°
## b. Korelacje (grafy) - zmienne ci¹g³e ##########################################################•°
rho =.7 #  =.5  =.3   # wazne jest tylko r=.7
 windows()
 #! ponizsza funkcja znajduje korelacje jedynie dla zmiennych liczbowych !!!   I dobrze!
 (graf0 <- graf(zmienne.graf = regressors$binomial[[1]], dane = Data.ref,
      r=rho,
      zmienne.bez.null = regressors$binomial[[1]],
      target = "skutecznosc", ver = 2,               #! bledy dla  ver=1 ## wysypuje sie gdy nie ma zadnych korelacji (powyzej zadanego r)
      metoda = list( c(1) , c("pearson", "kendall", "spearman") ) ))

 graf0$`korelacja zmiennych zaleznych`[c(5,6,8,9),c(5,6,8,9)]

   setdiff(regressors$binary[[1]],regressors$binomial[[1]])
   setdiff(regressors$binomial[[1]],regressors$binary[[1]])

#######################################•°
## sprz¹tanie
rm(Data.graf)

###################################################################################################•°
## c. chi^2 - zmienne kategoryczne ################################################################•°
##
## Ponizej tesujemy hurtowo hipoteze ze zmienne kategoryczne (factors) sa niezalezne.
## Odrzucamy ja dla p.value<0.05.
## Postêpujemy tak jak dla zmiennych numerycznych tj. odrzucamy zmienne a¿ bêdzie brak 'korelacji'
##  na poczatek te ktore koreluja z duza iloscia innych
chisqs = chisq.multi.test(Data.ref[,regressors$binomial[[1]]])
  ## nie przejmujemy sie ostrzezeniami
chisqs$cor.pairs ## "skorelowane" (a raczej zalezne) pary zmiennych kategorycznych, tj. tylko te pary dla których p<0.05 (tak male P(para zmiennych niezale¿na))

#######################################•°
## dokladniej

with(Data.ref,table(ile_ugoda_0_k,id_bank_glowny))
with(Data.ref,table(rodzaj_strony,wojewodztwo_0_k))
with(Data.ref,table(id_bank_glowny,wojewodztwo_0_k))

with( Data.ref ,{ var.name = "wojewodztwo_0_k" ; yy = "skutecznosc"
   formula_0 = paste0(yy ," ~ ",var.name)
   model = lm(formula(formula_0))
##
   print(table(get(var.name),useNA="ifany"))
##
windows(width = 4  , height = 8);par.my();par(mfrow = c(2,1))
#windows(width = 4  , height = 4);par.my()
plot.means.fit(model , cex.means = .7 , main = formula_0 , ylab = yy , xlab=var.name , labels = levels(get(var.name))) ## my function
barplot(table(get(var.name)),xlab = var.name)
#windows(width = 4  , height = 4);par.my()
#barplot.confint(get(var.name),get(yy))   ## my function -- independent of external model thus creates its own
})

##################################################################################################•°


########################################################################################################################•°
## 8. Random Forest -- ponowna ocena istotnosci zmiennych  #############################################################•°
##                     tylko wybrane zmienne               #############################################################•°

rf_y_0 = rf_y_1
rf_y_1 = randomForest( y = as.factor(Data.ref[idx,VARY]), x = Data.ref[idx,regressors$binary[[1]]] , importance=TRUE)
windows();varImpPlot(rf_y_0)

#rf_efficiency_1 = randomForest( y = Data.ref[,VARY1], x = Data.ref[,regressors$binomial$`0`] , importance=TRUE)
#windows();varImpPlot(rf_efficiency_1)

   save.image(file=FILEmodel.environment)           ## NIE NADPISZ BEZMYŒLNIE !!!


########################################################################################################################•°
########################################################################################################################•°
## III. BUDOWA MODELU
########################################################################################################################•°

###########################################################•°
#!#!  Listy: formula , summaries , summaries.agg
#!#!
#!#!  formulas = list()
#!#!  summaries = list()
#!#!  summaries.agg = list()  ##
#!#!  summaries.agg.adds = list()
###########################################################•°


########################################################################################################################•°
## UWAGA: Poszukuj¹c wzorca do budowy modelu idŸ do pkt.   2. model gam(,family=poisson)
## (pkt 1. ju¿ OK ale nie przetestowany - jednak nie powinno byæ niespodzianek)
########################################################################################################################•°

########################################################################################################################•°
## 1. model gam(,family=binomial(link=logit))  #########################################################################•°
cbind(unlist(mget(ls(pattern='^VAR'))))

###########################################################•°
#!#!  Listy: formula$binary , summaries$binary , summaries.agg$binary.
#!#!
#!#!  regressors$binary = regressors$binary[1]
#!#!  formulas$binary = list()
#!#!  summaries$binary = list()
#!#!  summaries.agg$binary = list()  ##
#!#!  rm(panova.agg.adds)
###########################################################•°

summaries = list()
summaries$binary = list()

summary_k_0 = list()
summary_k_0$raw = NA #(data.frame,panova)
summary_k_0$agg = list()
summary_k_0$agg$pars = NA #(data.frame,panova.agg)    # BUT : summaries$binary[[k]]$whole$agg$pars  = update.df( summaries$binary[[k]]$folds$agg$pars , summaries$binary[[k]]$whole$raw , join = "left" , action = "append" )
summary_k_0$agg$adds = NA #(data.frame,panova.agg.adds)

summary_k = list()
summary_k$folds = summary_k_0
summary_k$whole = summary_k_0

##
summaries.agg = list()
summaries.agg$binary = list()
summaries.agg$binary$folds = NA  ## data.frame
summaries.agg$binary$whole = NA  ##


###########################################################•°
## zmienna pomocnicza do zbalansowanego podzialu Data.ref
VARbalanced = "czy_bez_k"
table(Data.ref[,VARbalanced])
nulls0.table(Data.ref)
  #
cat(regressors$binary[[1]],sep="\"\n,\"")

k = 4
regressors$binary[[k]] = c( "ile_ugoda_rata_zaplacone_0_k"
,"ile_wplat_0_k"
#,"dp_ugoda_0_k"           #1
,"wartosc_sporu_0_k"
,"czy_bez_k"
,"skutecznosc_0_k"
,"kwota_wplata_o_0_k"
,"dp_dekl_0_k"
,"ile_dekl_zaplacone_0_k"
,"dp_tel_0_k"
,"czy_kom_k"
,"dzien_list_0_k"
#,"dt_0_k"                 #2
,"dzien_wplata_0_k"
,"grupa_portfela"
,"dzien_ugoda_0_k"
#,"dzien_dekl_0_k"         #3
,"saldo_start_k_kapital"
,"prop_wartosc_sporu_saldo_stop_0_k"
#,"dp_list_0_k"            #1
#,"prop_kwota_wplat_dekl_saldo_start_0_k"       #2
#,"czas_polaczen_sek_razem_0_k"        #3
,"dp_wiz_0_k"
,"saldo_start_k_odsetki"
,"prop_koszt_do_saldo_start_0_k"
,"dtu"
,"ile_pglos_0_k"
,"ile_list_0_k"
,"prop_wartosc_sporu_saldo_start_pol_k"
#,"saldo_start_k_koszty"   #1
,"dpo_start_k"
,"dpd_start_0_k"
,"dzien_tel_0_k"
,"dt_dekl_0_k"
,"wiek"
,"dt_sad_k"
,"ile_telp_0_k"
,"id_status_produkt_0_k"
,"dt_start_0_k"
#,"dt_kom_k"               #1
#,"prop_koszt_do_saldo_start_przed_0_k"        #1
,"dt_bez_k"
,"kwota_wplata_o_przed_0_k"
)

cat(regressors$binary[[k]],sep=")\"\n,\"s(")

k=7
regressors$binary[[k]] = c( "s(ile_ugoda_rata_zaplacone_0_k)"
,"s(ile_wplat_0_k)"
#,"dp_ugoda_0_k"           #1
,"s(wartosc_sporu_0_k)"
,"czy_bez_k"
,"s(skutecznosc_0_k)"
,"s(kwota_wplata_o_0_k)"
,"s(dp_dekl_0_k)"
,"s(ile_dekl_zaplacone_0_k)"
#,"s(dp_tel_0_k)"              #5
,"czy_kom_k"
,"s(dzien_list_0_k)"
#,"dt_0_k"                 #2
,"s(dzien_wplata_0_k)"
,"grupa_portfela"
,"s(dzien_ugoda_0_k)"
#,"dzien_dekl_0_k"         #3
,"s(saldo_start_k_kapital)"
,"s(prop_wartosc_sporu_saldo_stop_0_k)"
#,"dp_list_0_k"            #1
#,"prop_kwota_wplat_dekl_saldo_start_0_k"       #2
#,"czas_polaczen_sek_razem_0_k"        #3
,"s(dp_wiz_0_k)"
,"s(saldo_start_k_odsetki)"
,"s(prop_koszt_do_saldo_start_0_k)"
,"s(dtu)"
,"s(ile_pglos_0_k)"
,"s(ile_list_0_k)"
,"s(prop_wartosc_sporu_saldo_start_pol_k)"
#,"saldo_start_k_koszty"   #1
,"s(dpo_start_k)"
#,"s(dpd_start_0_k)"             #5
#,"s(dzien_tel_0_k)"             #5
,"s(dt_dekl_0_k)"
,"s(wiek)"
,"s(dt_sad_k)"
#,"s(ile_telp_0_k)"                 #6
,"id_status_produkt_0_k"
#,"s(dt_start_0_k)"              #5
#,"dt_kom_k"               #1
#,"prop_koszt_do_saldo_start_przed_0_k"        #1
,"s(dt_bez_k)"
,"s(kwota_wplata_o_przed_0_k)"
)

      #for(k in 1:7){summaries$binary[[k]] = summary_k}

summaries$binary[[k]] = summary_k
formulas$binary[[k]] = paste0(VARY," ~ ",paste(regressors$binary[[k]],collapse=" + "))
rm(summary.df_k)
for(j in 1:30){
   Data.ref_kj = split.rand.portion(Data.ref,split=TRUE,variable=VARbalanced,portion=.8)
   model_kj = gam(formula(formulas$binary[[k]]),family=binomial(link=logit),data=Data.ref_kj)
   summary.gam.panova(model_kj,what=c("coef","pval"),sstars=TRUE,append.to="summary.df_k",add=c("mse","aic","gini","auc","p.mww"),print=FALSE)
}
(summaries$binary[[k]]$folds$raw = summary.df_k)

      #for(k in 1:7){summaries$binary[[k]]$folds$raw = summaries.a$binary[[k]]}


summary.df_k.agg = summary.gam.panova.agg( summary.df_k
   , FUN = list( "coeffs" =function(x){c("mean" = mean(x))}
               , "coeffs" =function(x){signs.table(x)}
               , "Pr(>F)" =function(x){c("mean" = mean(x))}
               , "Pr(>F)" =function(x){c("sstars" = signif.stars(mean(x)))}
               , "sstars" =function(x){sstars.table(x) })
   , add.colnames = c(TRUE,FALSE,TRUE,TRUE,FALSE))
(summaries$binary[[k]]$folds$agg$pars = summary.df_k.agg)

      for(k in 1:7){summaries$binary[[k]]$folds$agg$pars = summaries.agg.a$binary[[k]]}

summary.gam.panova.agg.adds( summary.df_k
      , FUN = list( "MSE" = function(x){c("min"=min(x),"max"=max(x),"mean"=mean(x))}
                  , "AIC" = function(x){c("min"=min(x),"max"=max(x),"mean"=mean(x))}
                  , "Gini" = function(x){c("min"=min(x),"max"=max(x),"mean"=mean(x))}
                  , "AUC" = function(x){c("min"=min(x),"max"=max(x),"mean"=mean(x))}
                  , "p.mww" = function(x){c("mean"=mean(x))}
                  )
      , add.colnames = TRUE
      , append.to = "panova.agg.adds" ,print=TRUE
)
summaries$binary[[k]]$folds$agg$adds = panova.agg.adds[k,]

      for(k in 1:7){summaries$binary[[k]]$folds$agg$adds = summaries.agg.adds$binary[k,]}

summaries.agg$binary$folds = summaries.agg.adds$binary

windows();par.my()
split.stats( predict(model_kj,type="response") , Data.ref_kj[,VARY] , plot = TRUE )


rm(panova.agg.adds)

#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
   save.image(file=FILEmodel.environment)           ## NIE NADPISZ BEZMYŒLNIE !!!
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#


###############################################################################•°
## Modele na caloœci danych w oparciu o wyniki badañ na podzbiorach danych

rm(whole.panova.agg.adds)
for(k in 1:length(formulas$binary)){ # k=1
   print(k)
   model_k = gam(formula(formulas$binary[[k]]),family=binomial(link=logit),data=Data.ref)
   summaries$binary[[k]]$whole$raw = summary.gam.panova(model_k,what=c("coef","pval"),sstars=TRUE,add=c("mse","aic","gini","auc","p.mww"))
#
   summaries$binary[[k]]$whole$agg$pars = update.df( summaries$binary[[k]]$folds$agg$pars , summaries$binary[[k]]$whole$raw , join = "left" , action = "append" )
#
   summary.gam.panova.agg.adds( summaries$binary[[k]]$whole$raw
      , FUN = list( "MSE"   = function(x){c(mean(x))}
                  , "AIC"   = function(x){c(mean(x))}
                  , "Gini"  = function(x){c(mean(x))}
                  , "AUC"   = function(x){c(mean(x))}
                  , "p.mww" = function(x){c(mean(x))}
                  )
      , add.colnames = TRUE
      , append.to = "whole.panova.agg.adds"
   )
   summaries$binary[[k]]$whole$agg$adds = whole.panova.agg.adds[k,]
#
}
summaries.agg$binary$whole = whole.panova.agg.adds ;

   #summaries.agg$binary$whole = summaries.agg.adds$binary_whole

summaries.agg
summaries$binary[[5]]

###############################################################################•°
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
#!#  Wybieramy model 5  #!#!#!#!#!#!#!#
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#

#regressors.gam.binomial = regressors.binomial.list[[5]]
#formula.gam.binomial = paste0(VARY.binomial," ~ ",paste(regressors.gam.binomial,collapse=" + "))
model.gam.binary = gam(formula(formulas$binary[[5]]),family=binomial(link=logit),data=Data.ref)

windows();par.my()
split.stats( predict(model.gam.binary,type="response") , Data.ref[,VARY] , plot = TRUE )

###############################################################################•°
## odzyski faktyczne a przewidywane z binomial

   prediction.gam.binary = predict(model.gam.binary,type="response")*Data.ref[,VARdebt1]
	sum(prediction.gam.binary)
	sum(Data.ref[,VARpays])

  windows(height = 6, width = 5 ); par.my()
  plot( Data.ref[,VARpays] ~ prediction.gam.binary , pch='.' , col = "yellow" , ylab = VARpays )
  	title("wp³aty rzeczywiste vs przewidywane binomial")
	lm.pois = lm(Data.ref[,VARpays] ~ prediction.gam.binary)
	abline(lm.pois,col="white");
	text(	  x = max(prediction.gam.binary)/2 , y = max(Data.ref[,VARpays])*.9
			, labels = paste0( "y = " , paste( paste0( round( lm.pois$coef, 3 ) , c(""," * x") ), collapse = " + ") )
		 )
	#  abline(h=0,col="white")
	#  legend( "topleft" , legend = unique(Data.ref$grupa_portfela) , col = unique(as.numeric(Data.ref$grupa_portfela)) + 1, pch = 20 )


#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
   save.image(file=FILEmodel.environment)           ## NIE NADPISZ BEZMYŒLNIE !!!
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#


########################################################################################################################•°
## 2. model gam(,family=poisson)  ######################################################################################•°

VARY.poisson = VARY

with( Data.ref , { plot.variable(wplaty_suma) } )

###########################################################•°
## zmienna pomocnicza do zbalansowanego podzialu Data.ref
Data.ref$czy_wplata = Data.ref[,VARY] == 0 ; table(Data.ref$czy_wplata)
nulls0.table(Data.ref)
  #
###########################################################•°
#!#!  Listy: summary. , regressors. , formula.
#!#!    summary.poisson.list = list()   ## nowa lista z tabelami modeli ## uwazaj by nie usunac!!!!!
#!#!    regressors.poisson.list = list()
#!#!    formula.poisson.list = list()
###########################################################•°

cat(regressors.payments,sep="\"\n,\"")

k=4
regressors.poisson.list[[k]] = c( "kwota_sr_rata_ugoda_o_0_k"
,"dpw_0_k"
,"ile_ugoda_rata_zaplacone_0_k"
,"dp_tel_0_k"
,"dp_ugoda_0_k"
#,"dp_wiz_0_k"                             #3
,"dpo_start_k"
,"saldo_start_k_odsetki"
,"dp_list_0_k"
,"dpd_start_0_k"
,"prop_koszt_do_saldo_start_0_k"
#,"dzien_tel_0_k"                         #2
,"koszt_0_k"
#,"dpd_stop_0_k"                          #2
,"dzien_wiz_0_k"
#,"wiek"                                   #3
#,"prop_koszt_do_saldo_start_przed_0_k"  #1
#,"kwota_sr_dekl_0_k"                      #3
#,"dtu"                                #1
,"saldo_start_k_koszty"
#,"ile_telw_0_k"                            #3
#,"ile_telp_0_k"                       #1
,"dzien_list_0_k"
#,"dzien_wplata_0_k"                    #2
,"wojewodztwo_0_k"
#,"saldo_stop_koszty"                   #2
,"prop_wartosc_sporu_saldo_stop_0_k"
#,"dzien_ugoda_0_k"                    #1
,"saldo_stop_0_k"
#,"prop_kwota_wplat_dekl_saldo_start_0_k" #3
#,"ile_pglos_0_k"                       #3
,"dt_sad_k"
#,"skutecznosc_przed_0_k"      #1
,"id_bank_glowny"
#,"dt_dekl_0_k"                #1
#,"dt_kom_k"                   #1
#,"czy_epu_0_k"                   #3
#,"czy_aktywny_telefon_0_k"    #1
#,"czy_list_0_k"                 #2
#,"czy_tel_0_k"                    #3
,"czy_bez_k"
)

cat(regressors.poisson.list[[4]],sep=")\"\n,\"s(")

k = 8
regressors.poisson.list[[k]] = c( "s(kwota_sr_rata_ugoda_o_0_k)"
,"s(dpw_0_k)"
#,"s(ile_ugoda_rata_zaplacone_0_k)"   #7
,"s(dp_tel_0_k)"
#,"s(dp_ugoda_0_k)"                #5
,"s(dpo_start_k)"
,"s(saldo_start_k_odsetki)"
,"s(dp_list_0_k)"
#,"s(dpd_start_0_k)"
#,"s(prop_koszt_do_saldo_start_0_k)"  #7
#,"s(koszt_0_k)"                   #5
#,"s(dzien_wiz_0_k)"               #5
,"s(saldo_start_k_koszty)"
#,"s(dzien_list_0_k)"              #5
,"wojewodztwo_0_k"
,"s(prop_wartosc_sporu_saldo_stop_0_k)"
,"s(saldo_stop_0_k)"
,"s(dt_sad_k)"
,"id_bank_glowny"
,"czy_bez_k"
)
(formula.poisson.list[[k]] = paste0(VARY.poisson," ~ ",paste(regressors.poisson.list[[k]],collapse=" + ")))

rm(summary.df_k)
for(j in 1:10){
   Data.ref_kj = split.rand.portion(Data.ref,split=TRUE,variable="czy_wplata",portion=.9)
   model_kj = gam(formula(formula.poisson.list[[k]]),family=poisson,data=Data.ref_kj)
   summary.df_k = summary.gam.panova(model_kj,what="coef",append.to="summary.df_k")
}
summary.df_k

## zapisujemy wyniki dla kazdego zestawu zmiennych do listy
summary.poisson.list[[k]] = summary.df_k
names(summary.poisson.list)[k] = paste( k , paste0("mean(AIC) = " , mean.aic(summary.df_k))
                                          , paste0("mean(MSE) = " , mean.mse(summary.df_k)) , sep = " ; ")
summary.poisson.list

## MODEL NA CALOŒCI DANYCH (dla porównania)
model.gam_k = gam(formula(formula.poisson.list[[k]]),family=poisson,data=Data.ref)
summary.gam.panova(model.gam_k,what=c("coef","pval"))

#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
#!#   Wybieramy model  8  #!#!#!#!#!#!#
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#

regressors.gam.poisson = regressors.poisson.list[[8]]
formula.gam.poisson = paste0(VARY.poisson," ~ ",paste(regressors.gam.poisson,collapse=" + "))
model.gam.poisson = gam(formula(formula.gam.poisson),family=poisson,data=Data.ref)


###########################################################•°
## odzyski faktyczne a przewidywane z poisson( log )

   prediction.gam.poisson = predict(model.gam.poisson,type="response")
	sum(prediction.gam.poisson)
	sum(Data.ref[,VARpays])

  windows(height = 6, width = 5 ); par.my()
  plot( Data.ref[,VARpays] ~ prediction.gam.poisson , pch='.' , col = "yellow" , ylab = VARpays )
  	title("wp³aty rzeczywiste vs przewidywane poisson")
	lm.pois = lm(Data.ref[,VARpays] ~ prediction.gam.poisson)
	abline(lm.pois,col="white");
	text(	  x = max(prediction.gam.poisson)/2 , y = max(Data.ref[,VARpays])*.9
			, labels = paste0( "y = " , paste( paste0( round( lm.pois$coef, 3 ) , c(""," * x") ), collapse = " + ") )
		 )
	#  abline(h=0,col="white")
	#  legend( "topleft" , legend = unique(Data.ref$grupa_portfela) , col = unique(as.numeric(Data.ref$grupa_portfela)) + 1, pch = 20 )

########################################################################################################################•°
## przegl¹d
names(summaries)
names(summaries$binary)
length(summaries$binary)
names(summaries$binary[[1]])
names(summaries$binary[[1]]$folds)
class(summaries$binary[[1]]$folds$raw)
dim(summaries$binary[[1]]$folds$raw)
colnames(summaries$binary[[1]]$folds$raw)
class(summaries$binary[[1]]$folds$agg)
names(summaries$binary[[1]]$folds$agg)
class(summaries$binary[[1]]$folds$agg$pars)
class(summaries$binary[[1]]$folds$agg$adds)

names(summaries.agg)
names(summaries.agg$binary)
class(summaries.agg$binary$folds)
class(summaries.agg$binary$whole)


########################################################################################################################•°
########################################################################################################################•°
## IV. ZAPIS DO PLIKU
#!######################################################################################################################•°

###################################################################################################•°
## A. Cale œrodowisko

save.image(file=FILEmodel.environment,compress="xz")				## NIE NADPISZ BEZMYŒLNIE!!!

###################################################################################################•°
## B. Tylko potrzebne rzeczy

   cat(sort(ls()),sep="\n")

ls(pattern='^(D|d)ata')
   ls(pattern='^names')
ls(pattern='^VAR')

trans
ls(pattern='^trans')
ls(pattern='\\.condlist$')
variables.new.condlist

      #!#!#!#!#! To powinno byæ w E1 !!!
      #!#!#!#!#!  lista oryginalnych poziomów zmiennych kategorycznych (data.raw) - bêdzie potrzebna do prognozy aby uzgodniæ poziomy tych zmiennch dla  data.new
      levels.raw = list()
      for(fac in names(trans$toimprove.list)){
         levels.raw[[fac]] = levels(as.factor(data.ref[,fac]))
      }
      #!#!#!#!#!#!#!#!#!#!#!#!#!

   ls(pattern='^model')

OBJECTS = union(OBJECTS,ls(pattern='^model'))
OBJECTSmod = OBJECTS
OBJECTS = c(OBJECTS,"OBJECTSmod")

	save( list = OBJECTS
			, file = FILEmodel.Rdata , compress = "xz" )

#####################

   #	write.table(model.df, FILEmodel.csv , sep = ";", na="" ,dec = ",", qmethod = "double", row.names = TRUE)

## fitted values to DB
#   #ch = odbcConnect("PostgreSQL35W",uid="akasprzyk",pwd="akasprzyk") ## IKTORN
#	ch = odbcConnect("PostgreSQL35W-T",uid="arkadiusz.kasprzyk",pwd="akasprzyk123") ## TOUDI
#   # sqlQuery(ch,paste0("DROP TABLE IF EXISTS ",TABLEmodel))
#      TABLEmodel
#	sqlSave( channel = ch,tablename = TABLEmodel , dat =  model.df , rownames = "id_produkt_k" , verbose = TRUE )                  ##rownames = names(wynik1[[1]])
#	odbcClose(ch)


########################################################################################################################•°


        
        
        
        
