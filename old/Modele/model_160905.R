########################################################################################################################•°
*## DZIA£ ANALIZ ## ANALIZY ## MODELE STATYSTYCZNE ## A.K. ## 160905
########################################################################################################################•°
rm(list=ls())     ## czyszczenie przestrzeni roboczej
########################################################################################################################•°
#! MODEL ###############################################################################################################•°
########################################################################################################################•°
## CEL: model na wysy³kê do s¹du
##
## OPIS:
##    REFERENCJA
##       X = etap pol
##       Y = wp³aty/skutecznoœæ  w 360 dni s¹d
##    DODATKOWO
##       • wykluczono z referencji rekordy bez  dpd_start_pol_k
##
## MODEL:
##	   • gam binomial
##	   • gam poisson
##
#################################################################
########################################################################################################################•°
ID = "160905"   ## takie samo jak nazwa katalogu ##  w przybli¿eniu data powstania;
DATEmodel = as.POSIXct("2016-09-05") ## data modelu
ENTITY = ""
##
METHOD = "gam"
FAMILY = c("poisson", "binomial", "binary")
prediction.name = ""            ## np: "score" w przypadku scoringu ; "prognoza"  dla innych metod
##
IDdataref = "160616"						 ## katalog z danymi referencyjnymi na których by³ zbudowany model
IDref = "1"
Ysuffix = ""      		 ## przyrostek nazwy tabeli ze zmienn¹ objaœnian¹ ## TYLKO JEDEN W CA£YM PLIKU !!!  inny Y = inny model = inny plik i katalog modelu
##
##  BD :  toudi.modele.
##
##    Utworzono zmienne nazw tabel:
##                  TABLE
##   TABLEdataref   "modele.referencja_160616"
##   TABLEdataref_Y "modele.referencja_160616_"
##   TABLEmodel     "modele.referencja_160616_model_160905"
##
##    Utworzono zmienne nazw plików:
##                         FILE
##   FILEdataref.Rdata     "referencja_160616_1.Rdata"
##   FILEmodel_Y           "model_160905_Y.sql"
##   FILEmodel.environment "model_160905_environment.Rdata"
##   FILEmodel.Rdata       "model_160905.Rdata"
##   FILEmodel.sql         "model_160905.sql"
##   FILEmodel.xls         "model_160905.xls"
##   FILEmodel.csv         "model_160905.csv"
##
##    Utworzono zmienne nazw katalogów:
##               PATH
##   PATHdataref "//10.1.1.5/PK_Analitycy/Modele/Dane Referencyjne/160616"
##   PATHmodel   "//10.1.1.5/PK_Analitycy/Modele/Modele/160905"
##   PATHwd      "//10.1.1.5/PK_Analitycy/Modele/Modele/160905"
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
DOMAIN = paste(ROOT, "Modele", sep="/")
  source( paste0( DOMAIN, "/Funkcje/funkcje_modele.r" ) )
PATHdata = paste(DOMAIN, "Dane Referencyjne", IDdataref, sep="/")
PATHmodel = paste(DOMAIN, "Modele", ID, sep="/")
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
#! 2. UWAGI WSTÊPNE ####################################################################################################•°
########################################################################################################################•°
## A. PODSTAWOWYM ZADANIEM niniejszego pliku jest zbudowanie MODELU na danych z pliku  FILEdataref.Rdata (ale NIE  FILEdataref.environment !!!)
##    Dane FILEdataref.Rdata s¹ w 100% gotowe do analizy statystycznej: badania korelacji, wykluczania zmiennych, budowy modelu.
##    1. pobranie danych  Data.ref , Data.ref_y  z  FILEdataref.Rdata
##    2. selekcja rekordów na których chcemy zbudowaæ model
##    3. badanie korelacji
##    4. wykluczanie zmiennych
##    5. ...
##    6. stabilny model (byæ mo¿e wiele wersji)
##
########################################################################################################################•°
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
########################################################################################################################•°

########################################################################################################################•°
## 3. PAKIETY I SKRYPTY ################################################################################################•°

## PAKIETY
packages = c( "MASS",
			  	  "RPostgreSQL", "RODBC"#, "vioplot"
                , "rpart",# "earth" ,
              "randomForest",#"car",
              "ROCR", "RODBC", "igraph",
              "gam", "rattle", "igraph",
              "stats", "graphics", "grDevices",
              "datasets", "methods", "base"
              #, "caret"
              #, "gdata"#, "sqldf"
              #, "data.table"
              )

## Instalacja pakietów jeœli jeszcze nie zainstalowane
#  chooseCRANmirror()
#  for(x in packages)install.packages(x)

#! Pakiety nie ³aduj¹ siê wraz z przestrzeni¹ robocz¹!
#! £adowanie pakietów - konieczne!
l = quote(library(a))
for(p in packages){ l[[2]] = as.name(p);  eval(l) }    #!!! YEAH!!!
  #!  l; as.list(l)   ## check it !!!
## for(p in packages){library(p, character.only = FALSE)}  ## simpler but less sexy :)

## SKRYPTY 1  ## funkcje nowe by AK
  setwd(paste0(ROOT, "/R/FunctionsAK"));    source("sourceFunctionsAK.r");  setwd(PATHwd)

## SKRYPTY 2  ## funkcje stare (do scoringu)
  setwd(paste0(ROOT, "/R/FunctionsScoring"));   source("sourceFunctionsScoring.r");   setwd(PATHwd)
  # source("wszystkie_funkcje_kompilacja.r")

## SKRYPTY 3  ## mo¿e jeszcze coœ?
  #source("")


########################################################################################################################•°
## 4. DANE I OBIEKTY ###################################################################################################•°
## Pomijamy jeœli wczeœniej za³adawaliœmy œrodowisko w linii 90 (oko³o), rozdzia³ 1.c

###############################################################################•°
## A. OBIEKTY WYMAGANE - bed¹ póŸniej zapisane i przekazane do E2  ############•°

OBJECTS = c(  ## z E1
              "Data.ref" , "Data.ref.nas" , "data.ref"   #, "data.ref.nas" , "data.ref.outs"
            ##
				, "VAR" #, "VARid" , "VARdebt" , "VARY" , "VARY1"
            ##
            , "variables"  ## wszystkie nazwy w jednej liœcie, której elemanetami s¹ wektory nazw, stare "names._"
            ##
            , "condlists"
            ##
				, "trans"      ## wszystkie obiekty "trans" (transformacje zmiennych) w jednej liœcie;
                           ## informacja o przekszta³ceniach cech : lista przekszta³ceñ i df z t¹ list¹ i innymi statystykami
    #!, "levels.raw" ## oryginalne wartoœci poziomów dla przekszta³conych danych kategorycznych; zob.
    ##  teraz ju¿ zapisane jako attr(trans$toimprove, "levels_old")
                           #     names(trans$toimprove.list) == names(levels.raw)
            #, ls(pattern='^VAR')
            , "OBJECTSref"
            ## E2
            , "regressors" , "formulas" , "models" , "predictions"
            , "summaries" , "summaries.agg"
            #, "model.gam.binary" #, "model.gam.poisson"
            ##
            #, DFtrans )   ## informacja o przekszta³ceniach cech : lista przekszta³ceñ i df z t¹ list¹ i innymi statystykami
           )

#! Mo¿na w trakcie praæ dodaæ inne obiekty do zapamiêtania
#  OBJECTS = union(OBJECTS, "object.xyz")


###############################################################################•°
## B. DANE REFERENCYJNE  ######################################################•°

## pierwsze ³adowanie danych referencyjnych z	DOMAIN/Dane Referencyjne/YYMMDD

## POBRANIE danych z .Rdata po E0 i E1
load(file = paste(PATHdataref, FILEdataref.Rdata, sep="/") )

ls(pattern='^(D|d)ata')   ## budujemy model na Data.ref;
   ##  data.ref do wgl¹du -- zawiera zmienna po etapie E0 - nieprzeksztalcone i z obs. odstaj¹cymi
   ## mog¹ slu¿yæ jako Ÿródlo dadatkowych danych w razie naglej potrzeby
   dim(data.ref)
   dim(Data.ref)   ## 48741   154
   (nt.all = nulls0.table(Data.ref))

OBJECTSref  ## obiekty z E1

###############################################################################•°
## C. PODSTAWOWE OBIEKTY -- przegl¹d  #########################################•°

###########################################################•°
## variables
names(variables)

            #!#!#!#! DO WYCIÊCIA !!!!
            load(file = paste(PATHdataref, "referencja_160104_1_appx.Rdata", sep="/") )
               names(variables.e1)
               names(condlists.e1)
               names(trans.e1)
           variables$ref0 = variables.e1$ref0
           variables$ref1 = variables.e1[setdiff(names(variables.e1), "ref0")]
                 rm("variables.e1", "condlists.e1", "trans.e1")

            names(variables)
            cat(names(variables), sep="\" , \"")
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


variables$ref1 = variables[setdiff(names(variables), "ref0")]
variables = variables[c("ref0" , "ref1" , "ref", "id" , "dates" #, "notregressors"  , "full"
                        , "regressors" , "regressors_poor" )]

variables$ref = names(Data.ref)
   ( nt.regs = nulls0.table(Data.ref[, variables$regressors]) )  ## 0 NA -- OK!!!
   #
   nulls0.table(data.ref[rownames(Data.ref), variables$regressors])    #!#!#!#!#!#!#!#!#!#!!!!!!!!!

###########################################################•°
## condlists

   names(condlists)
##condlists$ref1 = condlists[setdiff(setdiff(names(condlists), "ref0"), names(condlists$ref0))]
condlists$ref1 = condlists[setdiff(names(condlists), "ref0")]
condlists = condlists[ c( "ref0" , "ref1" ) ]

   # condlists$ref1$rm_na_0 <- NULL
   # condlists$ref1$rm      <- NULL
   # condlists$ref1$fill    <- NULL
   # condlists$ref1$replace <- NULL

   #   names(condlists)
   #      class(condlists$variables_new) <- c("condlist", "list")
   #      class(condlists$selection) <- c("condlist", "list")
   #   names(condlists$ref0)
   #      class(condlists$ref0$rm_na_0) <- c("condlist", "list")
   #      class(condlists$ref0$fill) <- c("condlist", "list")
   #      class(condlists$ref0$replace) <- c("condlist", "list")
   #      class(condlists$ref0$rm) <- c("condlist", "list")
   #   names(condlists$ref1)
   #      class(condlists$ref1$mins) <- c("condlist", "list")
   #      class(condlists$ref1$maxs) <- c("condlist", "list")

###########################################################•°
## trans

   names(trans)
   lapply(trans, class)
   attr(trans$toimprove, "levels_old")
      any(names(trans$toimprove.list) !=  names(attr(trans$toimprove, "levels_old")))   #! MUST BE FALSE

###########################################################•°
## VAR
## jakie zmienne s¹ potrzebne okazuje siê czêsto dopiero w trakcie budowy modelu;
## tutaj tylko lista najistotniejszych zmiennych, którym nadajemy VARznaczenie;
## ich tworzenie w paragrafie  6. ZMIENNE POCHODNE;
##

ls(pattern='^VAR')
cbind(unlist(mget(ls(pattern='^VAR'))))

   grep('^saldo', variables$ref, value=TRUE)
   trans$list[grep('^saldo', names(trans$list), value=TRUE)]

VAR = list()

   ls(pattern="^VAR")

###################•°
VAR$id                 = "id_produkt_k"
VAR$debt               = "saldo_start_sad_k"              #! to jest  log( saldo_start_sad_k + 1 )
VAR$debt0              = "saldo_start_sad_k.1"            #! to jest oryginalne   saldo_stop_0_k  =  saldo_start_sad_k
VAR$debt1              = "saldo_start_sad_k.1"
###################•°
## Opisowe NAZWY zmiennych celu Y wg. ich rodzaju/typu -- ALE to nie s¹ NAZWY metod budowy modeli !!!
   Ynames <- c("pays", "efficiency", "efficiency_1", "efficiency_binomial", "efficiency_groups", "goodbad") ## objaœnienia poni¿ej    #!#!#! DODAÆ DO PREAMBU£Y !!!
   cat(paste0("VARY", Ynames), sep="\n")
## odpowiednie zmienne Y opisowo
VAR$pays               = "kwota_wplat_poly_k"
#VARpays0               "kwota_wplata.0"          #! to jest oryginalna kwota_wplata z E0 -- jaka wplata (bez przeksztalceñ) jeœli byla po conajmniej 30 dniach przerwy (jesli brak wplaty to 0)
#VARpays1               "kwota_wplata.1"          #! == kwota_wplata.0 * czy_wplata_60
##
VAR$efficiency         = "skutecznosc_poly_k"             ## skutecznosc oryginalna
VAR$efficiency_1       = "skutecznosc_1_poly_k"           ## skutecznosc obciêta do 1
VAR$efficiency_binomial= "skutecznosc_binomial_poly_k"    ## skutecznosc do gam(..., family=binomial) jako macierz Nx2 [succeses, failures]
VAR$efficiency_groups  = "skutecznosc_grupy_poly_k"       ## skutecznoœæ oryg. podzielona na grupy (0,.001,.01,.1, 1, 10)
VAR$goodbad            = "czy_dobra"                      ## podzia³ spraw na dobre i z³e
###################•°
## nazwy zmiennych celu wg. typu modelu
   FAMILY
   cat(paste0("VAR$Y", FAMILY), sep="\n")
VAR$Ypoisson           = VARpays
VAR$Ybinomial          = VARefficiency_binomial
VAR$Ybinary            = VARgoodbad
###################•°
## Y skrótowo -- lepiej nieu¿ywaæ
VAR$Y <- VAR$Y1 <- VAR$pays
   #class(Data.ref[, VARY]) ; table(Data.ref[, VARY]) ;   table(as.numeric(Data.ref[, VARY]))
   #!#   Data.ref[, VARY] = as.numeric(Data.ref[, VARY]) - 1   ## czynimy to poni¿ej w par. 6 -- tutaj dla zwrócenia uwagi
VAR$Y2                 = VAR$efficiency_binomial
VAR$Y3                 = VAR$efficiency_1       ## do utworzenia
VAR$Y4                 = VAR$efficiency_groups
VAR$Y5                 = VAR$goodbad
###################•°

OBJECTS = sort(union(OBJECTS, "VAR"))  ## VAR is a list now with elements being single names of key variables (not vectors of names of variables)
                                #, ls(pattern='^VAR')))
     ## setdiff(OBJECTS, ls(pattern='^VAR'))

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

   #  dpd_k <- sqlQuery(ch, paste0("SELECT id_produkt , dpd_k from akasprzyk.", TABELA))
   #  dpd_sad <- sqlQuery(ch, paste0("SELECT id_produkt , dpd_sad from akasprzyk.", TABELA))
   #  dpd_kom <- sqlQuery(ch, paste0("SELECT id_produkt , dpd_kom from akasprzyk.", TABELA))
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
   	##	data.ref_Y[ is.na(data.ref_Y[, "suma_wplat_mo"]), "suma_wplat_mo" ] = 0

   	# table(data.ref_Y[, "suma_wplat_mo"] == 0, useNA = "ifany")

   	#########################################•°
   	## zmienna nazwowa ID -- pozosta³e zmienne nazwowe w p.7.

      VAR$id

   	#########################################•°

      rownames(data.ref_y) = data.ref_y[, VARid]
   	names.ref_y = names(data.ref_y)

      Data.ref = update.df( Data.ref , data.ref_y , variables = names.ref_y , action = "append" )


########################################################################################################################•°
## 6. ZMIENNE POCHODNE #################################################################################################•°
##    Powinny byæ przewidziane w E1 a nawet wczeœniej (SQL), jednak nie wszystko da siê przewidziec...
##

#! WA¯NE!!!
#!    condlists$variables_new = list() ; class(condlists$variables_new) = c("condlist", "list")
#!    variables$newmod = character(0)


   ###################################################################################################•°
   ## SZABLON, np. ###################################################################################•°
   with(Data.ref, plot.variable(v1/v2) )
   with(Data.ref, { windows();coldef.black(); plot(v2, v1)} )

condlists$variables_new[["vx"]] = list(  ## zob. pomoc w condapply()  (condapply.r)
                                 conditions = quote("all")
                               , inputs = c("v1", "v2", "..")
                               , substitute = quote( f(a, b,..) )
                               , plot = TRUE
                               )

   vx = condapply( Data.ref , condlists$variables_new["vx"] , return.all = FALSE )

      with(datfram, plot.variable(vx) )
      with(datfram, plot.variable(vx[vx < 10]) )

   #!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
   ## jeœli OK (po skonstruowaniu WSZYSTKICH dodatkowych zmiennych)
   Data.ref = condapply( Data.ref , condlists$variables_new ) ; rm(vx)
   #!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
   ###################################################################################################•°


###################################################################################################•°
## 6.1 VARdebt ####################################################################################•°

VARdebt = "saldo_start_sad_k"      #! to jest logx(saldo_stop_0_k,.1)
#!#!#!#!#!#!#!#!#!#
Data.ref = update.df(Data.ref, data.ref, id=0, variable=VARdebt, action="append")
#!#!#!#!#!#!#!#!#!#
   VARdebt0 <- VARdebt1 <- "saldo_start_sad_k.1"    #! to jest oryginalne  saldo_stop_0_k
      plot.variable(Data.ref[, VARdebt1])   ## OK

###################################################################################################•°
## 6.2 VARefficiency_1 ############################################################################•°

VARefficiency_1 = "skutecznosc_1_poly_k"  ## skutecznoœæ przyciêta do 1
VARY1   = VARefficiency

condlists$variables_new[[VARefficiency_1]] = list(
                     variables = c()
                   , conditions = quote("all")
                   , inputs = c(VARY1)
                   #, substitute = body(function(){ max.debt_pay <- pmax(a, b) ; fails <- max.debt_pay - b ; round(cbind(b, fails)) })
                   , substitute = quote(pmin(a, 1))
                   , isolate = FALSE
                   , plot = TRUE
      )

   condapply( Data.ref , condlists$variables_new[VARefficiency_1] , return.all = NULL )

###################################################################################################•°
## 6.3 VARefficiency_binomial #####################################################################•°

VARefficiency_binomial = "skutecznosc_binomial_poly_k"

condlists$variables_new[[VARefficiency_binomial]] = list(
                     variables = c()
                   , conditions = quote("all")
                   , inputs = c(VARdebt0, VARY)
                   #, substitute = body(function(){ max.debt_pay <- pmax(a, b) ; fails <- max.debt_pay - b ; round(cbind(b, fails)) })
                   , substitute = quote({ max.debt_pay <- pmax(a, b) ; fails <- max.debt_pay - b ; round(cbind(b, fails)) })
                   , isolate = FALSE
                   #, plot = TRUE
      )

   datfram = condapply( Data.ref , condlists$variables_new[VARefficiency_binomial] , return.all = FALSE )
   head(datfram, 50)

   with(datfram, {   vx <- skutecznosc_binomial_poly_k ; skutecznosc <- vx[, 1] / rowSums(vx)
                     plot.variable(skutecznosc)
                     plot.variable(skutecznosc[skutecznosc<.2])
                  } )

###################################################################################################•°
## 6.4 skutecznosc_grupy ##########################################################################•°

VARefficiency_groups <- "skutecznosc_grupy_poly_k"

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

datfram = condapply( Data.ref , condlists$variables_new[VARefficiency_groups] , return.all = FALSE ) #, as.factor=TRUE)
        table(datfram[, VARefficiency_groups], useNA="ifany")
vx = datfram[, VARefficiency_groups]

 ( table.number.VARY1groups = as.table(tapply(Data.ref[, VARdebt0], vx, length)) )
 ( table.debt.VARY1groups = as.table(tapply(Data.ref[, VARdebt0], vx, sum)) )
 ( table.pay.VARY1groups = as.table(tapply(Data.ref[, VARY], vx, sum)) )

 windows(height=4, width=12); par.my(); par(mfrow=c(1, 2))
 barplot(table.debt.VARefficiency_groups);title("debt");
 barplot(table.pay.VARefficiency_groups);title("payments")

## inaczej
agg.glm = aggregate( as.formula(paste0(VARdebt0, "~", VARefficiency_groups))
				, FUN = function(x){ y = c( sum(x), length(x), max(x), mean(x) , min(x) ) ; names(y) = c("sum", "number", "max", "mean", "min") ; y }
 				, data = update.df(datfram, Data.ref, id=0, variable=VARdebt0)
				#, simplify = FALSE
				)
format( agg.glm  , scientific = FALSE	 , digits = 6 	 #, nsmall = 1
		)

###################################################################################################•°
## 6.5 czy_dobra ##################################################################################•°

VARgoodbad <- "czy_dobra"

condlists$variables_new[[VARgoodbad]] = list(
                           variables = c()
                        ,  conditions = "all"
                        ,  inputs = c(VARefficiency_1)
                        ,  substitute = quote( a == 1 )
                        , isolate = FALSE
                        , plot = TRUE
                        )

  dataref <- condapply( Data.ref , condlists$variables_new[VARgoodbad] , return.all = FALSE )
      table(dataref[VARgoodbad])

#  Data.ref <- condapply( Data.ref , condlists$variables_new[VARgoodbad] , return.all = TRUE )
#      table(Data.ref[VARgoodbad])

###################################################################################################•°
## NA KONIEC (po skonstruowaniu wszystkich zmiennych) #############################################•°

#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
## jeœli OK to
cbind(unlist(mget(ls(pattern='^VAR'))))
variables$newmod = setdiff(unlist(mget(ls(pattern='^VAR'))), variables$ref)  ##  0 ok!
                           unlist(VAR)
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#

Data.ref = condapply( Data.ref , condlists$variables_new )
##                        id    sum
##                      ...
## czy_dobra               5  64001

   rm(dataref, vx)

   nulls0.table(Data.ref)

###################################################################################################•°
   save.image(file=FILEmodel.environment)           ## NIE NADPISZ BEZMYŒLNIE !!!
###################################################################################################•°


########################################################################################################################•°
## 7. GRUPY ZMIENNYCH ##################################################################################################•°
## dziedziczymy z E1 ale musimy zaktualizowaæ (wykluczyæ zmienne odrzucone w E1)  ###########################•°

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

colnames.class(Data.ref, class="factor")

   cat(intersect(variables$ref1$id , variables$ref), sep = "\"\n,\"")

variables$id = c(  "id_produkt_k"
, "id_produkt_bank_k"
, "id_dluznik_k"
, "id_bank_k"
, "id_paczka_k"
, "id_bank_paczka_k"
, "id_nazwa_finansowa_k"
, "nazwa_finansowa_k"
, "nazwa_biznesowa_k"
, "typ_klienta_k"
, "grupa_portfela"
, "podmiot_kupujacy"
)

  setdiff(variables$id, variables$ref)   ## 0

###############################################################################•°
## B. inne

variables$regressors  ## ustalone w E1 - w pliku z etapu obróbki danych  dane_referencyjne_YYMMDD_1.r
                  ## tak jak i poni¿sze grupy zmiennych:
   intersect(variables$regressors, variables$id)
   setdiff(variables$ref, union(variables$regressors, variables$id))

variables$regressors_poor

##
( variables$dates = grep('^data', variables$ref, value=TRUE) )    ## tu nie ma juz dat
( variables$saldo = grep('^saldo', variables$ref, value=TRUE) )
   variables$ref1$full
   variables$ref1$tofill

## pomocnicze grupy zmiennych
(variables$dt = grep('^dt', variables$ref, value=TRUE))
(variables$dp = grep('^dp', variables$ref, value=TRUE))
(variables$czy = grep('^czy', variables$ref, value=TRUE))
(variables$kat = grep('^kat', variables$ref, value=TRUE))
(variables$kwota = grep('^kwota', variables$ref, value=TRUE))

(variables$ile = grep('^ile', variables$ref, value=TRUE))
(variables$factor = colnames.class(Data.ref, "factor"))
   cbind(sapply(Data.ref[, intersect(variables$ile, variables$factor), drop=FALSE], function(x){length(unique(x))}))
   ############################
   ##   ile_wplat_pol_k                   3
   ##   ile_wplat_przed_pol_k             3
   ##   ile_wiz_pol_k                     2
   ##   ile_list_pol_k                    4
   ##   ile_list_przed_pol_k              5
   ##   ile_ugoda_pol_k                   2
   ##   ile_ugoda_rata_zaplacone_pol_k    2
   ##   ile_dekl_pol_k                    3
   ##   ile_dekl_zaplacone_pol_k          3
   ##   ile_kancelaria_pol_k              4

(variables$wplaty = grep('^wplaty', variables$ref, value=TRUE))
(variables$skutecznosc = grep('^skutecznosc', variables$ref, value=TRUE))

#cbind(unlist(mget(ls(pattern='^VAR'))))
 VAR

###################################################################################################•°
   save.image(file=FILEmodel.environment)           ## NIE NADPISZ BEZMYŒLNIE !!!
###################################################################################################•°


########################################################################################################################•°
## 8. SELEKCJA REKORDÓW ################################################################################################•°
##
## Niniejszy model bêdzie oparty na podzbiorze ca³oœci danych - tylko dane z  dpd_start_k

Data.ref1 <- Data.ref ## kopia bezpieczeñstwa
Data.ref1.nas <- Data.ref.nas

##
condlists$selection = list( "dpd_start_k" = list( conditions = quote(x) , substitute = "idx" , logs = 1 )
                          #, "conditem" = list( ... )
                          )
condapply( Data.ref.nas, condlists$selection, logs="selection.df", return.all=NULL )
    table(selection.df$sum..==0)
Data.ref     <- Data.ref[selection.df$sum..==0,]
Data.ref.nas <- Data.ref.nas[selection.df$sum..==0,]

dim(Data.ref)     ## 64001   139
(nt <- nulls0.table(Data.ref))
       nulls0.table(data.ref[rownames(Data.ref), variables$regressors])  ## ALE NA s¹ ju¿ uzupe³nione RF w E1

########################################################################################################################•°
## 9. SZYBKI PRZEGL¥D ##################################################################################################•°

	attach(Data.ref)     ##
   ## po X zmiennych

   variables$tosee = setdiff( variables$ref , union(variables$id, variables$dates) )

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
      margins.my();coldef.black();par(mfrow=c(rows, cols))
      for(k in ((rows*cols*(pg-1))+1):min((rows*cols*pg), length(variables$tosee))){
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

###################################################################################################•°
   save.image(file=FILEmodel.environment)           ## NIE NADPISZ BEZMYŒLNIE !!!
###################################################################################################•°


########################################################################################################################•°
########################################################################################################################•°
## II. PRESELEKCJA ZMIENNYCH
########################################################################################################################•°

########################################################################################################################•°
## 10. Random Forest 0 -- wstêpna ocena istotnosci zmiennych  ###########################################################•°

(Ynames_RF <- Ynames[1:3])

RF <- list("0"=list(), "1"=list())
RF$exec_time = list()
for(type in Ynames_RF){
   RF[["0"]][[type]] <- NA
   RF[["1"]][[type]] <- NA
   RF$exec_time[["0"]][[type]] <- list()
}

idx = sample(1:dim(Data.ref)[1], 10000)

# for(type in Ynames_RF){  #1
   (type = Ynames_RF[1])   ## pêtla rêczna -- ka¿da iteracja trwa kilkanaœcie minut (a mo¿e i d³u¿ej)
      ptm <- proc.time()
RF[["0"]][[type]] = randomForest( y = Data.ref[idx, get(paste0("VAR", type))] , x = Data.ref[idx, variables$regressors] , importance=TRUE)
      (RF$exec_time[["0"]][[type]] <- proc.time()-ptm)
windows(width=16, height=7);varImpPlot(RF[["0"]][[type]], main = paste0(type, " [0]"))

#} ## END of #1

varImp = list("0"=NA, "1"=NA)

varImp[["0"]] = data.frame( pays = I(apply(RF[["0"]][["pays"]]$importance, 2, function(x){order(x, decreasing=TRUE)}))
                   , efficiency = I(apply(RF[["0"]][["efficiency"]]$importance, 2, function(x){order(x, decreasing=TRUE)}))
                   , efficiency_1 = I(apply(RF[["0"]][["efficiency_1"]]$importance, 2, function(x){order(x, decreasing=TRUE)}))
                   )

varImp[["0"]]$pays[, 1]
varImp[["0"]]$efficiency[, 1]
varImp[["0"]]$efficiency_1[, 1]

###################################################################################################•°
   save.image(file=FILEmodel.environment)           ## NIE NADPISZ BEZMYŒLNIE !!!
###################################################################################################•°


########################################################################################################################•°
## 11. Eliminacja zmiennych skorelowanych  ##############################################################################•°

###################################################################################################•°
## A. wektory zmiennych  ##########################################################################•°

regressors = list()
regressors$final = list()

###############################################################################•°
## regressors$common  -- Y = VARY = "kwota_wplat_poly_k"

cat(variables$regressors[varImp[["0"]]$pays[, "IncNodePurity"]], sep="\"\n,\"")  ## the higer the more important (according to RF)  -- for numeric y, eg. payments
#cat(variables$regressors[varImp$pays[, "MeanDecreaseAccuracy"]], sep="\"\n,\"")  ## the higer the more important (according to RF)  -- for factor y, eg. {0, 1} "czy_..."

regressors$common = list()

regressors$common[[1]] = c(  #"saldo_start_k_odsetki"
#, "saldo_start_pol_k"
"saldo_start_sad_k"
, "saldo_start_k_kapital"
, "dpw_pol_k"
#, "dtu"
, "saldo_start_k_koszty"
, "dpo_start_k"
, "prop_koszt_do_saldo_start_przed_pol_k"
, "wiek"
#, "kwota_wplata_o_przed_pol_k"
, "dp_tel_pol_k"
, "kwota_wplat_przed_pol_k"
, "dp_list_pol_k"
, "dp_dekl_pol_k"
, "dpd_start_k"
#, "dpd_stop_pol_k"
#, "dp_ugoda_pol_k"
, "dp_wiz_pol_k"
#, "kwota_wplata_o_pol_k"
#, "skutecznosc_przed_pol_k"
, "prop_koszt_do_saldo_start_pol_k"
, "dzien_tel_pol_k"
, "dt_pol_k"
#, "ile_list_przed_pol_k"
, "wojewodztwo_pol_k"
, "ile_pglos_pol_k"
#, "kwota_wplata_p_pol_k"
, "czas_polaczen_sek_razem_pol_k"
, "dzien_wplata_pol_k"
, "dzien_dekl_pol_k"
#, "skutecznosc_pol_k"
, "koszt_przed_pol_k"
, "kwota_wplat_pol_k"
, "dzien_ugoda_pol_k"
#, "koszt_pol_k"
#, "ile_telw_pol_k"
#, "grupa_portfela"
, "ile_telp_pol_k"
, "dzien_list_pol_k"
#, "dzien_wiz_pol_k"
#, "ile_tel_pol_k"
#, "plec"
#, "ile_wplat_przed_pol_k"
#, "kwota_dekl_o_pol_k"
#, "ile_list_pol_k"
#, "prop_ile_dekl_dt_pol_k"
#, "rank_rodzaj_telefon_pol_k"
, "kwota_sr_dekl_pol_k"
#, "prop_kwota_dekl_saldo_start_pol_k"
#, "kwota_dekl_pol_k"
, "kwota_wplat_dekl_pol_k"
, "dt_dekl_pol_k"
#, "prop_kwota_wplat_dekl_saldo_start_pol_k"
#, "ile_dekl_zaplacone_pol_k"
#, "prop_kwota_wplat_dekl_pol_k"
, "rank_rodzaj_adres_pol_k"
, "czy_emeryt"
, "ile_wplat_pol_k"
#, "czy_aktywny_telefon_pol_k"
#, "czy_tel_pol_k"
#, "czy_list_pol_k"
#, "ile_dekl_pol_k"
#, "czy_wplata_pol_k"
#, "prop_kwota_ugoda_saldo_start_pol_k"
#, "kwota_sr_rata_ugoda_o_pol_k"
, "kwota_ugoda_pol_k"
#, "prop_ile_dekl_zaplacone_pol_k"
#, "kwota_ugoda_o_pol_k"
#, "saldo_start_ugoda_o_pol_k"
, "ile_wiz_pol_k"
#, "czy_wiz_pol_k"
#, "czy_dekl_pol_k"
#, "ile_ugoda_rata_pol_k"
, "prop_ile_ugoda_rata_zaplacone_pol_k"
#, "ile_ugoda_pol_k"
#, "ile_ugoda_rata_zaplacone_pol_k"
#, "dt_ugoda_pol_k"
#, "rodzaj_strony"
#, "czy_ugoda_pol_k"
#, "kwota_wplat_ugoda_pol_k"
#, "prop_kwota_wplat_ugoda_saldo_start_pol_k"
#, "prop_kwota_wplat_ugoda_pol_k"
)    # ...

###############################################################################•°
## regressors$poisson  - Y = VARY = "kwota_wplat_poly_k"
##    ==    .$common

 regressors$poisson = regressors$common

###############################################################################•°
## regressors$binomial  - Y = VARefficiency_1 = "skutecznosc_1_poly_k" przyciête do 1
      plot.variable(Data.ref[, VARefficiency_1])

regressors$binomial[[1]] = list()

cat(variables$regressors[varImp[["0"]]$efficiency_1[, "IncNodePurity"]], sep="\"\n,\"")  ## the higer the more important (according to RF)  -- for numeric y, eg. payments

regressors$binomial[[1]] = c(
   "saldo_start_sad_k"
#, "saldo_start_pol_k"
, "saldo_start_k_kapital"
, "dpo_start_k"
#, "saldo_start_k_odsetki"
, "wiek"
#, "dtu"
, "dpw_pol_k"
, "dp_tel_pol_k"
#, "prop_koszt_do_saldo_start_pol_k"
, "dpd_start_k"
, "dp_dekl_pol_k"
, "prop_koszt_do_saldo_start_przed_pol_k"
, "wojewodztwo_pol_k"
#, "dpd_stop_pol_k"
, "skutecznosc_pol_k"
, "dp_list_pol_k"
#, "dp_wiz_pol_k"
, "dp_ugoda_pol_k"
, "saldo_start_k_koszty"
, "dzien_tel_pol_k"
, "dt_pol_k"
#, "czas_polaczen_sek_razem_pol_k"
, "dzien_wplata_pol_k"
, "dzien_dekl_pol_k"
, "koszt_pol_k"
#, "grupa_portfela"
, "dzien_list_pol_k"
#, "ile_telw_pol_k"
, "skutecznosc_przed_pol_k"
#, "dzien_wiz_pol_k"
#, "plec"
, "ile_pglos_pol_k"
, "dzien_ugoda_pol_k"
#, "ile_list_przed_pol_k"
#, "ile_tel_pol_k"
, "rank_rodzaj_telefon_pol_k"
, "czy_emeryt"
, "koszt_przed_pol_k"
#, "kwota_wplat_pol_k"
, "ile_telp_pol_k"
#, "kwota_wplata_o_pol_k"
#, "kwota_wplata_o_przed_pol_k"
#, "kwota_wplat_przed_pol_k"
#, "ile_list_pol_k"
, "czy_aktywny_telefon_pol_k"
#, "prop_kwota_dekl_saldo_start_pol_k"
, "prop_ile_dekl_dt_pol_k"
#, "kwota_wplata_p_pol_k"
, "prop_kwota_wplat_dekl_saldo_start_pol_k"
#, "kwota_sr_dekl_pol_k"
#, "kwota_dekl_o_pol_k"
#, "kwota_dekl_pol_k"
#, "prop_kwota_wplat_dekl_pol_k"
#, "ile_wplat_przed_pol_k"
#, "rank_rodzaj_adres_pol_k"
#, "kwota_wplat_dekl_pol_k"
, "dt_dekl_pol_k"
, "prop_kwota_ugoda_saldo_start_pol_k"
#, "czy_tel_pol_k"
#, "kwota_sr_rata_ugoda_o_pol_k"
, "ile_dekl_pol_k"
, "ile_wplat_pol_k"
#, "czy_list_pol_k"
#, "kwota_ugoda_o_pol_k"
#, "rodzaj_strony"
#, "czy_wplata_pol_k"
#, "kwota_ugoda_pol_k"
#, "saldo_start_ugoda_o_pol_k"
#, "ile_dekl_zaplacone_pol_k"
#, "prop_ile_dekl_zaplacone_pol_k"
#, "czy_dekl_pol_k"
#, "prop_kwota_wplat_ugoda_saldo_start_pol_k"
#, "prop_kwota_wplat_ugoda_pol_k"
#, "ile_ugoda_rata_pol_k"
#, "czy_wiz_pol_k"
#, "dt_ugoda_pol_k"
#, "ile_wiz_pol_k"
, "prop_ile_ugoda_rata_zaplacone_pol_k"
#, "ile_ugoda_rata_zaplacone_pol_k"
#, "kwota_wplat_ugoda_pol_k"
#, "czy_ugoda_pol_k"
#, "ile_ugoda_pol_k"
)


###################################################################################################•°
## B. Korelacje (grafy) - zmienne ci¹g³e ##########################################################•°
rho =.3 #  =.5  =.3   # wa¿ne jest tylko r=.7
 #windows()
 #! ponizsza funkcja znajduje korelacje jedynie dla zmiennych liczbowych !!!   I dobrze!
 (graf0 <- graf(zmienne.graf = regressors$binomial[[1]], dane = Data.ref,
      r=rho,
      zmienne.bez.null = regressors$binomial[[1]],
      target = VARY, ver = 2,               #! bledy dla  ver=1 ## wysypuje sie gdy nie ma zadnych korelacji (powyzej zadanego r)
      metoda = list( c(1) , c("pearson", "kendall", "spearman") ) ))

 graf0$`korelacja zmiennych zaleznych`[c(5, 6, 8, 9), c(5, 6, 8, 9)]

#######################################•°
## sprz¹tanie
rm(Data.graf, Y, nazwy, wspzm)

###################################################################################################•°
## C. chi^2 - zmienne kategoryczne ################################################################•°
##
     colnames.class( Data.ref[, variables$regressors] , "factor" )
(regs_factors <- colnames.class( Data.ref[, regressors$binomial[[1]]] , "factor" ))
## Ponizej testujemy hurtowo hipoteze ze zmienne kategoryczne (factors) sa niezalezne.
## Odrzucamy ja dla p.value<0.05.
## Postêpujemy tak jak dla zmiennych numerycznych tj. odrzucamy zmienne a¿ bêdzie brak 'korelacji'
##  na poczatek te ktore koreluja z duza iloscia innych
chisqs = chisq.multi.test(Data.ref[, regressors$binomial[[1]]])
  ## nie przejmujemy sie ostrzezeniami
chisqs$cor.pairs ## "skorelowane" (a raczej zalezne) pary zmiennych kategorycznych, tj. tylko te pary dla których p<0.05 (tak male P(para zmiennych niezale¿na))

#######################################•°
## dok³adniej

with(Data.ref, table(grupa_portfela, wojewodztwo_pol_k))

## wszystkie kategoryczne - ka¿dy z ka¿dym (ale kiedy zostaje ju¿ tylko kilka z nich)
(regs_factors <- colnames.class( Data.ref[, regressors$binomial[[1]]] , "factor" ))
for(j in 1:(length(regs_factors)-1)){ nam_j = regs_factors[j]
for(k in (j+1):length(regs_factors)){ nam_k = regs_factors[k]
      dd = chisqs$cor.pairs
      idx = (dd$var1 == nam_j & dd$var2 == nam_k) | (dd$var1 == nam_k & dd$var2 == nam_j)
      print( dd[ idx , "p.value"] )
      print({ tt <- table(Data.ref[, nam_j], Data.ref[, nam_k], deparse.level=1) ;
              names(attr(tt, "dimnames")) <- c(nam_j, nam_k)
              tt
            })
      cat("\n")
}}

## wykresik
with( Data.ref ,{ var.name = "ile_wplat_pol_k" ; yy = VARY
   formula_0 = paste0(yy , " ~ ", var.name)
   model = lm(formula(formula_0))
   summary(model)
   ##
   windows();par.my();par(mfrow = c(2, 2))
   plot(model, main = formula_0)
   par(mfrow = c(1, 1))
##
table(get(var.name), useNA="ifany")
##
#windows(width = 4  , height = 8);par.my();par(mfrow = c(2, 1))
windows(width = 4  , height = 4);par.my()
plot.means.fit(model , cex.means = .7 , main = formula_0 , ylab = yy , xlab=var.name , labels = levels(get(var.name))) ## my function
windows(width = 4  , height = 4);par.my()
barplot.confint(get(var.name), get(yy))   ## my function -- independent of external model thus creates its own
})

###################################################################################################•°
   save.image(file=FILEmodel.environment)           ## NIE NADPISZ BEZMYŒLNIE !!!
###################################################################################################•°


########################################################################################################################•°
## 12. Random Forest 1 -- ponowna ocena istotnosci zmiennych  ##########################################################•°
##                        tylko wybrane zmienne               ##########################################################•°

Ynames_RF
idx = sample(1:dim(Data.ref)[1], 10000)

# for(type in Ynames_RF){  #1
  (type <- Ynames_RF[3])   ## pêtla rêczna -- ka¿da iteracja trwa kilkanaœcie minut (a mo¿e i d³u¿ej)
      ptm <- proc.time()
RF[["1"]][[type]] = randomForest( y = Data.ref[idx, get(paste0("VAR", type))] , x = Data.ref[idx, variables$regressors] , importance=TRUE)
      ( RF$exec_time[["1"]][[type]] <- proc.time() - ptm )
windows(width=16, height=7);varImpPlot( RF[["1"]][[type]] , main = paste0(type, " [1]"))
#} ## END of #1

varImp[["1"]] = data.frame( pays = I(apply(RF[["1"]][["pays"]]$importance, 2, function(x){order(x, decreasing=TRUE)}))
                   , efficiency = I(apply(RF[["1"]][["efficiency"]]$importance, 2, function(x){order(x, decreasing=TRUE)}))
                   , efficiency_1 = I(apply(RF[["1"]][["efficiency_1"]]$importance, 2, function(x){order(x, decreasing=TRUE)}))
                   )

varImp[["1"]]$pays[, 1]
varImp[["1"]]$efficiency[, 1]
varImp[["1"]]$efficiency_1[, 1]

format(object.size(RF), units="Mb")

###################################################################################################•°
   save.image(file=FILEmodel.environment)           ## NIE NADPISZ BEZMYŒLNIE !!!
###################################################################################################•°



########################################################################################################################•°
########################################################################################################################•°
## III. BUDOWA MODELU
########################################################################################################################•°

########################################################################################################################•°
## 11. Intro - podstawowe obiekty  #####################################################################################•°

###########################################################•°
#!#!  Listy:
###########################################################•°

FAMILY = c("poisson", "binomial")
   #
formulas = list()
formulas$final = list()
   #
models = list()
predictions = list()
   #
summaries = list()
summaries_agg = list()
   #
summary_k_0 = list()
summary_k_0$raw = NA #(data.frame, panova)
summary_k_0$agg = list()
summary_k_0$agg$pars = NA #(data.frame, panova.agg)    # BUT : summaries$binary[[k]]$whole$agg$pars  = update.df( summaries$binary[[k]]$folds$agg$pars , summaries$binary[[k]]$whole$raw , join = "left" , action = "append" )
summary_k_0$agg$adds = NA #(data.frame, panova.agg.adds)
   #
summary_k = list()
summary_k$folds = summary_k_0
summary_k$whole = summary_k_0
   #
summaries_agg$binary$folds = NA  ## data.frame
summaries_agg$binary$whole = NA  ##
   #
for(fam in FAMILY){
   formulas[[fam]] <- list()
   summaries[[fam]] <- list()
   ## summaries[[fam]][[k]]     <- summary_k
   summaries_agg[[fam]] <- list("folds"=NA, "whole"=NA)
}
   #
str(formulas)
str(summaries)
   str(summary_k)
str(summaries_agg)

########################################################################################################################•°
## 12. MODELE  #########################################################################################################•°

########################################################################################################################•°
## 12.A. model gam(, family=binomial(link=logit))  ######################################################################•°

cbind(unlist(mget(ls(pattern='^VAR'))))

###########################################################•°
## zmienna pomocnicza do zbalansowanego podzialu Data.ref
   nulls0.table(Data.ref[, regressors$binomial[[1]]])
   VARbalanced = "czy_bez_k"
   table(Data.ref[, VARbalanced])
  #
cat(regressors$binomial[[1]], sep="\"\n,\"")

k = 6
regressors$binomial[[k]] = c( "saldo_start_sad_k"
#, "saldo_start_k_kapital"                              #4 +5
, "dpo_start_k"
, "wiek"
, "dpw_pol_k"
, "dp_tel_pol_k"
, "dpd_start_k"
, "dp_dekl_pol_k"
, "prop_koszt_do_saldo_start_przed_pol_k"
, "wojewodztwo_pol_k"
, "skutecznosc_pol_k"
, "dp_list_pol_k"
, "dp_ugoda_pol_k"
#, "saldo_start_k_koszty"                            #3    +5
, "dzien_tel_pol_k"
, "dt_pol_k"
, "dzien_wplata_pol_k"
#, "dzien_dekl_pol_k"                                #3    +5
, "koszt_pol_k"
, "dzien_list_pol_k"
, "skutecznosc_przed_pol_k"
#, "ile_pglos_pol_k"                             #2
#, "dzien_ugoda_pol_k"                        #1
, "rank_rodzaj_telefon_pol_k"
, "czy_emeryt"
, "koszt_przed_pol_k"
, "ile_telp_pol_k"
#, "czy_aktywny_telefon_pol_k"                      #3     +5
#, "prop_ile_dekl_dt_pol_k"                         #3     +5
, "prop_kwota_wplat_dekl_saldo_start_pol_k"
#, "dt_dekl_pol_k"                            #1
, "prop_kwota_ugoda_saldo_start_pol_k"
, "ile_dekl_pol_k"
, "ile_wplat_pol_k"
#, "prop_ile_ugoda_rata_zaplacone_pol_k"         #2
)

cat(regressors$binomial[[3]], sep=")\"\n,\"s(")
k=10
regressors$binomial[[k]] = c( "s(saldo_start_sad_k)"
, "s(saldo_start_k_kapital)"
, "s(dpo_start_k)"
, "wiek"                                         #6-s
, "s(dpw_pol_k)"
, "dp_tel_pol_k"                                 #6-s
, "s(dpd_start_k)"
, "s(dp_dekl_pol_k)"
, "s(prop_koszt_do_saldo_start_przed_pol_k)"
, "wojewodztwo_pol_k"
, "s(skutecznosc_pol_k)"
, "s(dp_list_pol_k)"
, "s(dp_ugoda_pol_k)"
, "s(saldo_start_k_koszty)"
, "s(dzien_tel_pol_k)"
, "s(dt_pol_k)"
, "s(dzien_wplata_pol_k)"
#, "dzien_dekl_pol_k"                             #6-s 7
, "s(koszt_pol_k)"
, "s(dzien_list_pol_k)"
, "s(skutecznosc_przed_pol_k)"
, "rank_rodzaj_telefon_pol_k"
, "czy_emeryt"
, "s(koszt_przed_pol_k)"
, "s(ile_telp_pol_k)"
#, "czy_aktywny_telefon_pol_k"                         #8
, "prop_ile_dekl_dt_pol_k"                        #7-s
, "s(prop_kwota_wplat_dekl_saldo_start_pol_k)"
#, "prop_kwota_ugoda_saldo_start_pol_k"           #6-s 7
#, "ile_dekl_pol_k"                                       #9
, "ile_wplat_pol_k"
)

      #for(k in 1:7){summaries$binary[[k]] = summary_k}

summaries$binomial[[k]] = summary_k
formulas$binomial[[k]] = paste0( VARefficiency_binomial , " ~ " , paste(regressors$binomial[[k]] , collapse=" + " ))
rm(summary.df_k) ## rm(panova.agg.adds)
for(j in 1:30){
   Data.ref_kj <- split.rand.portion(Data.ref, split=TRUE, portion=.8)#, variable=VARbalanced)
      model_kj <- gam( formula(formulas$binomial[[k]]) , family=binomial(link=logit) , data=Data.ref_kj )
   summary.gam.panova( model_kj , what=c("coef", "pval") , sstars=TRUE
                     , append.to="summary.df_k" , add=c("mse", "aic", "gini", "auc", "p.mww") , print=FALSE )
}
summaries$binomial[[k]]$folds$raw = summary.df_k

      #for(k in 1:7){summaries$binary[[k]]$folds$raw = summaries.a$binary[[k]]}


summary.df_k.agg = summary.gam.panova.agg( summary.df_k
   , FUN = list( "coeffs" = function(x){c("mean" = mean(x))}
               , "coeffs" = function(x){signs.table(x)}
               , "Pr(>F)" = function(x){c("mean" = mean(x))}
               , "Pr(>F)" = function(x){c("sstars" = signif.stars(mean(x)))}
               , "sstars" = function(x){sstars.table(x) }
               )
   , add.colnames = c(TRUE, FALSE, TRUE, TRUE, FALSE))
(summaries$binomial[[k]]$folds$agg$pars = summary.df_k.agg)

   summaries$binomial[[9]]$folds$agg$pars

      ## for(k in 1:7){summaries$binomial[[k]]$folds$agg$pars = summaries.agg.a$binomial[[k]]}

summary.gam.panova.agg.adds( summary.df_k
      , FUN = list( "MSE"  = function(x){c("min"=min(x), "mean"=mean(x), "max"=max(x))}
                  , "AIC"  = function(x){c("min"=min(x), "mean"=mean(x), "max"=max(x))}
                  , "Gini" = function(x){c("min"=min(x), "mean"=mean(x), "max"=max(x))}
                  , "AUC"  = function(x){c("min"=min(x), "mean"=mean(x), "max"=max(x))}
                  , "p.mww" = function(x){c("mean"=mean(x))}
                  )
      , add.colnames = TRUE
      , append.to = "panova.agg.adds" , row = k
      , print=TRUE
)
summaries$binomial[[k]]$folds$agg$adds = panova.agg.adds[k,]
summaries_agg$binomial$folds = panova.agg.adds

      ## for(k in 1:7){summaries$binomial[[k]]$folds$agg$adds = summaries.agg.adds$binomial[k,]}
  # panova.agg.adds <- panova.agg.adds[1:3,]

windows();par.my()
split.stats( predict(model_kj, type="response") , Data.ref_kj[, VARefficiency_1] == 1  , plot = TRUE )

rm(panova.agg.adds)

###############################################################################•°
## Modele na ca³oœci danych w oparciu o wyniki badañ na podzbiorach danych

   rm(whole.panova.agg.adds)
#for(k in 6:length(formulas$binomial)){ #1   # k=1
   print(k)
   model_k = gam(formula(formulas$binomial[[k]]), family=binomial(link=logit), data=Data.ref)
   summaries$binomial[[k]]$whole$raw = summary.gam.panova(model_k, what=c("coef", "pval"), sstars=TRUE, add=c("mse", "aic", "gini", "auc", "p.mww"))
      #
   summaries$binomial[[k]]$whole$agg$pars = update.df( summaries$binomial[[k]]$folds$agg$pars , summaries$binomial[[k]]$whole$raw , join = "left" , action = "append" )
      #
   summary.gam.panova.agg.adds( summaries$binomial[[k]]$whole$raw
      , FUN = list( "MSE"   = function(x){c(mean(x))}
                  , "AIC"   = function(x){c(mean(x))}
                  , "Gini"  = function(x){c(mean(x))}
                  , "AUC"   = function(x){c(mean(x))}
                  , "p.mww" = function(x){c(mean(x))}
                  )
      , add.colnames = TRUE  , row = k
      , append.to = "whole.panova.agg.adds"
   )
   summaries$binomial[[k]]$whole$agg$adds = whole.panova.agg.adds[k,]
      #
   windows();par.my()
   split.stats( predict(model_k, type="response")
               , Data.ref[, VARefficiency_1] == 1      #!#!#!#!!!!!!
               , plot = TRUE )
      #
#}  ## END of #1
      #
summaries_agg$binomial$whole = whole.panova.agg.adds ;



   #summaries.agg$binary$whole = summaries.agg.adds$binary_whole

summaries.agg
summaries$binomial[[5]]

#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
   save.image(file=FILEmodel.environment)           ## NIE NADPISZ BEZMYŒLNIE !!!
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#

###############################################################################•°
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
#!#  Wybieramy model 9  #!#!#!#!#!#!#!#
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#

regressors$final$binomial = list()
 regressors$final$binomial$s = regressors$binomial[[9]]
 regressors$final$binomial$x = gsub('s\\(','', gsub(')','', regressors$final$binomial$s))
formulas$final$binomial   = formulas$binomial[[9]]
models$binomial = gam(formula(formulas$final$binomial), family=binomial(link=logit), data=Data.ref)

windows();par.my()
split.stats( predict( models$binomial , type="response") , Data.ref[, VARefficiency_1] == 1 , plot = TRUE )

###############################################################################•°
## odzyski faktyczne a przewidywane z binomial

predictions = list()

   predictions$binomial = predict(models$binomial, type="response")*Data.ref[, VARdebt1]
   	sum(predictions$binomial)
   	sum(Data.ref[, VARpays])

   windows(height = 10, width = 5 ); par.my()
   plot( Data.ref[, VARpays] ~ predictions$binomial , pch='.' , col = "yellow" , ylab = VARpays )
     	title("wp³aty rzeczywiste vs przewidywane binomial")
   	lm.binomial = lm(Data.ref[, VARpays] ~ predictions$binomial)
   	abline(lm.binomial, col="white");
   	text(	  x = max(predictions$binomial)/2 , y = max(Data.ref[, VARpays])*.9
   			, labels = paste0( "y = " , paste( paste0( round( lm.binomial$coef, 3 ) , c("", " * x") ), collapse = " + ") )
   		 )
	#  abline(h=0, col="white")
	#  legend( "topleft" , legend = unique(Data.ref$grupa_portfela) , col = unique(as.numeric(Data.ref$grupa_portfela)) + 1, pch = 20 )

      #!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
      sum(cbind(1, predictions$binomial) %*% cbind(lm.binomial$coef))
      #!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#

      resids <- Data.ref[, VARpays] - predictions$binomial
   windows(height = 12, width = 6 ); par.my() ; par(mfrow=c(2, 1))
   plot( resids ~ Data.ref[, VARpays] , pch='.' , col = "yellow" , xlab = VARpays , ylab = "residua" )
   plot( predictions$binomial ~ Data.ref[, VARpays] , pch='.' , col = "yellow" , xlab = VARpays , ylab = "prediction" )

      range(resids)
   plot.variable(resids, breaks=c(-8e3,-5e3,-2e3,-1e2, 1e2, 2e3, 5e3, 1e4, 3e4), breaks.lines=TRUE)

###################################################################################################•°
   save.image(file=FILEmodel.environment)           ## NIE NADPISZ BEZMYŒLNIE !!!
###################################################################################################•°

rm(panova.agg.adds)


########################################################################################################################•°
## 12.B. model gam(, family=poisson)  ###################################################################################•°

cbind(unlist(mget(ls(pattern='^VAR'))))
FAMILY
Ynames

###########################################################•°
## zmienna pomocnicza do zbalansowanego podzialu Data.ref
#   nulls0.table(Data.ref[, regressors$poisson[[1]]])
#   VARbalanced = "czy_bez_k"
#   table(Data.ref[, VARbalanced])
  #
cat(regressors$poisson[[1]], sep="\"\n,\"")

k = 3
regressors$poisson[[k]] = c( "saldo_start_sad_k"
, "saldo_start_k_kapital"
, "dpw_pol_k"
, "saldo_start_k_koszty"
, "dpo_start_k"
, "prop_koszt_do_saldo_start_przed_pol_k"
, "wiek"
#, "dp_tel_pol_k"                       #2
, "kwota_wplat_przed_pol_k"
, "dp_list_pol_k"
, "dp_dekl_pol_k"
, "dpd_start_k"
, "dp_wiz_pol_k"
, "prop_koszt_do_saldo_start_pol_k"
, "dzien_tel_pol_k"
, "dt_pol_k"
, "wojewodztwo_pol_k"
#, "ile_pglos_pol_k"                  #1
, "czas_polaczen_sek_razem_pol_k"     #1
, "dzien_wplata_pol_k"
, "dzien_dekl_pol_k"
, "koszt_przed_pol_k"
, "kwota_wplat_pol_k"
#, "dzien_ugoda_pol_k"                  #2
, "ile_telp_pol_k"
#, "dzien_list_pol_k"                 #1
, "kwota_sr_dekl_pol_k"
, "kwota_wplat_dekl_pol_k"
, "dt_dekl_pol_k"
, "rank_rodzaj_adres_pol_k"
, "czy_emeryt"
, "ile_wplat_pol_k"
, "kwota_ugoda_pol_k"
, "ile_wiz_pol_k"
, "prop_ile_ugoda_rata_zaplacone_pol_k"
)

cat(regressors$poisson[[3]], sep=")\"\n,\"s(")

k=12
regressors$poisson[[k]] = c( "s(saldo_start_sad_k)"
, "s(saldo_start_k_kapital)"
, "s(dpw_pol_k)"
, "s(saldo_start_k_koszty)"
, "s(dpo_start_k)"
, "s(prop_koszt_do_saldo_start_przed_pol_k)"
, "wiek"                                        #4-s
, "s(kwota_wplat_przed_pol_k)"
, "dp_list_pol_k"                                                     #10-s
, "s(dp_dekl_pol_k)"
, "dpd_start_k"                                          #6-s
#, "dp_wiz_pol_k"                                             #7-s #8
, "s(prop_koszt_do_saldo_start_pol_k)"
#, "dzien_tel_pol_k"                             #4-s #5
, "s(dt_pol_k)"
, "wojewodztwo_pol_k"
, "s(czas_polaczen_sek_razem_pol_k)"
, "s(dzien_wplata_pol_k)"
#, "s(dzien_dekl_pol_k)"                                                   #11
, "s(koszt_przed_pol_k)"
, "s(kwota_wplat_pol_k)"
, "s(ile_telp_pol_k)"
, "s(kwota_sr_dekl_pol_k)"
, "s(kwota_wplat_dekl_pol_k)"                                    #9-s
#, "dt_dekl_pol_k"                                    #6                   #11
#, "rank_rodzaj_adres_pol_k"                    #4
, "czy_emeryt"
, "ile_wplat_pol_k"
#, "s(kwota_ugoda_pol_k)"                       #4
, "ile_wiz_pol_k"
#, "prop_ile_ugoda_rata_zaplacone_pol_k"        #4
)

      #for(k in 1:7){summaries$binary[[k]] = summary_k}

summaries$poisson[[k]] = summary_k
formulas$poisson[[k]] = paste0( VARYpoisson , " ~ " , paste(regressors$poisson[[k]] , collapse=" + " ))
rm(summary.df_k) ## rm(panova.agg.adds)
for(j in 1:30){
   ll_kj <- split.rand.portion(Data.ref, split=TRUE, portion=.8, envir=0)#, variable=VARbalanced)
      model_kj <- gam( formula(formulas$poisson[[k]]) , family=poisson , data=ll_kj$datfram.1 )
   summary.gam.panova( model_kj , what=c("coef", "pval") , sstars=TRUE
                     , add=c("mse", "aic", "gini", "auc", "p.mww", "r2", "cvr2") , newdata=ll_kj$datfram.0  ## CV
                     , append_to="summary.df_k"
                     , print=FALSE )
}
summaries$poisson[[k]]$folds$raw = summary.df_k

      #for(k in 1:7){summaries$binary[[k]]$folds$raw = summaries.a$binary[[k]]}


summary.df_k.agg = summary.gam.panova.agg( summary.df_k
   , FUN = list( "coeffs" = function(x){c("mean" = mean(x))}
               , "coeffs" = function(x){signs.table(x)}
               , "Pr(>F)" = function(x){c("mean" = mean(x))}
               , "Pr(>F)" = function(x){c("sstars" = signif.stars(mean(x)))}
               , "sstars" = function(x){sstars.table(x) }
               )
   , add.colnames = c(TRUE, FALSE, TRUE, TRUE, FALSE))
(summaries$poisson[[k]]$folds$agg$pars = summary.df_k.agg)

   summaries$poisson[[6]]$folds$agg$pars

      ## for(k in 1:7){summaries$binomial[[k]]$folds$agg$pars = summaries.agg.a$binomial[[k]]}

# rm(panova.agg.adds)
# for(k in 1:12){summary.df_k <- summaries$poisson[[k]]$folds$raw
summary.gam.panova.agg.adds( summary.df_k
      , FUN = list( "MSE"  = function(x){c("min"=min(x), "mean"=mean(x), "max"=max(x))}
                  , "AIC"  = function(x){c("min"=min(x), "mean"=mean(x), "max"=max(x))}
                  , "Gini" = function(x){c("min"=min(x), "mean"=mean(x), "max"=max(x))}
                  , "AUC"  = function(x){c("min"=min(x), "mean"=mean(x), "max"=max(x))}
                  , "p.mww" = function(x){c("mean"=mean(x))}
                  , "R2"    = function(x){c("min"=min(x), "mean"=mean(x), "max"=max(x))}
                  , "CV_R2" = function(x){c("min"=min(x), "mean"=mean(x), "max"=max(x))}
                  )
      , add.colnames = TRUE
      , append.to = "panova.agg.adds" #, row = k
      , print=TRUE
)
summaries$poisson[[k]]$folds$agg$adds = panova.agg.adds[k,]
# }
(summaries_agg$poisson$folds = panova.agg.adds)

      ## for(k in 1:7){summaries$binomial[[k]]$folds$agg$adds = summaries.agg.adds$binomial[k,]}
  # panova.agg.adds <- panova.agg.adds[1:3,]

windows();par.my()
split.stats( predict(model_kj, type="response") , Data.ref_kj[, VARefficiency_1] == 1  , plot = TRUE )

rm(panova.agg.adds)


###############################################################################•°
## Modele na ca³oœci danych w oparciu o wyniki badañ na podzbiorach danych

#!  rm(whole.panova.agg.adds)
#for(k in 1:length(formulas$poisson)){ #1   # k=7
   print(k)
   model_k = gam(formula(formulas$poisson[[k]]), family=poisson, data=Data.ref)
   summaries$poisson[[k]]$whole$raw = summary.gam.panova(model_k, what=c("coef", "pval"), sstars=TRUE, add=c("mse", "aic", "gini", "auc", "p.mww", "r2"))
      #
   summaries$poisson[[k]]$whole$agg$pars = update.df( summaries$poisson[[k]]$folds$agg$pars , summaries$poisson[[k]]$whole$raw , join = "left" , action = "append" )
      #
   summary.gam.panova.agg.adds( summaries$poisson[[k]]$whole$raw
      , FUN = list( "MSE"   = function(x){c(mean(x))}
                  , "AIC"   = function(x){c(mean(x))}
                  , "Gini"  = function(x){c(mean(x))}
                  , "AUC"   = function(x){c(mean(x))}
                  , "p.mww" = function(x){c(mean(x))}
                  , "R2"    = function(x){c(mean(x))}
                  )
      , add.colnames = TRUE  , row = k
      , append.to = "whole.panova.agg.adds"
   )
   summaries$poisson[[k]]$whole$agg$adds = whole.panova.agg.adds[k,]
      #
   windows();par.my()
   split.stats( predict(model_k, type="response")
               , Data.ref[, VARefficiency_1] == 1      #!#!#!#!!!!!!
               , plot = TRUE )
      #
#}  ## END of #1
      #
summaries_agg$poisson$whole = whole.panova.agg.adds ;


summaries_agg
summaries$poisson[[k]]

###################################################################################################•°
   save.image(file=FILEmodel.environment)           ## NIE NADPISZ BEZMYŒLNIE !!!
###################################################################################################•°

###############################################################################•°
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
#!#  Wybieramy model 10   #!#!#!#!#!#!#  (generalnie model do bani! lepszy binomial!)
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#

regressors$final$poisson$s = regressors$poisson[[10]]
regressors$final$poisson$x = gsub('s\\(','', gsub(')','', regressors$final$poisson$s))
formulas$final$poisson   = formulas$poisson[[10]]
models$poisson = gam(formula(formulas$final$poisson), family=poisson, data=Data.ref)

windows();par.my()
split.stats( predict( models$poisson , type="response") , Data.ref[, VARefficiency_1] == 1 , plot = TRUE ) #!!! nie ma sensu dla poisson !!!

###############################################################################•°
## odzyski faktyczne a przewidywane z binomial

   predictions$poisson = predict(models$poisson, type="response")
   	sum(predictions$poisson)
   	sum(Data.ref[, VARpays])

   windows(height = 10, width = 7 ); par.my()
   plot( Data.ref[, VARpays] ~ predictions$poisson , pch='.' , col = "yellow" , ylab = VARpays )
     	title("wp³aty rzeczywiste vs przewidywane poisson")
   	lm.poisson = lm(Data.ref[, VARpays] ~ predictions$poisson)
   	abline(lm.poisson, col="white");
   	text(	  x = max(predictions$poisson)/2 , y = max(Data.ref[, VARpays])*.9
   			, labels = paste0( "y = " , paste( paste0( round( lm.poisson$coef, 3 ) , c("", " * x") ), collapse = " + ") )
   		 )
	#  abline(h=0, col="white")
	#  legend( "topleft" , legend = unique(Data.ref$grupa_portfela) , col = unique(as.numeric(Data.ref$grupa_portfela)) + 1, pch = 20 )

      #!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
      sum(cbind(1, predictions$poisson) %*% cbind(lm.poisson$coef))
      #!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#

         names(models$poisson)
      resids <- Data.ref[, VARYpoisson] - predictions$poisson
   windows(height = 12, width = 6 ); par.my() ; par(mfrow=c(2, 1))
   plot( resids ~ Data.ref[, VARpays] , pch='.' , col = "yellow" , xlab = VARpays , ylab = "residua" )
   plot( predictions$poisson ~ Data.ref[, VARpays] , pch='.' , col = "yellow" , xlab = VARpays , ylab = "prediction" )

      range(resids)
   plot.variable(resids, breaks=c(-1.1e4,-5e3,-2e3,-1e2, 1e2, 2e3, 5e3, 1e4, 3e4), breaks.lines=TRUE)


###################################################################################################•°
   save.image(file=FILEmodel.environment)           ## NIE NADPISZ BEZMYŒLNIE !!!
###################################################################################################•°


########################################################################################################################•°
## przegl¹d
names(summaries)
names(summaries$binomial)
length(summaries$binomial)
names(summaries$binomial[[1]])
names(summaries$binomial[[1]]$folds)
class(summaries$binomial[[1]]$folds$raw)
dim(summaries$binomial[[1]]$folds$raw)
colnames(summaries$binomial[[1]]$folds$raw)
class(summaries$binomial[[1]]$folds$agg)
names(summaries$binomial[[1]]$folds$agg)
class(summaries$binomial[[1]]$folds$agg$pars)
class(summaries$binomial[[1]]$folds$agg$adds)

names(summaries_agg)
names(summaries_agg$binomial)
class(summaries_agg$binomial$folds)
class(summaries_agg$binomial$whole)


########################################################################################################################•°
########################################################################################################################•°
## IV. ZAPIS DO PLIKU
#!######################################################################################################################•°

###################################################################################################•°
## A. Cale œrodowisko

save.image(file=FILEmodel.environment, compress="xz")				## NIE NADPISZ BEZMYŒLNIE!!!

###################################################################################################•°
## B. Tylko potrzebne rzeczy

   cat(sort(ls()), sep="\n")

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
         levels.raw[[fac]] = levels(as.factor(data.ref[, fac]))
      }
      #!#!#!#!#!#!#!#!#!#!#!#!#!

   ls(pattern='^model')

OBJECTS = union(OBJECTS, ls(pattern='^model'))
OBJECTSmod = OBJECTS
OBJECTS = c(OBJECTS, "OBJECTSmod")

	save( list = OBJECTS
			, file = FILEmodel.Rdata , compress = "xz" )

#####################

   #	write.table(model.df, FILEmodel.csv , sep = ";", na="" , dec = ", ", qmethod = "double", row.names = TRUE)

## fitted values to DB
#   #ch = odbcConnect("PostgreSQL35W", uid="akasprzyk", pwd="akasprzyk") ## IKTORN
#	ch = odbcConnect("PostgreSQL35W-T", uid="arkadiusz.kasprzyk", pwd="akasprzyk123") ## TOUDI
#   # sqlQuery(ch, paste0("DROP TABLE IF EXISTS ", TABLEmodel))
#      TABLEmodel
#	sqlSave( channel = ch, tablename = TABLEmodel , dat =  model.df , rownames = "id_produkt_k" , verbose = TRUE )                  ##rownames = names(wynik1[[1]])
#	odbcClose(ch)


########################################################################################################################•°


        
        
        
        
