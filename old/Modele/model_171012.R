########################################################################################################################—•°
*## DZIA£ ANALIZ -- ANALIZY -- MODELE STATYSTYCZNE -- A.K. -- 150202
########################################################################################################################—•°
  rm(list=ls()) ; ID = list()      ## clearing the workspace
########################################################################################################################—•°
#! MODEL ###############################################################################################################—•°
########################################################################################################################—•°
## AIM: model on R1
##
## DESCRIPTION:
##    REFERENCE
##       X =
##       Y =
##    ADDITIONALY
##       • excluded from reference:
##
## MODEL:
##	   • gam binomial
##	   • gam poisson
##	   • gam binary
##
########################################################################################################################—•°
ID$. = "171012"   ## takie samo jak nazwa katalogu --  w przyblizeniu data powstania;
#####DATEprediction = as.POSIXct("2016-11-07") ## data prognozy, zazwyczaj taka jak wy¿ej
ENTITY  = ""
##
METHOD = "gam"
FAMILY = c("binary","poisson","binomial")
prediction.name = ""          ## np: "score" w przypadku scoringu ; "prognoza"  dla innych metod
##
ID$ref = "170828"			## katalog z danymi referencyjnymi na których by³ zbudowany model
ID$ref_x = "1"
ID$ref_y = ""      		## przyrostek nazwy tabeli ze zmienn¹ objaœnian¹ ## TYLKO JEDEN W CA£YM PLIKU !!!  inny Y = inny model = inny plik i katalog modelu
##
##
### Data otwarcia:
## [1] "2017-10-13 10:09:05 CEST"
##
## Zmienne nazw tabel:
##                TABLE
## TABLE$ref =    "modele.referencja_170828"
## TABLE$ref_y =  "modele.referencja_"
## TABLE$mod =    "modele.model_171012"
##
## Zmienne nazw plików:
##                         FILE$ref
## FILE$ref$environment =  "referencja_170828_1_environment.Rdata"
## FILE$ref$Rdata =        "referencja_170828_1.Rdata"
## FILE$ref$sql =          "referencja_170828_1.sql"
## FILE$ref$xls =          "referencja_170828_1.xls"
## FILE$ref$csv =          "referencja_170828_1.csv"
##
##                         FILE$mod
## FILE$mod$environment =  "model_171012_environment.Rdata"
## FILE$mod$Rdata =        "model_171012.Rdata"
## FILE$mod$sql =          "model_171012.sql"
## FILE$mod$xls =          "model_171012.xls"
## FILE$mod$csv =          "model_171012.csv"
##
## Zmienne nazw katalogów:
##                PATH
## PATH$R =       "//10.1.1.5/PK_Analitycy/R"
## PATH$packs =   "//10.1.1.5/PK_Analitycy/R/PacksAK"
## PATH$modfun =  "//10.1.1.5/PK_Analitycy/Modele/Funkcje"
## PATH$ref =     "//10.1.1.5/PK_Analitycy/Modele/Dane Referencyjne/170828"
## PATH$mod =     "//10.1.1.5/PK_Analitycy/Modele/Modele/171012"
## PATH$wd =      "//10.1.1.5/PK_Analitycy/Modele/Modele/171012"
##
########################################################################################################################—•°

########################################################################################################################—•°
## 0. Do poprawnego dzialania TinnR (powinno ladowac siê samo - trzeba naprawic jakies .ini)
##  .trPaths <- paste('C:/Users/kasprar/AppData/Local/Temp/Tinn-R/', c('', 'search.txt', 'objects.txt', 'file.r', 'selection.r', 'block.r', 'lines.r'), sep='')
##  ## more automatic (but less reliable)
##  .trPaths <- paste(paste(Sys.getenv('APPDATA'), '\\Tinn-R\\tmp\\', sep=''), c('', 'search.txt', 'objects.txt', 'file.r', 'selection.r', 'block.r', 'lines.r'), sep='')
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
source( paste0( DOMAIN, "/Funkcje/initiate_model.r" ) )



## b. PLIKI ############################################################################################################—•°
initiate_model()  ## tworzy zmienne nazw sciezek, plików i tabel
setwd(PATH$wd)

## c. ŒRODOWISKO #######################################################################################################—•°
## Na wypadek gdyby trzeba by³o przerwaæ / wznowiæ pracê prze ukoñczeniem pliku.

     #!###########################################################################
     ## Gdyby trzeba by³o przerwaæ pracê wróæ do tego miejsca i zapisz œrodowisko:
     # save.model()  ## save.image(file=FILE$pred.environment , compress="xz")	## NIE NADPISZ BEZMYŒLNIE!!!
     ## lub za³aduj œrodowisko gdy wznawiasz pracê
       load(file=FILE$mod$environment) ; detach("package:TinnRcom", unload=TRUE) ; library(TinnRcom) ## unloadNamespace("TinnRcom")  may work too
     #!###########################################################################

.GlobalEnv

########################################################################################################################—•°
#! 2. UWAGI WSTÊPNE ####################################################################################################—•°
########################################################################################################################—•°
##  A. PODSTAWOWYM ZADANIEM niniejszego pliku jest obliczenie prognozy na podstawie nowych danych i modelu:
##    1. pobranie danych z bazy do data.new
##    2. wczytanie modelu
##    3. uzgadnianie zmiennych tzn. obróbka data.new wg wytycznych z modelu:
##       transformacje i ograniczenie zakresu zmiennych tak jak danych referencyjnych
##    4. zapis przetworzonych danych do Data.new
##    5. prognoza, przegl¹d wyników, zapis do plików.
##
########################################################################################################################—•°
##  B.
##    1. Niektóre komendy s¹ zakomentowane jako potencjalnie niebezpieczne.
##       Wywo³ujemy je stawiaj¹c kursor po znaku komentarza i klikajac "play" na pasku.
##       Czynimy to po upewnieniu siê, ¿e uruchomienie danej komendy jest dok³adnie tym czego w³aœnie chcemy.
##    2. Komendy niebezpieczne to te, ktore uruchomione nieprawid³owo bêd¹ trudne do korekty
##       i konieczne bêdzie przeprowadzenie od nowa niemal ca³ej analizy.
##    3. SKRÓTY:
##       • E0 — etap 0 tj. ³adowanie danych z bazy i ich wstêpna obróbka (eliminacja b³êdów; bez metod statystycznych)
##       • E1 — etap 1 tj. w³asciwa obróbka danych:
##              - eliminacja obserwacji odstajacych i przekszta³cenia zmiennych;
##              - uzupelnianie NULLi z u¿yciem metod statystycznych, np. RF;
##       • E2 — etap 2 tj. budowa modelu
##       • E3 — etap 3 tj. prognoza w oparciu o model z E2.
##    4. Podstawowe ramki danych:
##       • E0 — data.raw0 ——> data.raw ——> Data.raw
##       • E1 — Data.raw ——> data.ref ——> Data.ref
##       • E2 — Data.ref, Data.ref_y (zmienne Y, mog¹ ju byc w Data.ref)
##       • E3 — Data.ref;    data.new0 ——> data.new ——> Data.new ——> Prediction.df
##
########################################################################################################################—•°

########################################################################################################################—•°
## 3. MODEL I PROGNOZA #################################################################################################—•°

#####################################################
## a. jeœli nowa prognoza ³adujemy model



########################################################################################################################—•°
## 4. PAKIETY I SKRYPTY ################################################################################################—•°

###############################################################################—•°
## a. PAKIETY #################################################################—•°
 #  .libPaths("D:/Program Files/R/R-3.1.3/library")
   .libPaths("C:/R/R-3.4.1/library")

packages = c( #"stats","graphics","grDevices","datasets","methods","base",  ## these packages are always loaded
               "RPostgreSQL","RODBC"  #"vioplot", #"tree",
              ,"randomForest"
              ,"ROCR"
              ,"gam"
              ,"gdata"   ## Various R Programming Tools for Data Manipulation. !!! ... combining objects, factor manipulation, manipulating MS-Excel formatted files ...
              ,"tseries"
              ,"rattle"  ## Graphical User Interface for Data Mining in R. The R Analytic Tool To Learn Easily (Rattle) provides a Gnome (RGtk2) based interface to R functionality for data mining.
              ,"igraph"  ## Network Analysis and Visualization. Routines for simple graphs and network analysis. It can handle large graphs very well and provides functions for generating random and regular graphs, graph visualization, centrality methods and much more.
              #,"caret"  ## Classification and Regression Training; Misc functions for training and plotting classification and regression models.
              #,"car"    ## Functions and Datasets to Accompany J. Fox and S. Weisberg, An R Companion to Applied Regression, Second Edition, Sage, 2011.
            )

  ## Instalacja pakietów jesli jeszcze nie zainstalowane
  ## Mozna puscic dla pewnosci
#  chooseCRANmirror()
  #for(x in packages)install.packages(x)

  #! Ladowanie pakietow - konieczne zawsze!
l = quote(library(a))
for(p in packages){ l[[2]] = as.name(p);  eval(l) }    #!!! YEAH!!!
## for(p in packages){library(p,character.only = FALSE)}  ## simpler but less sexy :)

###############################################################################—•°
## b. PACKs ###################################################################—•°
## proto-packages -- packages under construction, former FunctionsAK organised in bundels of common themes,
##          fully-fledged packages in the near future;
source(paste(PATH$packs,FILE$packs,sep="/"))
#source("//10.1.1.5/PK_Analitycy/R/PacksAK/PacksAK.R")

   ## now you may re-load any of the packs , e.g.
   pack = "Condapply"
   loadPacksAK(pack)
   ##PATHwd = paste(PATH$packs_dir,pack,sep="/") ; setwd(PATHwd)

########################################################################################################################—•°
## 5. DANE I OBIEKTY ###################################################################################################—•°
## Pomijamy jeœli wczeœniej za³adawaliœmy œrodowisko w linii 90 (oko³o), rozdzia³ 1.c

###############################################################################—•°
## A. OBIEKTY WYMAGANE - bed¹ póŸniej zapisane i przekazane do E2  ############—•°

OBJECTS = c(  ## z E1
              "Data.ref" , "Data.ref.nas" , "data.ref"   #, "data.ref.nas" , "data.ref.outs"
            ##
				, "idx" ,"VAR" #,"VARid" , "VARdebt" , "VARY" , "VARY1"
            ##
            , "variables"  ## wszystkie nazwy w jednej liœcie, której elemanetami s¹ wektory nazw, stare "names._"
            ##
            , "condlists"
            ##
				, "trans"      ## wszystkie obiekty "trans" (transformacje zmiennych) w jednej liœcie;
                           ## informacja o przekszta³ceniach cech : lista przekszta³ceñ i df z t¹ list¹ i innymi statystykami
    #!, "levels.raw" ## oryginalne wartoœci poziomów dla przekszta³conych danych kategorycznych; zob.
    ##  teraz ju¿ zapisane jako attr(trans$toimprove,"levels_old")
                           #     names(trans$toimprove.list) == names(levels.raw)
            #, ls(pattern='^VAR')
            , "OBJECTSref"
            ## E2
            , "regressors" , "formulas" , "models" , "predictions"
            , "summaries" , "summaries_agg"
            #, "model.gam.binary" #, "model.gam.poisson"
            ##
            #, DFtrans )   ## informacja o przekszta³ceniach cech : lista przekszta³ceñ i df z t¹ list¹ i innymi statystykami
           )

#! Mo¿na w trakcie praæ dodaæ inne obiekty do zapamiêtania
#  OBJECTS = union(OBJECTS,"object.xyz")


###############################################################################—•°
## B. DANE REFERENCYJNE  ######################################################—•°

## pierwsze ³adowanie danych referencyjnych z	DOMAIN/Dane Referencyjne/YYMMDD

## POBRANIE danych z .Rdata po E0 i E1
load(file = paste(PATH$ref,FILE$ref$Rdata,sep="/") )

ls(pattern='^(D|d)ata')   ## budujemy model na Data.ref;
   ##  data.ref do wgl¹du -- zawiera zmienna po etapie E0 - nieprzeksztalcone i z obs. odstaj¹cymi
   ## mog¹ slu¿yæ jako Ÿródlo dadatkowych danych w razie naglej potrzeby
   dim(data.ref)   ## 221883    164
   dim(Data.ref)   ## 221026    102
   (nt.ref = nulls0.table(Data.ref))

OBJECTSref  ## obiekty z E1

###############################################################################—•°
## C. PODSTAWOWE OBIEKTY -- przegl¹d  #########################################—•°

###########################################################—•°
## variables
names(variables)
compact(variables,7)

variables$ref1 = variables[setdiff(names(variables),"ref0")]
variables = variables[c("ref0" , "ref1" , "ref", "id" , "dates" #, "notregressors"  , "full"
                        , "regressors" , "y" )] ##, "regressors_poor" )]

variables$ref = names(Data.ref)
   ( nt.regs = nulls0.table(Data.ref[,variables$regressors]) )  ## 0 NA -- OK!!!
   #
   nulls0.table(data.ref[rownames(Data.ref),variables$regressors])    #!#!#!#!#!#!#!#!#!#!!!!!!!!!

variables$ref1$not_regressors
variables$y <- variables$ref1$y
variables$Y <- VAR$Y

###########################################################—•°
## condlists

   names(condlists)
##condlists$ref1 = condlists[setdiff(setdiff(names(condlists),"ref0"),names(condlists$ref0))]
condlists$ref1 = condlists[setdiff(names(condlists),"ref0")]
condlists = condlists[ c( "ref0" , "ref1" ) ]

   # condlists$ref1$rm_na_0 <- NULL
   # condlists$ref1$rm      <- NULL
   # condlists$ref1$fill    <- NULL
   # condlists$ref1$replace <- NULL

   #   names(condlists)
   #      class(condlists$variables_new) <- c("condlist","list")
   #      class(condlists$selection) <- c("condlist","list")
   #   names(condlists$ref0)
   #      class(condlists$ref0$rm_na_0) <- c("condlist","list")
   #      class(condlists$ref0$fill) <- c("condlist","list")
   #      class(condlists$ref0$replace) <- c("condlist","list")
   #      class(condlists$ref0$rm) <- c("condlist","list")
   #   names(condlists$ref1)
   #      class(condlists$ref1$mins) <- c("condlist","list")
   #      class(condlists$ref1$maxs) <- c("condlist","list")

###########################################################—•°
## trans

   names(trans)
   lapply(trans,class)
   attr(trans$toimprove,"levels_old")
      any(names(trans$toimprove.list) !=  names(attr(trans$toimprove,"levels_old")))   #! MUST BE FALSE

###########################################################—•°
## idx
idx=list()
idx$. <- rownames(Data.ref)

   compact(idx,9)

###########################################################—•°
## VAR
## jakie zmienne s¹ potrzebne okazuje siê czêsto dopiero w trakcie budowy modelu;
## tutaj tylko lista najistotniejszych zmiennych, którym nadajemy VAR$znaczenie;
## ICH TWORZENIE w paragrafie  6. ZMIENNE POCHODNE;
##

   grep('^saldo',variables$ref,value=TRUE)
   trans$list[grep('^saldo',names(trans$list),value=TRUE)]

   indentr(VAR,info=0,as.text=1)

#!#!  VAR$Y     ## zdefiniowane w E1  ## check it!
#!#!  VAR$Y1 <- VAR$Y

##   VARfunctions()  ## from  initiate.model()

   METHOD; FAMILY

  Data.ref <- update.df(Data.ref,data.ref,id=0,variable="saldo_y",suffix_start=0,join="left")

dim(Data.ref)
nulls0.table(Data.ref)
# ?merge

###################—•°
## NAZWY zmiennych

#(VAR$Y <- data.frame(name=character(0),description=character(0),family=character(0),plot=logical(0)))
(  VAR$df <-
   rbind( data.frame( meaning = "id"           ## podstawowa zmienna identyfikuj¹ca
                  , name = "id_produkt"
                     , gam = ""
                        , model = FALSE , rf = FALSE , plot = FALSE , stringsAsFactors=FALSE)
        , data.frame( meaning = "debt"         #! to jest  log( saldo_... + 1 )
                  , name = "saldo_y"
                     , gam = ""
                        , model = FALSE , rf = FALSE , plot = TRUE
                         )
        , data.frame( meaning = "debt.0"       #! to jest oryginalne  saldo_... — NIEprzekszta³cone
                  , name = "saldo_y.0"
                     , gam = ""
                        , model = FALSE , rf = FALSE , plot = TRUE
                         )
   #########
   ## NAZWY zmiennych celu Y wg. ich rodzaju/typu -- ALE to nie s¹ NAZWY metod budowy modeli !!!
        , data.frame( meaning = "pays"       ## oryginalne — NIEprzekszta³cone
                  , name = "suma_wplat_pp_y"
                     , gam = "poisson"
                        , model = TRUE , rf = TRUE , plot = TRUE
                        )
      #, data.frame( meaning = "pays.0"    #! to jest oryginalne "pays" z E0 — jaka wplata (bez przeksztalceñ) jeœli [...] (jesli brak wplaty to 0)
      #         , name = ""
      #            , plot = TRUE
      #               , gam = "poisson" )
      #, data.frame( meaning = "pays.1"    #! == "pays.0" * czy_wplata_
      #         , name = ""
      #            , plot = TRUE
      #               , gam = "poisson" )
        , data.frame( meaning = "efficiency"                 ## skutecznosc oryginalna
                  , name = "s_pp_y"
                     , gam = ""
                        , model = FALSE , rf = FALSE , plot = TRUE
                         )
        , data.frame( meaning = "efficiency_1"               ## skutecznosc obciêta do 1
                  , name = "s_pp_y"
                     , gam = "binomial"
                        , model = FALSE , rf = TRUE , plot = TRUE
                         )
        , data.frame( meaning = "efficiency_binomial"        ## skutecznosc do gam(...,family=binomial) jako macierz Nx2 [succeses,failures]
                  , name = "s_pp_y.binomial"
                     , gam = "binomial"
                        , model = TRUE , rf = FALSE , plot = FALSE
                         )
        , data.frame( meaning = "efficiency_groups"          ## skutecznoœæ oryg. podzielona na grupy (0,.001,.01,.1,1,10)
                  , name = "s_pp_y.grupy"
                     , gam = ""
                        , model = FALSE , rf = FALSE , plot = TRUE
                         )
        , data.frame( meaning = "goodbad"                    ## podzia³ spraw na dobre i z³e
                  , name = "czy_wplata_30d_pp"
                     , gam = "binary"
                        , model = TRUE , rf = TRUE , plot = TRUE
                         )
      #, data.frame( meaning = "..."                    ## podzia³ spraw na dobre i z³e
      #         , name = ""
      #            , plot = TRUE
      #               , gam = "" )
   )
)
#names(VAR$Y) <- c("description","name","family","plot")

   indentr(VAR,0)
   VAR$gam()
      VAR$gam("meaning","gam")
   VAR$model()
   VAR$rf()
   VAR$plot()
   VAR$meaning()


   #!#!#! depricated #!#!#!
   ###################—•°
   ## specific names
   cat(paste0("VAR$",VAR$meaning(), " = \"", VAR$name(),"\""),sep="\n")
   ## paste it here and run
   VAR$id = "id_przypis"
   VAR$debt = "saldo_pocz_mc"
   VAR$debt.0 = "saldo_pocz_mc.0"
   VAR$pays = "suma_wplat_po_wizycie_do_30_dni"
   VAR$efficiency = "skutecznosc_po_wizycie_do_30_dni"
   VAR$efficiency_1 = "skutecznosc_po_wizycie_do_30_dni_1"
   VAR$efficiency_binomial = "skutecznosc_po_wizycie_do_30_dni.binomial"
   VAR$efficiency_groups = "skutecznosc_po_wizycie_do_30_dni.grupy"
   VAR$goodbad = "czy_wplata_po_wizycie_do_30_dni_50_zl"

   ################
   cat(paste0("VAR$",VAR$model("gam"), " = \"", VAR$model("name"),"\""),sep="\n")
   ## paste it here and run
   VAR$poisson = "suma_wplat_po_wizycie_do_30_dni"
   VAR$binomial = "skutecznosc_po_wizycie_do_30_dni.binomial"
   VAR$binary = "czy_wplata_po_wizycie_do_30_dni_50_zl"

   ###################—•°
   ## Y skrótowo -- lepiej nie u¿ywaæ
   VAR$Y1                 = VAR$goodbad
      #class(Data.ref[,VARY]) ; table(Data.ref[,VARY]) ;   table(as.numeric(Data.ref[,VARY]))
      #!#   Data.ref[,VARY] = as.numeric(Data.ref[,VARY]) - 1   ## czynimy to poni¿ej w par. 6 -- tutaj dla zwrócenia uwagi
   VAR$Y2                 = VAR$pays
   VAR$Y3                 = VAR$efficiency_binomial       ## do utworzenia
   VAR$Y4                 = VAR$efficiency_groups
   #!#!#!#!#!#!#!#!#!#!#!#!

###################—•°

   indentr(VAR)

OBJECTS = sort(union(OBJECTS,"VAR"))  ## VAR is a list now with elements being single names of key variables (not vectors of names of variables)
                                #,ls(pattern='^VAR')))
     ## setdiff(OBJECTS,ls(pattern='^VAR'))

###############################################################################—•°
## D. POPRAWKI do danych z BD  ## oby jak najmniej!  ##########################—•°

###########################################################—•°
## DANE REFERENCYJNE Y ## zazwyczaj wp³aty w zadanym okresie

   ## TUTAJ NIEPOTRZEBNE - y jest w Data.ref

   ## pierwsze ³adowanie danych referencyjnych z	DOMAIN/Dane Referencyjne/YYMMDD
   ch <- odbcConnect("PostgreSQL35W3", uid = "arkadiusz.kasprzyk", pwd = "akasprzyk123") ## TOUDI

      ( query = paste("SELECT * FROM ", TABLE$ref_y , sep = " ") )
       data.ref_y <- sqlQuery(ch, query,  errors = TRUE)
        dim(data.ref_y)    ##

   #  dpd_k <- sqlQuery(ch, paste0("SELECT id_produkt , dpd_k from akasprzyk.",TABELA))
   #  dpd_sad <- sqlQuery(ch, paste0("SELECT id_produkt , dpd_sad from akasprzyk.",TABELA))
   #  dpd_kom <- sqlQuery(ch, paste0("SELECT id_produkt , dpd_kom from akasprzyk.",TABELA))
   #
   odbcClose(ch)

###########################################################—•°
## INNE zmienne z BD

	#  ch <- odbcConnect("PostgreSQL35W2", uid = "akasprzyk", pwd = "akasprzyk") ## IKTORN
	#  ch <- odbcConnect("PostgreSQL35W3", uid = "arkadiusz.kasprzyk", pwd = "akasprzyk123") ## TOUDI
	#  ...


###############################################################################—•°
## E. £¥CZENIE DANYCH : [ X , Y ] i nie tylko #################################—•°
##
## Zanim po³¹czymy X z Y sprawdzamy czy wszystko w porzadku z Y (X jest ju¿ opracowany na etapie "Dane Referencyjne").
   #!
   ## TUTAJ NIEPOTRZEBNE - y jest w Data.ref

   	nulls.table(data.ref_y)
   	# nulls.table(data.ref_y)

   	## NIE USUWAMY NULLi z   data.ref_y   bowiem oznaczaj¹ one ¿e sprawa nie byla tak dlugo w obsludze po s¹dzie
   	##	data.ref_Y[ is.na(data.ref_Y[,"suma_wplat_mo"]), "suma_wplat_mo" ] = 0

   	# table(data.ref_Y[,"suma_wplat_mo"] == 0,useNA = "ifany")

   	#########################################—•°
   	## zmienna nazwowa ID -- pozosta³e zmienne nazwowe w p.7.

      VAR$id

   	#########################################—•°

      rownames(data.ref_y) = data.ref_y[,VARid]
   	names.ref_y = names(data.ref_y)

      Data.ref = update.df( Data.ref , data.ref_y , variables = names.ref_y , action = "append" )

###################################################################################################—•°
   save.model()           ## NIE NADPISZ BEZMYŒLNIE !!!
###################################################################################################—•°


########################################################################################################################—•°
## 6. ZMIENNE POCHODNE #################################################################################################—•°
##    Powinny byæ przewidziane w E1 a nawet wczeœniej (SQL), jednak nie wszystko da siê przewidziec...
##

#! WA¯NE!!!
#!    condlists$variables_new = structure( list() , class = c("condlist","list") )
#!    variables$newmod = character(0)


   ###################################################################################################—•°
   ## SZABLON, np. ###################################################################################—•°
   with(Data.ref, plot.variable(v1/v2) )
   with(Data.ref, { windows();coldef.black(); plot(v2,v1)} )

condlists$variables_new[["vx"]] = list(  ## zob. pomoc w condapply()  (condapply.r)
                                 conditions = quote("all")   ## domyœlna wartoœæ — mozna pomin¹æ
                               , inputs = c("v1","v2","..")
                               , substitute = quote( f(a,b,..) )
                               , plot = TRUE
                               )

   vx = condapply( Data.ref , condlists$variables_new["vx"] , return.all = FALSE )

      with(datfram, plot.variable(vx) )
      with(datfram, plot.variable(vx[vx < 10]) )

   #!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
   ## jeœli OK (po skonstruowaniu WSZYSTKICH dodatkowych zmiennych)
   Data.ref = condapply( Data.ref , condlists$variables_new ) ; rm(vx)
   #!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
   ###################################################################################################—•°


###################################################################################################—•°
## 6.1 VAR$debt ###################################################################################—•°

VAR$debt         #! to jest logx( VAR$debt ,.1)
VAR$debt.0       ## to bêdzie nazwa na oryginalne saldo z data.ref

      plot.variable(Data.ref[,VAR$debt])
      plot.variable(data.ref[,VAR$debt])

#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#—•°
Data.ref[,VAR$debt.0] <- data.ref[idx$.,VAR$debt]   #!# idx$. zdefiniowane powy¿ej
#!# lub
Data.ref = update.df(Data.ref,data.ref,id=0,variable=VAR$debt,action="append",suffix_start=0)
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#—•°
variables$ref <- union(variables$ref,VAR$debt.0)
   names(Data.ref)

dataref <- Data.ref[,c(VAR$id,VAR$debt,VAR$debt.0,VAR$pays,VAR$goodbad)]  # kopia robocza do podgl¹du

###################################################################################################—•°
## 6.2 VAR$efficiency ###########################################################################—•°

VAR$efficiency   ## skutecznoœæ

   plot.variable(Data.ref[,VAR$pays]) #,FUN=function(x){log(x+1)})
   plot.variable(Data.ref[,VAR$debt])
   plot.variable(Data.ref[,VAR$debt.0])
      sum(Data.ref[,VAR$pays]>0)


   condapply( dataref, return.all=NULL
            , condlist = list( "efficiency" = list( inputs = c(VAR$pays,VAR$debt.0) , substitute = quote(a/b)) )
            , plot=c(F,T)
            #, what=c("hist","ecdf","cloud","density")
            )

condlists$variables_new[[VAR$efficiency]] = list(
                     inputs = c(VAR$pays,VAR$debt.0)
                   , substitute = quote(a/b)
                   , isolate = FALSE
                   , plot = TRUE
      )

dataref <- condapply( dataref , condlists$variables_new[VAR$efficiency] , breaks = c(-.01,.01,.1,.2,.5,1,3))

   head(dataref)

###################################################################################################—•°
## 6.2 VAR$efficiency ###########################################################################—•°

VAR$efficiency_1   ## skutecznoœæ przyciêta do 1

condlists$variables_new[[VAR$efficiency_1]] = list(
                    inputs = c(VAR$efficiency)
                   , substitute = quote(pmin(a,1))
                   , isolate = FALSE
                   , plot = TRUE
      )

dataref <- condapply( dataref , condlists$variables_new[VAR$efficiency_1] , breaks = c(-.01,.01,.1,.2,.5,1) )

###################################################################################################—•°
## 6.3 VAR$efficiency_binomial #####################################################################—•°

VAR$efficiency_binomial

condlists$variables_new[[VAR$efficiency_binomial]] = list(
                     inputs = c(VAR$pays,VAR$debt.0)
                   #, substitute = body(function(){ max.debt_pay <- pmax(a,b) ; fails <- max.debt_pay - b ; round(cbind(b,fails)) })
                   , substitute = quote({ max.ab <- pmax(a,b) ; fails <- max.ab - a ; round(cbind(a,fails)) })
                   , isolate = FALSE
                   #, plot = TRUE
      )

dataref = condapply( dataref , condlists$variables_new[VAR$efficiency_binomial] )
   head(dataref,20)


   with(dataref, {   vx <- skutecznosc_po_wizycie_do_30_dni.binomial ; skutecznosc <- vx[,1] / rowSums(vx)
                     plot.variable(skutecznosc,breaks = c(-.01,.01,.1,.2,.5,1))
                     plot.variable(skutecznosc[skutecznosc<.2], breaks = c(-.01,.01,.1,.2) )
                  } )

###################################################################################################—•°
## 6.4 skutecznosc_grupy ##########################################################################—•°


VAR$efficiency_groups
sum(dataref[,VAR$efficiency]==0)  ## 14972

condlists$variables_new[[VAR$efficiency_groups]] = list(
                           inputs = c(VAR$efficiency)
                        ,  substitute = quote(
                                    cut( a , breaks = monetary(  max(a) , power.range = 3 , base = 1 )#, merge.last = 1 )
                                        , include.lowest = TRUE )
                                    )
                        , isolate = FALSE
                        , plot = TRUE
                        )

dataref = condapply( dataref , condlists$variables_new[VAR$efficiency_groups] ) #, as.factor=TRUE)
        table(dataref[,VAR$efficiency_groups],useNA="ifany")
vx = dataref[,VAR$efficiency_groups]

 ( table.number.VAReffgroups = as.table(tapply(Data.ref[,VAR$debt.0],vx,length)) )
 ( table.debt.VAReffgroups = as.table(tapply(Data.ref[,VAR$debt.0],vx,sum)) )
 ( table.pay.VAReffgroups = as.table(tapply(Data.ref[,VAR$pays],vx,sum)) )

 windows(height=4,width=12); par.my(); par(mfrow=c(1,2))
 barplot(table.debt.VAReffgroups);title("debt");
 barplot(table.pay.VAReffgroups);title("payments")

## inaczej
agg.glm = aggregate( as.formula(paste0(VAR$debt.0,"~",VAR$efficiency_groups))
				,FUN = function(x){ y = c( sum(x), length(x), max(x), mean(x) , min(x) ) ; names(y) = c("sum","number","max","mean","min") ; y }
 				,data = update.df(dataref,Data.ref,id=0,variable=VAR$debt.0)
				#,simplify = FALSE
				)
format( agg.glm  , scientific = FALSE	 , digits = 6 	 #, nsmall = 1
		)

###################################################################################################—•°
## 6.5 czy_dobra ##################################################################################—•°

VAR$goodbad   ## ju¿ istnieje

#condlists$variables_new[[VARgoodbad]] = list(
#                          inputs = c(VAR$efficiency_1)
#                        , substitute = quote( a == 1 )
#                        , isolate = FALSE
#                        , plot = TRUE
#                        )
#
#  dataref <- condapply( dataref , condlists$variables_new[VAR$goodbad] , return.all = FALSE )
#      table(dataref[VAR$goodbad])

###################################################################################################—•°
## NA KONIEC (po skonstruowaniu wszystkich zmiennych) #############################################—•°

#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
## jeœli OK to
variables$newmod = unique(setdiff(VAR$name(),variables$ref))  ##  0 ok!
                           unlist(VAR)
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#

Data.ref = condapply( Data.ref , condlists$variables_new )
##                        id    sum
##                      ...
## czy_dobra               5  64001

names(Data.ref)

   rm(dataref,vx)

(nt.ref <- nulls0.table(Data.ref))

###################################################################################################—•°
   save.model()           ## NIE NADPISZ BEZMYŒLNIE !!!
###################################################################################################—•°


########################################################################################################################—•°
## 7. GRUPY ZMIENNYCH ##################################################################################################—•°
## dziedziczymy z E1 ale musimy zaktualizowaæ (wykluczyæ zmienne odrzucone w E1)  ###########################—•°

(variables$ref = names(Data.ref))
variables$newmod

   ## zarzucone, ale warto wróciæ i przemyœleæ
   #! update.names.ref2mod()  ## zdefiniowane w    DOMAIN/Funkcje/funkcje_modele.r
   ##
   ##   ref.names.vec           ## grupy nazw zmiennych z E1 (nazwy wektorów nazw)
   ##   ref.names.list          ## grupy (wektory) nazw z E1 zapisane do listy
   ##   ref_not_mod.names.list  ## zmienne (w odpowiednich grupach), które byly w E1 ale zostaly usuniête (bo byly slabe)
   ##   mod.names.list          ## zmienne (w odpowiednich grupach), które przeszly do E2

###############################################################################—•°
## A. identyfikatory

colnames.class(Data.ref,class="factor")

   cat(intersect(variables$ref1$id ,variables$ref), sep = "\"\n,\"")

variables$id = c(  "id_produkt"
,"id_przypis"
,"czy_bzwbk"
,"id_bank"
,"id_paczka"
,"data_start"
,"data_stop"
,"grupa_portfela"
,"nazwa_biznesowa"
,"podmiot_kupujacy"
,"pesel"
,"wiek_dluznika_band"
)

  setdiff(variables$id,variables$ref)   ## 0

###############################################################################—•°
## B. inne

variables$regressors  ## ustalone w E1 - w pliku z etapu obróbki danych  dane_referencyjne_YYMMDD_1.r
                  ## tak jak i poni¿sze grupy zmiennych:
   intersect(variables$regressors,variables$id)
   setdiff(variables$ref,union(variables$regressors,variables$id))

variables$regressors_poor

##
( variables$dates = grep('^data' ,variables$ref,value=TRUE) )    ## tu nie ma juz dat
( variables$saldo = grep('^saldo',variables$ref,value=TRUE) )
   variables$ref1$full
   variables$ref1$tofill

## pomocnicze grupy zmiennych
(variables$dt =    grep('^dt',variables$ref,value=TRUE))
(variables$dp = grep('^dp',variables$ref,value=TRUE))
(variables$czy = grep('^czy',variables$ref,value=TRUE))
(variables$kat = grep('^kat',variables$ref,value=TRUE))
(variables$kwota = grep('^kwota',variables$ref,value=TRUE))

(variables$ile = grep('^ile',variables$ref,value=TRUE))
(variables$factor = colnames.class(Data.ref,"factor"))
   cbind(sapply(Data.ref[,intersect(variables$ile,variables$factor),drop=FALSE],function(x){length(unique(x))}))
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

(variables$wplaty = grep('^wplaty',variables$ref,value=TRUE))
(variables$skutecznosc = grep('^skutecznosc',variables$ref,value=TRUE))

(variables$suma = grep('^suma',variables$ref,value=TRUE))

#cbind(unlist(mget(ls(pattern='^VAR'))))
   VAR
   compact(variables,7)
   indentr(variables, compact=7, ind="\t")

###################################################################################################—•°
   save.model()           ## NIE NADPISZ BEZMYŒLNIE !!!
###################################################################################################—•°


   ########################################################################################################################—•°
   ## 8. SELEKCJA REKORDÓW ################################################################################################—•°
   ## (tutaj pominiêto)
   ## Niniejszy model bêdzie oparty na podzbiorze ca³oœci danych — tylko dane z  dpd_start_k

      nt.ref[nt.ref$name %in% variables$regressors,]

      ## POMIJAMY

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
             nulls0.table(data.ref[rownames(Data.ref),variables$regressors])  ## ALE NA s¹ ju¿ uzupe³nione RF w E1

########################################################################################################################—•°
## 9. SZYBKI PRZEGL¥D ##################################################################################################—•°

	attach(Data.ref)     ##
   ## po X zmiennych

#   variables$tosee = setdiff( variables$ref , union(variables$id,variables$dates) )
   variables$tosee = union( variables$regressors , c(VAR$Y1,variables$newmod) )

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

###################################################################################################—•°
   save.model()           ## NIE NADPISZ BEZMYŒLNIE !!!
###################################################################################################—•°


########################################################################################################################—•°
########################################################################################################################—•°
## II. PRESELEKCJA ZMIENNYCH
########################################################################################################################—•°

########################################################################################################################—•°
## 10. Random Forest 0 -- wstêpna ocena istotnosci zmiennych  ###########################################################—•°

VAR$rf()
VAR$rf("gam","name")

RF <- list("0"=list(),"1"=list())
RF$exec_time = list()
for(mod in VAR$rf("gam","name")){
   RF[["0"]][[mod]] <- NA
   RF[["1"]][[mod]] <- NA
   RF$exec_time[["0"]][[mod]] <- list()
}

varImp = list("0"=NA,"1"=NA)
varImp[["0"]] = data.frame(rownames=1:length(variables$regressors))[-1]

idx$RF = sample(1:nrow(Data.ref),10000)

# for(type in Ynames_RF){  #1
k = 2
   (yname = VAR$rf()[k])   ## pêtla rêczna -- ka¿da iteracja trwa kilkanaœcie minut (a mo¿e i d³u¿ej)
   (mod = VAR$rf("gam","name")[k])
      ptm <- proc.time()
   RF[["0"]][[mod]] <- randomForest( y = Data.ref[idx$RF,yname]
                                   , x = Data.ref[idx$RF,variables$regressors]
                                   , importance=TRUE
                                   )
         (RF$exec_time[["0"]][[mod]] <- proc.time()-ptm)
   windows(width=16,height=7);varImpPlot(RF[["0"]][[mod]], main = paste0(mod," [0]"))
   varImp[["0"]][mod] <- I(apply(RF[["0"]][[type]]$importance,2,function(x){order(x,decreasing=TRUE)}))
#} ## END of #1

lapply(varImp[["0"]],function(x){x[,1]})

###################################################################################################—•°
   save.model()           ## NIE NADPISZ BEZMYŒLNIE !!!
###################################################################################################—•°


########################################################################################################################—•°
## 11. Eliminacja zmiennych skorelowanych  #############################################################################—•°

###################################################################################################—•°
## A. wektory zmiennych  ##########################################################################—•°
FAMILY
VAR$model()
VAR$model("gam")

regressors = list()
for( nam in  c("final","all",VAR$model("gam")) ){
   regressors[[nam]] <- list()
}

###############################################################################—•°
## regressors$all  uzupelniamy po wygenerowaniu regressors$...
###############################################################################—•°
## regressors$all = regressors$poisson —— Y = VAR$poisson = "suma_wplat_po_wizycie_do_30_dni"

regressors$all[[1]] = c(  "saldo_pocz_mc"
,"saldo_pocz"
,"dpd_start"
,"dpd_przypis"
,"wiek_dluznika"
,"liczba_wizyt_rpc"
,"ktora_wizyta_na_kodzie"
,"liczba_wizyt"
,"liczba_wizyt_przed"
,"liczba_przypisow_przed"
,"czy_placaca_na_zleceniu"
,"grupa_saldo_pocz_mc"
,"czy_wizyta_przed"
,"czy_wplata_50_pimportem"
,"grupa_portfela"
,"czy_przypis_przed"
,"etap_start_przypis"
,"czy_aktywna_ugoda"
)    # ...

###############################################################################—•°
## regressors$final  uzupelniamy na koncu

###################################################################################################—•°
## pêtla rêczna; dopisujemy od góry, przestawiamy w trakcie SELEKCJi zmiennych

# for(k in VAR$model("gam")){  #1
k=3
(mod<-VAR$model("gam")[k])
cat.regressors(mod)          ## defined in  initiate_model()

plot.variable(Data.ref[,VAR$plot()[mod]],main=VAR$plot()[mod])
#} ## END of #1

###############################################################################—•°
## rk=1; regressors$poisson —— Y = VAR$poisson = "suma_wplat_po_wizycie_do_30_dni"

regressors$poisson[[1]] = c(  "saldo_pocz_mc"
,"saldo_pocz"
,"dpd_start"
,"dpd_przypis"
,"wiek_dluznika"
,"liczba_wizyt_rpc"
,"ktora_wizyta_na_kodzie"
#,"liczba_wizyt"
#,"liczba_wizyt_przed"
#,"liczba_przypisow_przed"
,"czy_placaca_na_zleceniu"
#,"grupa_saldo_pocz_mc"
#,"czy_wizyta_przed"
,"czy_wplata_50_pimportem"
#,"grupa_portfela"
#,"czy_przypis_przed"
#,"etap_start_przypis"
#,"czy_aktywna_ugoda"
)

###############################################################################—•°
## k=2; regressors$binomial —— Y = VAR$binomial = "skutecznosc_po_wizycie_do_30_dni.binomial"

regressors$binomial[[1]] = c(
   "saldo_pocz_mc"
,"saldo_pocz"
,"dpd_przypis"
,"dpd_start"
,"wiek_dluznika"
,"liczba_wizyt_rpc"
,"liczba_wizyt"
,"ktora_wizyta_na_kodzie"
,"czy_placaca_na_zleceniu"
#,"grupa_portfela"
,"czy_wplata_50_pimportem"
,"liczba_wizyt_przed"
#,"liczba_przypisow_przed"
#,"grupa_saldo_pocz_mc"
#,"czy_wizyta_przed"
#,"czy_przypis_przed"
#,"etap_start_przypis"
#,"czy_aktywna_ugoda"
)

###############################################################################—•°
## k=3; regressors$binary —— Y = VAR$binary = "czy_wplata_po_wizycie_do_30_dni_50_zl"

regressors$binary[[1]] = c(
   "liczba_wizyt_rpc"
,"saldo_pocz_mc"
,"saldo_pocz"
,"dpd_przypis"
,"dpd_start"
,"wiek_dluznika"
,"ktora_wizyta_na_kodzie"
,"liczba_wizyt"
,"czy_placaca_na_zleceniu"
#,"liczba_przypisow_przed"
#,"liczba_wizyt_przed"
#,"czy_aktywna_ugoda"
#,"grupa_portfela"
#,"czy_przypis_przed"
#,"czy_wplata_50_pimportem"
#,"grupa_saldo_pocz_mc"
#,"etap_start_przypis"
#,"czy_wizyta_przed"
)

###############################################################################—•°
indentr(regressors)

###################################################################################################—•°
## B. SELEKCJA ####################################################################################—•°
## Usuwamy zmienne skorelowane (jedn¹ z).
## Usuwamy te zmienne, które s¹ ni¿ej co do wa¿noœci wg wyników z RF - nale¿y otworzyæ wykresy utworzone na etapie RF.

##  Pêtla rêczna po  VAR$rf("gam")

# for(k in VAR$rf("gam")){  #1
k = 3
(mod = VAR$rf("gam","name")[k])

###############################################################################—•°
## 1°. Korelacje (grafy) - zmienne ci¹g³e #####################################—•°

setdiff(regressors[[mod]][[1]],names(Data.ref)) ## powinno byæ  0 ; jeœli nie to graf() nie zadziala
                                                ## poza tym wskazuje to na blêdn¹ nazwê w  regressors[[mod]][[k]]

rho =.7 #  =.5  =.3   # wa¿ne jest tylko r=.7
 #windows()
 #! ponizsza funkcja znajduje korelacje jedynie dla zmiennych liczbowych !!!   I dobrze!
 (graf0 <- graf(zmienne.graf = regressors[[mod]][[1]], dane = Data.ref,
      r=rho,
      zmienne.bez.null = regressors[[mod]][[1]],
      target = VAR$rf()[k], ## "skutecznosc_po_wizycie_do_30_dni_1", ##trzeba czasem wpisaæ rêcznie
      ver = 2,               #! bledy dla  ver=1 ## wysypuje sie gdy nie ma zadnych korelacji (powyzej zadanego r)
      metoda = list( c(1) , c("pearson", "kendall", "spearman") ) ))

 graf0$`korelacja zmiennych zaleznych`[c(5,6,8,9),c(5,6,8,9)]

#######################################—•°
## sprz¹tanie
   rm(Data.graf,Y,nazwy,wspzm)

###############################################################################—•°
## 2°. chi^2 - zmienne kategoryczne ###########################################—•°
##
     colnames.class( Data.ref[,variables$regressors] , "factor" )
(regs_factors <- colnames.class( Data.ref[,regressors[[mod]][[1]]] , "factor" ))
## Ponizej testujemy hurtowo hipoteze ze zmienne kategoryczne (factors) sa niezalezne.
## Odrzucamy ja dla p.value<0.05.
## Postêpujemy tak jak dla zmiennych numerycznych tj. odrzucamy zmienne a¿ bêdzie brak 'korelacji'
##  na poczatek te ktore koreluja z duza iloscia innych
chisqs = chisq.multi.test(Data.ref[,regressors[[mod]][[1]]])
  ## nie przejmujemy sie ostrzezeniami
chisqs$cor.pairs ## "skorelowane" (a raczej zalezne) pary zmiennych kategorycznych, tj. tylko te pary dla których p<0.05 (tak male P(para zmiennych niezale¿na))
  ## wszystkie korelacje (p.values)
print(chisqs$p.values.table,zero.print=".")

#######################################—•°
## dok³adniej

   with(Data.ref,table(grupa_portfela,wojewodztwo_pol_k))

## wszystkie kategoryczne - ka¿dy z ka¿dym (ale kiedy zostaje ju¿ tylko kilka z nich)
(regs_factors <- colnames.class( Data.ref[,regressors[[mod]][[1]]] , "factor" ))
for(j in 1:(length(regs_factors)-1)){ nam_j = regs_factors[j]
for(k in (j+1):length(regs_factors)){ nam_k = regs_factors[k]
      dd = chisqs$cor.pairs
      idx = (dd$var1 == nam_j & dd$var2 == nam_k) | (dd$var1 == nam_k & dd$var2 == nam_j)
      print( dd[ idx , "p.value"] )
      print({ tt <- table(Data.ref[,nam_j],Data.ref[,nam_k],deparse.level=1) ;
              names(attr(tt,"dimnames")) <- c(nam_j,nam_k)
              tt
            })
      cat("\n")
}}

## wykresik
with( Data.ref ,{ var.name = "liczba_wizyt" ; yy = VAR$plot()[mod]
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

#} ## END of #1


###################################################################################################—•°
## C. regressors$final
(regressors$final[[1]] = Reduce(union,lapply(VAR$model("gam"),function(x)regressors[[x]][[1]])))

indentr(regressors)

###################################################################################################—•°
   save.model()           ## NIE NADPISZ BEZMYŒLNIE !!!
###################################################################################################—•°


########################################################################################################################—•°
## 12. Random Forest 1 -- ponowna ocena istotnosci zmiennych  ##########################################################—•°
##                        tylko wybrane zmienne               ##########################################################—•°

varImp[["1"]] = data.frame(rownames=1:length(regressors$final))[-1]

idx$RF = sample(1:nrow(Data.ref),10000)
   compact(idx,10)

# for(type in Ynames_RF){  #1
k = 3
   (yname = VAR$rf()[k])   ## pêtla rêczna -- ka¿da iteracja trwa kilkanaœcie minut (a mo¿e i d³u¿ej)
   (mod = VAR$rf("gam","name")[k])
      ptm <- proc.time()
   RF[["1"]][[mod]] <- randomForest( y = Data.ref[idx$RF,yname]
                                   , x = Data.ref[idx$RF,regressors$final]
                                   , importance=TRUE
                                   )
         (RF$exec_time[["1"]][[mod]] <- proc.time()-ptm)
   windows(width=16,height=7);varImpPlot(RF[["1"]][[mod]], main = paste0(mod," [1]"))
   varImp[["1"]][mod] <- I(apply(RF[["1"]][[type]]$importance,2,function(x){order(x,decreasing=TRUE)}))
#} ## END of #1

lapply(varImp[["1"]],function(x){x[,1]})


format(object.size(RF),units="Mb")    ## 194 MB

###################################################################################################—•°
   save.model()           ## NIE NADPISZ BEZMYŒLNIE !!!
###################################################################################################—•°



########################################################################################################################—•°
########################################################################################################################—•°
## III. BUDOWA MODELU
########################################################################################################################—•°

########################################################################################################################—•°
## 13. Intro - podstawowe obiekty  #####################################################################################—•°

###################################################################################################—•°
## A. LISTY zmiennych, modeli i podsumowañ ########################################################—•°

FAMILY = c("poisson","binomial","binary")
   #
formulas = structure(list(),class="formulas")
formulas$final = list()
   #
models = structure(list(),class="models")
predictions = structure(list(),class="predictions")
   #
###################—•°
modelings     = structure(list(),class="modelings")
modelings_agg = structure(list(),class="modelings_agg")
   #
modeling_k_0 = structure(list(),class="modeling_k_0")
modeling_k_0$panova  = NA #(panova.gam,panova,data.frame)
modeling_k_0$summary = NA #(summary.panova.gam,summary.panova,summary,data.frame)    # BUT : summaries$binary[[k]]$whole$agg$pars  = update.df( summaries$binary[[k]]$folds$agg$pars , summaries$binary[[k]]$whole$raw , join = "left" , action = "append" )
   #
modeling_k = structure(list(),class="modeling_k")
modeling_k$folds = modeling_k_0
modeling_k$whole = modeling_k_0
   #
###################—•°
for(fam in FAMILY){
   formulas[[fam]] <- list()
   modelings[[fam]] <- structure(list(),class="modeling")
   ## summaries[[fam]][[k]]     <- summary_k
   modelings_agg[[fam]] <- structure(list("folds"=NA,"whole"=NA),class="modelings_agg_mod")
}

   str(formulas)
   str(summaries)
      str(summary_k)
   str(summaries_agg)

   indentr(formulas)
   indentr(summaries)
      indentr(summary_k)
   indentr(summaries_agg)


   ###################—•°
   summaries     = structure(list(),class="summaries")
   summaries_agg = structure(list(),class="summaries_agg")
      #
   summary_k_0 = structure(list(),class="summary_k_0")
   summary_k_0$panova  = NA #(panova.gam,panova,data.frame)
   summary_k_0$summary = NA #(summary.panova.gam,summary.panova,summary,data.frame)    # BUT : summaries$binary[[k]]$whole$agg$pars  = update.df( summaries$binary[[k]]$folds$agg$pars , summaries$binary[[k]]$whole$raw , join = "left" , action = "append" )
      #
   summary_k = structure(list(),class="summary_k")
   summary_k$folds = summary_k_0
   summary_k$whole = summary_k_0
      #
   ###################—•°
   for(fam in FAMILY){
      formulas[[fam]] <- list()
      summaries[[fam]] <- list()
      ## summaries[[fam]][[k]]     <- summary_k
      summaries_agg[[fam]] <- structure(list("folds"=NA,"whole"=NA),class="summaries_agg_mod")
   }

      str(formulas)
      str(summaries)
         str(summary_k)
      str(summaries_agg)

      indentr(formulas)
      indentr(summaries)
         indentr(summary_k)
      indentr(summaries_agg)

###################################################################################################—•°
## B. zmienna pomocnicza do zbalansowanego podzialu Data.ref  #####################################—•°

   nulls0.table(Data.ref[,regressors$binomial[[1]]])
   VAR$mod()["binary"]
VAR$balance = "czy_wplata_po_wizycie_do_30_dni_50_zl"
   table(Data.ref[,VAR$balance])

###################################################################################################—•°
   save.model()           ## NIE NADPISZ BEZMYŒLNIE !!!
###################################################################################################—•°


########################################################################################################################—•°
## 14. MODELE  #########################################################################################################—•°
########################################################################################################################—•°

FAMILY
VAR$mod()

## Hand LOOP
# for(mod in VAR$mod("gam")){  #1
#
#!!!  ... only few steps BUT each step is VERY LONG ...
#  ...
   ###############################################################################—•°
   ## TEMPLATE of each step
   ###############################################################################—•°
   ## linear — selection of non-transformed variables
   cat(regressors[[mod]][[1]],sep="\"\n,\"")
   k = 1 .. r
   regressors[[mod]][[k]] = c( "_paste_"
   )
   ###############################################################################—•°
   ## s()    — selection of variables after transforming them with s()
   cat(regressors[[mod]][[1]],sep=")\"\n,\"s(")
   k = r+1 .. K
   regressors[[mod]][[k]] = c( "s(_paste_)"
   )
   ###############################################################################—•°


########################################################################################################################—•°
## 14.A. model gam(,family=binomial(link=logit))  ######################################################################—•°

###############################################################################—•°
## linear

## dla tych danych nie selekcjonujemy zmiennych nieprzekszta³conych, od razu przechodzimy do s()

cat(regressors$binomial[[1]],sep="\"\n,\"")

k = 1
regressors$binomial[[k]] = c( "saldo_pocz_mc"
,"saldo_pocz"
,"dpd_przypis"
,"dpd_start"
,"wiek_dluznika"
,"liczba_wizyt_rpc"
,"liczba_wizyt"
,"ktora_wizyta_na_kodzie"
,"czy_placaca_na_zleceniu"
,"czy_wplata_50_pimportem"
,"liczba_wizyt_przed"
)

###############################################################################—•°
## s()

nulls.table(Data.ref[,regressors$binomial[[k]]])
cat(regressors$binomial[[k]],sep=")\"\n,\"s(")   ## erase s() for factors!

k=3
regressors$binomial[[k]] = c( "s(saldo_pocz_mc)"
,"s(saldo_pocz)"
,"s(dpd_przypis)"
,"s(dpd_start)"
,"s(wiek_dluznika)"
,"liczba_wizyt_rpc"
,"liczba_wizyt"
#,"s(ktora_wizyta_na_kodzie)"    #2
,"czy_placaca_na_zleceniu"
,"czy_wplata_50_pimportem"
,"liczba_wizyt_przed"
)

      #for(k in 1:7){summaries$binary[[k]] = summary_k}

summaries$binomial[[k]] = summary_k
(formulas$binomial[[k]] = paste0( VAR$mod()["binomial"] , " ~ " , paste(regressors$binomial[[k]] , collapse=" + " )))
rm(panova_k) ## rm(panova.agg.adds)
for(j in 1:30){
   Data.ref_kj <- split.rand.portion(Data.ref,split=TRUE,portion=.8,variable=VAR$balance)
      model_kj <- gam( formula(formulas$binomial[[k]]) , family=binomial(link=logit) , data=Data.ref_kj )
   panova( model_kj , what=c("coef","pval") , sstars=TRUE
                    , append_to="panova_k"
                    , add=c("mse","aic","gini","auc","p.mww")
                    , print=FALSE )
}
summaries$binomial[[k]]$folds$panova <- panova_k


summaries$binomial[[k]]$folds$summary <- summary( panova_k
   ## coeffs
      #, FUN.c = default.c                                  ## defined within function
      #, add_colnames.c = c(TRUE,FALSE,TRUE,TRUE,FALSE)     ## default
   ## measures
      #, FUN.m = default.m                                  ## defined within function
      #, add_colnames.m = TRUE                              ## default
      , append_to = "summary_panova_k" , row = k
      , print=TRUE
)
summaries_agg$binomial$folds <- summary_panova_k

   windows();par.my()
   split.stats( predict(model_kj,type="response") , Data.ref_kj[,VAR$plot()["binomial"]] == 1 , plot = TRUE )

###############################################################################—•°
## Modele na ca³oœci danych w oparciu o wyniki badañ na podzbiorach danych

#for(k in 6:length(formulas$binomial)){ #1   # k=1
   print(k)
model_k <- gam(formula(formulas$binomial[[k]]),family=binomial(link=logit),data=Data.ref)
   #
summaries$binomial[[k]]$whole$panova <- panova(model_k,what=c("coef","pval"),sstars=TRUE,add=c("mse","aic","gini","auc","p.mww"))
   #
summaries$binomial[[k]]$whole$summary <- summary( summaries$binomial[[k]]$whole$panova
   , FUN.c = list( )#"coeffs" = function(x){c("mean"=mean(x))} , "Pr(>F)" = function(x){c("mean"=mean(x))} , "Pr(>F)" = function(x){c("sstars" = signif_stars(mean(x)))} )
   , FUN.m = list( "MSE" = mean , "AIC" = mean , "Gini" = mean , "AUC" = mean , "p.mww" = mean )
   #, add_colnames.c = TRUE  #
   , row = k , append_to = "summary_panova_k_whole"
)
##summaries$binomial[[k]]$whole$summary$measures <- summary_panova_k_whole[k,]
summaries_agg$binomial$whole <- summary_panova_k_whole ;

   ## comparison
   update.df( summaries$binomial[[k]]$folds$summary$coeff
            , summaries$binomial[[k]]$whole$panova , join = "left" , action = "append"
            )

   windows();par.my()
   split.stats( predict(model_k,type="response")
               , Data.ref[,VAR$plot()["binomial"]] == 1      #!#!#!#!!!!!!
               , plot = TRUE )
      #
#}  ## END of #1

summaries_agg$binomial
summaries$binomial[[k]]

compact(summaries,9)
indentr(summaries)

#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
   save.model()           ## NIE NADPISZ BEZMYŒLNIE !!!
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#

###############################################################################—•°
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
#!#  Wybieramy model 9  #!#!#!#!#!#!#!#
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#

regressors$final$binomial = list()
 regressors$final$binomial$s = regressors$binomial[[9]]
 regressors$final$binomial$x = gsub('s\\(','',gsub(')','',regressors$final$binomial$s))
formulas$final$binomial   = formulas$binomial[[9]]
models$binomial = gam(formula(formulas$final$binomial),family=binomial(link=logit),data=Data.ref)

windows();par.my()
split.stats( predict( models$binomial , type="response") , Data.ref[,VARefficiency_1] == 1 , plot = TRUE )

###############################################################################—•°
## odzyski faktyczne a przewidywane z binomial

predictions = list()

   predictions$binomial = predict(models$binomial,type="response")*Data.ref[,VARdebt1]
   	sum(predictions$binomial)
   	sum(Data.ref[,VARpays])

   windows(height = 10, width = 5 ); par.my()
   plot( Data.ref[,VARpays] ~ predictions$binomial , pch='.' , col = "yellow" , ylab = VARpays )
     	title("wp³aty rzeczywiste vs przewidywane binomial")
   	lm.binomial = lm(Data.ref[,VARpays] ~ predictions$binomial)
   	abline(lm.binomial,col="white");
   	text(	  x = max(predictions$binomial)/2 , y = max(Data.ref[,VARpays])*.9
   			, labels = paste0( "y = " , paste( paste0( round( lm.binomial$coef, 3 ) , c(""," * x") ), collapse = " + ") )
   		 )
	#  abline(h=0,col="white")
	#  legend( "topleft" , legend = unique(Data.ref$grupa_portfela) , col = unique(as.numeric(Data.ref$grupa_portfela)) + 1, pch = 20 )

      #!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
      sum(cbind(1,predictions$binomial) %*% cbind(lm.binomial$coef))
      #!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#

      resids <- Data.ref[,VARpays] - predictions$binomial
   windows(height = 12, width = 6 ); par.my() ; par(mfrow=c(2,1))
   plot( resids ~ Data.ref[,VARpays] , pch='.' , col = "yellow" , xlab = VARpays , ylab = "residua" )
   plot( predictions$binomial ~ Data.ref[,VARpays] , pch='.' , col = "yellow" , xlab = VARpays , ylab = "prediction" )

      range(resids)
   plot.variable(resids,breaks=c(-8e3,-5e3,-2e3,-1e2,1e2,2e3,5e3,1e4,3e4),breaks.lines=TRUE)

###################################################################################################—•°
   save.model()           ## NIE NADPISZ BEZMYŒLNIE !!!
###################################################################################################—•°

rm(panova.agg.adds)


########################################################################################################################—•°
## 12.B. model gam(,family=poisson)  ###################################################################################—•°

cbind(unlist(mget(ls(pattern='^VAR'))))
FAMILY
Ynames

###########################################################—•°
## zmienna pomocnicza do zbalansowanego podzialu Data.ref
#   nulls0.table(Data.ref[,regressors$poisson[[1]]])
#   VARbalanced = "czy_bez_k"
#   table(Data.ref[,VARbalanced])
  #
cat(regressors$poisson[[1]],sep="\"\n,\"")

k = 3
regressors$poisson[[k]] = c( "saldo_start_sad_k"
,"saldo_start_k_kapital"
,"dpw_pol_k"
,"saldo_start_k_koszty"
,"dpo_start_k"
,"prop_koszt_do_saldo_start_przed_pol_k"
,"wiek"
#,"dp_tel_pol_k"                       #2
,"kwota_wplat_przed_pol_k"
,"dp_list_pol_k"
,"dp_dekl_pol_k"
,"dpd_start_k"
,"dp_wiz_pol_k"
,"prop_koszt_do_saldo_start_pol_k"
,"dzien_tel_pol_k"
,"dt_pol_k"
,"wojewodztwo_pol_k"
#,"ile_pglos_pol_k"                  #1
,"czas_polaczen_sek_razem_pol_k"     #1
,"dzien_wplata_pol_k"
,"dzien_dekl_pol_k"
,"koszt_przed_pol_k"
,"kwota_wplat_pol_k"
#,"dzien_ugoda_pol_k"                  #2
,"ile_telp_pol_k"
#,"dzien_list_pol_k"                 #1
,"kwota_sr_dekl_pol_k"
,"kwota_wplat_dekl_pol_k"
,"dt_dekl_pol_k"
,"rank_rodzaj_adres_pol_k"
,"czy_emeryt"
,"ile_wplat_pol_k"
,"kwota_ugoda_pol_k"
,"ile_wiz_pol_k"
,"prop_ile_ugoda_rata_zaplacone_pol_k"
)

cat(regressors$poisson[[3]],sep=")\"\n,\"s(")

k=12
regressors$poisson[[k]] = c( "s(saldo_start_sad_k)"
,"s(saldo_start_k_kapital)"
,"s(dpw_pol_k)"
,"s(saldo_start_k_koszty)"
,"s(dpo_start_k)"
,"s(prop_koszt_do_saldo_start_przed_pol_k)"
,"wiek"                                        #4-s
,"s(kwota_wplat_przed_pol_k)"
,"dp_list_pol_k"                                                     #10-s
,"s(dp_dekl_pol_k)"
,"dpd_start_k"                                          #6-s
#,"dp_wiz_pol_k"                                             #7-s #8
,"s(prop_koszt_do_saldo_start_pol_k)"
#,"dzien_tel_pol_k"                             #4-s #5
,"s(dt_pol_k)"
,"wojewodztwo_pol_k"
,"s(czas_polaczen_sek_razem_pol_k)"
,"s(dzien_wplata_pol_k)"
#,"s(dzien_dekl_pol_k)"                                                   #11
,"s(koszt_przed_pol_k)"
,"s(kwota_wplat_pol_k)"
,"s(ile_telp_pol_k)"
,"s(kwota_sr_dekl_pol_k)"
,"s(kwota_wplat_dekl_pol_k)"                                    #9-s
#,"dt_dekl_pol_k"                                    #6                   #11
#,"rank_rodzaj_adres_pol_k"                    #4
,"czy_emeryt"
,"ile_wplat_pol_k"
#,"s(kwota_ugoda_pol_k)"                       #4
,"ile_wiz_pol_k"
#,"prop_ile_ugoda_rata_zaplacone_pol_k"        #4
)

      #for(k in 1:7){summaries$binary[[k]] = summary_k}

summaries$poisson[[k]] = summary_k
formulas$poisson[[k]] = paste0( VARYpoisson , " ~ " , paste(regressors$poisson[[k]] , collapse=" + " ))
rm(summary.df_k) ## rm(panova.agg.adds)
for(j in 1:30){
   ll_kj <- split.rand.portion(Data.ref,split=TRUE,portion=.8,envir=0)#,variable=VARbalanced)
      model_kj <- gam( formula(formulas$poisson[[k]]) , family=poisson , data=ll_kj$datfram.1 )
   summary.gam.panova( model_kj , what=c("coef","pval") , sstars=TRUE
                     , add=c("mse","aic","gini","auc","p.mww","r2","cvr2") , newdata=ll_kj$datfram.0  ## CV
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
   , add.colnames = c(TRUE,FALSE,TRUE,TRUE,FALSE))
(summaries$poisson[[k]]$folds$agg$pars = summary.df_k.agg)

   summaries$poisson[[6]]$folds$agg$pars

      ## for(k in 1:7){summaries$binomial[[k]]$folds$agg$pars = summaries.agg.a$binomial[[k]]}

# rm(panova.agg.adds)
# for(k in 1:12){summary.df_k <- summaries$poisson[[k]]$folds$raw
summary.gam.panova.agg.adds( summary.df_k
      , FUN = list( "MSE"  = function(x){c("min"=min(x),"mean"=mean(x),"max"=max(x))}
                  , "AIC"  = function(x){c("min"=min(x),"mean"=mean(x),"max"=max(x))}
                  , "Gini" = function(x){c("min"=min(x),"mean"=mean(x),"max"=max(x))}
                  , "AUC"  = function(x){c("min"=min(x),"mean"=mean(x),"max"=max(x))}
                  , "p.mww" = function(x){c("mean"=mean(x))}
                  , "R2"    = function(x){c("min"=min(x),"mean"=mean(x),"max"=max(x))}
                  , "CV_R2" = function(x){c("min"=min(x),"mean"=mean(x),"max"=max(x))}
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
split.stats( predict(model_kj,type="response") , Data.ref_kj[,VARefficiency_1] == 1  , plot = TRUE )

rm(panova.agg.adds)


###############################################################################—•°
## Modele na ca³oœci danych w oparciu o wyniki badañ na podzbiorach danych

#!  rm(whole.panova.agg.adds)
#for(k in 1:length(formulas$poisson)){ #1   # k=7
   print(k)
   model_k = gam(formula(formulas$poisson[[k]]),family=poisson,data=Data.ref)
   summaries$poisson[[k]]$whole$raw = summary.gam.panova(model_k,what=c("coef","pval"),sstars=TRUE,add=c("mse","aic","gini","auc","p.mww","r2"))
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
   split.stats( predict(model_k,type="response")
               , Data.ref[,VARefficiency_1] == 1      #!#!#!#!!!!!!
               , plot = TRUE )
      #
#}  ## END of #1
      #
summaries_agg$poisson$whole = whole.panova.agg.adds ;


summaries_agg
summaries$poisson[[k]]

###################################################################################################—•°
   save.image(file=FILEmodel.environment)           ## NIE NADPISZ BEZMYŒLNIE !!!
###################################################################################################—•°

###############################################################################—•°
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
#!#  Wybieramy model 10   #!#!#!#!#!#!#  (generalnie model do bani! lepszy binomial!)
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#

regressors$final$poisson$s = regressors$poisson[[10]]
regressors$final$poisson$x = gsub('s\\(','',gsub(')','',regressors$final$poisson$s))
formulas$final$poisson   = formulas$poisson[[10]]
models$poisson = gam(formula(formulas$final$poisson),family=poisson,data=Data.ref)

windows();par.my()
split.stats( predict( models$poisson , type="response") , Data.ref[,VARefficiency_1] == 1 , plot = TRUE ) #!!! nie ma sensu dla poisson !!!

###############################################################################—•°
## odzyski faktyczne a przewidywane z binomial

   predictions$poisson = predict(models$poisson,type="response")
   	sum(predictions$poisson)
   	sum(Data.ref[,VARpays])

   windows(height = 10, width = 7 ); par.my()
   plot( Data.ref[,VARpays] ~ predictions$poisson , pch='.' , col = "yellow" , ylab = VARpays )
     	title("wp³aty rzeczywiste vs przewidywane poisson")
   	lm.poisson = lm(Data.ref[,VARpays] ~ predictions$poisson)
   	abline(lm.poisson,col="white");
   	text(	  x = max(predictions$poisson)/2 , y = max(Data.ref[,VARpays])*.9
   			, labels = paste0( "y = " , paste( paste0( round( lm.poisson$coef, 3 ) , c(""," * x") ), collapse = " + ") )
   		 )
	#  abline(h=0,col="white")
	#  legend( "topleft" , legend = unique(Data.ref$grupa_portfela) , col = unique(as.numeric(Data.ref$grupa_portfela)) + 1, pch = 20 )

      #!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
      sum(cbind(1,predictions$poisson) %*% cbind(lm.poisson$coef))
      #!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#

         names(models$poisson)
      resids <- Data.ref[,VARYpoisson] - predictions$poisson
   windows(height = 12, width = 6 ); par.my() ; par(mfrow=c(2,1))
   plot( resids ~ Data.ref[,VARpays] , pch='.' , col = "yellow" , xlab = VARpays , ylab = "residua" )
   plot( predictions$poisson ~ Data.ref[,VARpays] , pch='.' , col = "yellow" , xlab = VARpays , ylab = "prediction" )

      range(resids)
   plot.variable(resids,breaks=c(-1.1e4,-5e3,-2e3,-1e2,1e2,2e3,5e3,1e4,3e4),breaks.lines=TRUE)


###################################################################################################—•°
   save.image(file=FILEmodel.environment)           ## NIE NADPISZ BEZMYŒLNIE !!!
###################################################################################################—•°


########################################################################################################################—•°
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


########################################################################################################################—•°
########################################################################################################################—•°
## IV. ZAPIS DO PLIKU
#!######################################################################################################################—•°

###################################################################################################—•°
## A. Cale œrodowisko

save.image(file=FILEmodel.environment,compress="xz")				## NIE NADPISZ BEZMYŒLNIE!!!

###################################################################################################—•°
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


########################################################################################################################—•°


        
        
        
        
