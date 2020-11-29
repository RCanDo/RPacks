########################################################################################################################•°
*## DZIA£ ANALIZ ## ANALIZY ## MODELE STATYSTYCZNE ## A.K. ## 150413
########################################################################################################################•°
  rm(list=ls())     ## czyszczenie przestrzeni roboczej
########################################################################################################################•°
#! MODEL ###############################################################################################################•°
########################################################################################################################•°
## CEL 0:
##
## OPIS: referencja to
##
## DOCELOWY MODEL:
##		- scoring na wplaty
##
#################################################################
########################################################################################################################•°
ID = "160302"   ## takie samo jak nazwa katalogu ##  w przybli¿eniu data powstania;
DATEmodel = as.POSIXct("2016-05-09") ## data modelu
ENTITY = ""
##
METHOD = "gam"
prediction.name = "score"            ## np: "score" w przypadku scoringu ; "prognoza"  dla innych metod
##
IDdataref = "160301"						 ## katalog z danymi referencyjnymi na których by³ zudowany model
IDref = "1"
Ysuffix = ""      		 ## przyrostek nazwy tabeli ze zmienn¹ objaœnian¹ ## TYLKO JEDEN W CA£YM PLIKU !!!  inny Y = inny model = inny plik i katalog modelu
##
##  BD :  toudi.modele.
##
##    Utworzono zmienne nazw tabel:
##                  TABLE
##   TABLEdataref   "modele.referencja_160301"
##   TABLEdataref_Y "modele.referencja_160301_"
##   TABLEmodel     "modele.referencja_160301_model_160302"
##
##    Utworzono zmienne nazw plików:
##                         FILE
##   FILEdataref.Rdata     "referencja_160301_1.Rdata"
##   FILEmodel_Y           "model_160302_Y.sql"
##   FILEmodel.environment "model_160302_environment.Rdata"
##   FILEmodel.Rdata       "model_160302.Rdata"
##   FILEmodel.sql         "model_160302.sql"
##   FILEmodel.xls         "model_160302.xls"
##   FILEmodel.csv         "model_160302.csv"
##
##    Utworzono zmienne nazw katalogów:
##               PATH
##   PATHdataref "//10.1.1.5/PK_Analitycy/Modele/Dane Referencyjne/160301"
##   PATHmodel   "//10.1.1.5/PK_Analitycy/Modele/Modele/160302"
##   PATHwd      "//10.1.1.5/PK_Analitycy/Modele/Modele/160302"
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
        #!   save.image(file=FILEpred.environment , compress="xz")				## NIE NADPISZ BEZMYŒLNIE!!!
        ## lub zaladuj srodowisko gdy wznawiasz prace
         load(file=FILEmodel.environment) ; detach("package:TinnRcom", unload=TRUE) ; library(TinnRcom) ## unloadNamespace("TinnRcom")  may work too
        #!###########################################################################
        ## tylko modele (i dane)
         load(file=FILEmodel.Rdata)


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
##    1. Niektore komendy sa zakomentowane jako potencjalnie niebezpieczne.
##       Wywolujemy je stawiajac kursor po znaku komentarza i klikajac "play" na pasku.
##       Czynimy to po upewnieniu sie, ze uruchomienie danej komendy
##       jest dokladnie tym czego wlasnie chcemy.
##    2. Komendy niebezpieczne to te, ktore uruchomione nieprawidlowo beda trudne do korekty
##       i konieczne bedzie przeprowadzenie od nowa niemal calej analizy.
##    3. SKRÓTY:
##       • E0 - etap 0 tj. ladowanie danych z bazy i ich wstêpna obróbka (eliminacja bledow; bez metod statystycznych)
##       • E1 - etap 1 tj. wlasciwa obróbka danych (eliminacja obserwacji odstajacych; z metodami statystycznych, np. uzupelnianie NULLi z uzyciem RF)
##       • E2 - etap 2 tj. budowa modelu
##       • E3 - etap 3 tj. prognoza w oparciu o model z E2.
##    4. Podstawowe ramki danych:
##       • E0 - data.raw0 --> data.raw --> Data.raw
##       • E1 - Data.raw --> data.ref --> Data.ref
##       • E2 - Data.ref, Data.ref_y (zmienne Y, mog¹ juz byc w Data.ref)
##       • E3 - Data.ref;    data.new0 --> data.new --> Data.new --> Prediction.df
##
########################################################################################################################•°

########################################################################################################################•°
## 3. PAKIETY I SKRYPTY ################################################################################################•°

## PAKIETY
packages = c( "MASS",
			  	  "RPostgreSQL","RODBC"#,"vioplot"
                , "rpart", "earth" ,
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
## 4. POBRANIE DANYCH ##################################################################################################•°

###############################################################################•°
## a. DANE REFERENCYJNE  ######################################################•°

## pierwsze ³adowanie danych referencyjnych z	DOMAIN/Dane Referencyjne/YYMMDD

## POBRANIE z .Rdata (jesli bylo wczesniej zapisane)  ## szybsze
load(file = paste(PATHdataref,FILEdataref.Rdata,sep="/") )
## lub z .csv
# Data.ref = read.csv(paste0(FILEdataref0,".csv"),sep=";",dec=",")

   dim(Data.ref)   ## 7155  122
   names.ref = names(Data.ref)
   (nulls.tab = nulls0.table(Data.ref))

###############################################################################•°
## b. PODSTAWOWE OBIEKTY -- przegl¹d  #########################################•°

ls(pattern='^(D|d)ata')

###########################################################•°
## ZMIENNE

ls(pattern='^VAR')
cbind(unlist(mget(ls(pattern='^VAR'))))
   VARdebt = "saldo_stop_0_k"          #! to jest logx(saldo_stop_0_k,.1)
      VARdebt0 <-VARdebt1<- "saldo_stop_0_k.1"    #! to jest oryginalne  saldo_stop_0_k
   VARid   = "id_produkt_k"
   VARid2  = "id_produkt_glowny"
      VARid1 = VARid2
   VARY    = "wplaty_suma"
      VARpays = VARY
      VARefficiency = "skutecznosc"
      VARefficiency_binomial = "skutecznosc_binomial"
      VARY1   = VARefficiency
      VARY1groups <- VARefficiency_groups <- "skutecznosc_grupy"  ## do utworzenia
setdiff(unlist(mget(ls(pattern='^VAR'))),names.ref)  ##  0 ok!



###########################################################•°
## LISTY transformacji z E1 (etapu obróbki danych)

ls(pattern='^trans\\.')
trans.list.tofactor
trans.list

###############################################################################•°
## c. POPRAWKI do danych z BD  ## oby jak najmniej!  ##########################•°


###########################################################•°
## DANE REFERENCYJNE Y ## zazwyczaj wp³aty w zadanym okresie

   ## TUTAJ NIEPOTRZEBNE - y jest w Data.ref

   ## pierwsze ³adowanie danych referencyjnych z	DOMAIN/Dane Referencyjne/YYMMDD
      ch <- odbcConnect("PostgreSQL35W-T", uid = "arkadiusz.kasprzyk", pwd = "akasprzyk123") ## TOUDI

   	( query = paste("SELECT * FROM ", TABLEdataref , sep = " ") )

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
## d. £¥CZENIE DANYCH : [ X , Y ] i nie tylko #################################•°
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
##    Powinny byc przewidziane w E1 a nawet wczesniej (SQL), jednak nie wszystko da siê przewidziec.
##

variables.new.condlist = list()
names.newmod = character(0)

   ###################################################################################################•°
   ## SZABLON, np. ###################################################################################•°
   with(Data.ref, plot.variable(v1/v2) )
   with(Data.ref, { windows();coldef.black(); plot(v2,v1)} )

variables.new.condlist[["vx"]] = list(  ## zob. pomoc w condapply()  (condapply.r)
                                 conditions = quote("all")
                               , inputs = c("v1","v2","..")
                               , substitute = quote( f(a,b,..) )
                               , plot = TRUE
                               )

   vx = condapply( Data.ref , variables.new.condlist["vx"] , return.all = FALSE )

      with(datfram, plot.variable(vx) )
      with(datfram, plot.variable(vx[vx < 10]) )

   #!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
   ## jeœli OK (po skonstruowaniu wszystkich dodatkowych zmiennych)
   Data.ref = condapply( Data.ref , variables.new.condlist ) ; rm(vx)
   #!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
   ###################################################################################################•°


###################################################################################################•°
## 6.1 skutecznosc_gam ############################################################################•°
with(Data.ref, plot.variable(get(VARY1),main=VARY1) )
               with(Data.ref, plot.variable(get(VARY)/get(VARdebt1)) )  ## to samo
with(Data.ref, { windows();coldef.black(); plot(get(VARdebt1),get(VARY),pch=".",xlab=VARdebt1,ylab=VARY)} )

variables.new.condlist[["skutecznosc_binomial"]] = list(
                     variables = c()
                   , conditions = quote("all")
                   , inputs = c(VARdebt1,VARY)
                   #, substitute = body(function(){ max.debt_pay <- pmax(a,b) ; fails <- max.debt_pay - b ; round(cbind(b,fails)) })
                   , substitute = quote({ max.debt_pay <- pmax(a,b) ; fails <- max.debt_pay - b ; round(cbind(b,fails)) })
                   #, plot = TRUE
      )

   vx = condapply( Data.ref , variables.new.condlist["skutecznosc_binomial"] , return.all = FALSE )
   head(vx,50)

   with(datfram, {   vx <- skutecznosc_binomial ; skutecznosc <- vx[,1] / rowSums(vx)
                     plot.variable(skutecznosc)
                     plot.variable(skutecznosc[skutecznosc<.2])
                  } )

#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
## jeœli OK to
names.newmod = c("skutecznosc_binomial")
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#

VARefficiency_binomial = "skutecznosc_binomial"

###################################################################################################•°
## 6.2 skutecznosc_grupy ##########################################################################•°

cbind(unlist(mget(ls(pattern='^VAR'))))
VARY1groups <- VARefficiency_groups <- "skutecznosc_grupy"

variables.new.condlist[["skutecznosc_grupy"]] = list(
                           variables = c()
                        ,  conditions = "all"
                        ,  inputs = c("skutecznosc")
                        ,  substitute = quote(
                                    cut( a , breaks = monetary(  max(a) , power.range = 3 , base = 1, merge.last = 1)
                                        , include.lowest = TRUE)
                                    )
                        ,  plot = TRUE
                        )

vx = condapply( Data.ref , variables.new.condlist["skutecznosc_grupy"] , xlab = "skutecznosc_grupy" , return.all = FALSE)#, as.factor=TRUE)
        table(vx,useNA="ifany")

 ( table.number.VARY1groups = as.table(tapply(Data.ref[,VARdebt0],vx,length)) )
 ( table.debt.VARY1groups = as.table(tapply(Data.ref[,VARdebt0],vx,sum)) )
 ( table.pay.VARY1groups = as.table(tapply(Data.ref[,VARY],vx,sum)) )

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

#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
## jeœli OK to
names.newmod = c(names.newmod,"skutecznosc_grupy")
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#

###################################################################################################•°
## NA KONIEC (po skonstruowaniu wszystkich zmiennych) #############################################•°

   Data.ref = condapply( Data.ref , variables.new.condlist ) ; rm(vx)

   save.image(file=FILEmodel.environment)           ## NIE NADPISZ BEZMYŒLNIE !!!

########################################################################################################################•°
## 7. GRUPY ZMIENNYCH ##################################################################################################•°
## dziedziczymy z etapu 1 ale musimy zaktualizowaæ (wykluczyæ zmienne odrzucone w etapie 1)

(nulls.tab = nulls0.table(Data.ref))
(names.ref = names(Data.ref))
names.newmod

update.names.ref2mod()  ## zdefiniowane w    DOMAIN/Funkcje/funkcje_modele.r

ref.names.vec           ## grupy nazw zmiennych z E1 (nazwy wektorów nazw)
ref.names.list          ## grupy (wektory) nazw z E1 zapisane do listy
ref_not_mod.names.list  ## zmienne (w odpowiednich grupach), które byly w E1 ale zostaly usuniête (bo byly slabe)
mod.names.list          ## zmienne (w odpowiednich grupach), które przeszly do E2

###############################################################################•°
## a. identyfikatory

   nulls.tab[ nulls.tab$class == "factor" , "name" ]

   cat(intersect(names.id ,names.ref), sep = "\"\n,\"")

names.id = c(  "id_produkt_k"
,"id_produkt_glowny"
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
   ,"id_bank_glowny"     #!!!
   ,"grupa_portfela_glowny"
)

  setdiff(names.id,names.ref)   ## 0
  setdiff(names.id,names.ID)
  setdiff(names.ID,names.id)    ## 0

###############################################################################•°
## b. inne

names.regressors  ## ustalone w pliku z etapu obróbki danych  dane_referencyjne_YYMMDD_1.r
                  ## tak jak i poni¿sze grupy zmiennych:
   intersect(names.regressors,names.id)
   intersect(names.regressors,names.ID)
   setdiff(names.ref,union(names.regressors,names.id))
   setdiff(names.ref,union(names.regressors,names.ID))


##
( names.dates = grep('^data',names.ref,value=TRUE) )    ## tu nie ma juz dat
( names.saldo = grep('^saldo',names.ref,value=TRUE) )
names.full
names.tofill

## pomocnicze grupy zmiennych
(names.dt = grep('^dt',names.ref,value=TRUE))
(names.dp = grep('^dp',names.ref,value=TRUE))
(names.czy = grep('^czy',names.ref,value=TRUE))
(names.kat = grep('^kat',names.ref,value=TRUE))
(names.kwota = grep('^kwota',names.ref,value=TRUE))

(names.ile = grep('^ile',names.ref,value=TRUE))
(names.factor = colnames.class(Data.ref,"factor"))
   sapply(Data.ref[,intersect(names.ile,names.factor)],function(x){length(unique(x))}) ##
   ## ile_wplat_przed_0_k       ile_ugoda_0_k
   ##               2                   3

(names.wplaty = grep('^wplaty',names.ref,value=TRUE))
(names.skutecznosc = grep('^skutecznosc',names.ref,value=TRUE))

cbind(unlist(mget(ls(pattern='^VAR'))))


   save.image(file=FILEmodel.environment)           ## NIE NADPISZ BEZMYŒLNIE !!!

########################################################################################################################•°
## 7. SZYBKI PRZEGL¥D ##################################################################################################•°

	attach(Data.ref)     ## dla wygody, ALE nalezy uwazac by uzupelniac w Data.ref, tj.  Data.ref$zmienna[ index ] = ...
   ## po X zmiennych

   names.tosee = setdiff( names.ref , union(names.id,names.dates) )

rows = 3 ; cols =6  ## ile wierszy i kolumn wykresow w jednym oknie
pages = ceiling(length(names.tosee)/rows/cols)
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
      for(k in ((rows*cols*(pg-1))+1):min((rows*cols*pg),length(names.tosee))){
         var.name = names.tosee[k]
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

rf_payments = randomForest( y = Data.ref[,VARY], x = Data.ref[,names.regressors] , importance=TRUE)
windows();varImpPlot(rf_payments)

rf_efficiency = randomForest( y = Data.ref[,VARY1], x = Data.ref[,names.regressors] , importance=TRUE)
windows();varImpPlot(rf_efficiency)

names(rf_payments)

varImp = data.frame(  payments = I(apply(rf_payments$importance,2,function(x){order(x,decreasing=TRUE)}))
                   , efficiency = I(apply(rf_efficiency$importance,2,function(x){order(x,decreasing=TRUE)}))
             )
varImp$pay[,1]
varImp$eff[,1]

   save.image(file=FILEmodel.environment)           ## NIE NADPISZ BEZMYŒLNIE !!!

########################################################################################################################•°
## 9. Eliminacja zmiennych skorelowanych  ##############################################################################•°

###################################################################################################•°
## a. wektory zmiennych  ##########################################################################•°

###############################################################################•°
## payments -- VARY = "wplaty_suma"

cat(names.regressors[varImp$pay[,"IncNodePurity"]],sep="\"\n,\"")  ## the higer the more important (according to RF)

regressors.payments = c(  "kwota_sr_rata_ugoda_o_0_k"
  ,"dpw_0_k"
  ,"ile_ugoda_rata_zaplacone_0_k"
  #,"kwota_wplata_o_0_k"
  #,"kwota_wplat_ugoda_0_k"
#,"kwota_ugoda_o_0_k"
#,"saldo_start_ugoda_o_0_k"
#,"prop_ile_ugoda_rata_zaplacone_0_k"
#,"kwota_wplat_0_k"
#,"kwota_ugoda_0_k"
  ,"dp_tel_0_k"
,"dp_ugoda_0_k"
  ,"dp_wiz_0_k"
#,"ile_ugoda_rata_0_k"
,"dpo_start_k"
,"saldo_start_k_odsetki"
,"dp_list_0_k"
#,"saldo_stop_kapital"
#,"saldo_stop_odsetki"
#,"skutecznosc_0_k"
#,"ile_wplat_0_k"
  ,"dpd_start_0_k"
  ,"prop_koszt_do_saldo_start_0_k"
,"dzien_tel_0_k"
  ,"koszt_0_k"
#,"dp_dekl_0_k"
#,"czas_polaczen_sek_razem_0_k"
#,"dpd_start_k"
,"dpd_stop_0_k"
  ,"dzien_wiz_0_k"
#,"prop_kwota_wplat_ugoda_0_k"
#,"saldo_start_k_kapital"
#,"kwota_wplata_p_0_k"
#,"dt_ugoda_0_k"
 #,"id_status_produkt_0_k"   ## chi2
#,"wartosc_sporu_0_k"
,"wiek"
  ,"prop_koszt_do_saldo_start_przed_0_k"
#,"prop_wartosc_sporu_saldo_start_pol_k"
#,"kwota_wplat_dekl_0_k"
   ,"kwota_sr_dekl_0_k"
#,"saldo_start_0_k"
,"dtu"
,"saldo_start_k_koszty"
,"ile_telw_0_k"
#,"saldo_start_sad_k"
#,"prop_kwota_ugoda_saldo_start_0_k"
,"ile_telp_0_k"
#,"saldo_start_pol_k"
,"dzien_list_0_k"
#,"prop_kwota_wplat_ugoda_saldo_start_0_k"
,"dzien_wplata_0_k"
,"wojewodztwo_0_k"
#,"ile_tel_0_k"
#,"kwota_dekl_o_0_k"
,"saldo_stop_koszty"
,"prop_wartosc_sporu_saldo_stop_0_k"
#,"saldo_start_kom_k"
  ,"dzien_ugoda_0_k"
      ,"saldo_stop_0_k"    #?? powinno byæ wyrzucone
#,"ile_list_0_k"
#,"prop_kwota_wplat_dekl_0_k"
#,"ile_list_przed_0_k"
#,"dzien_dekl_0_k"
#,"dt_pol_k"
   ,"prop_kwota_wplat_dekl_saldo_start_0_k"
#,"ile_wiz_0_k"
#,"koszt_przed_0_k"
#,"dt_0_k"
,"ile_pglos_0_k"
  ,"dt_sad_k"
  ,"skutecznosc_przed_0_k"
#,"kwota_dekl_0_k"
,"id_bank_glowny"
#,"kwota_wplat_przed_0_k"
#,"kwota_wplata_o_przed_0_k"
#,"ile_ugoda_0_k"              ## chi2
#,"prop_kwota_dekl_saldo_start_0_k"
#,"prop_ile_dekl_dt_0_k"
  ,"dt_dekl_0_k"
,"dt_kom_k"
#,"rodzaj_strony"              ## chi2
#,"ile_dekl_0_k"
#,"prop_ile_dekl_zaplacone_0_k"
#,"rank_rodzaj_adres_0_k"      ## chi2
#,"czy_wiz_0_k"
#,"rank_rodzaj_telefon_0_k"    ## chi2
#,"czy_kom_k"
#,"ile_dekl_zaplacone_0_k"
  ,"czy_epu_0_k"
#,"kat_rodzaj_postepowania_0_k" ## chi2
#,"kat_tryb_postepowania_0_k"  ## chi2
,"czy_aktywny_telefon_0_k"
#,"typ_klienta_k"          ## chi2
#,"grupa_portfela_glowny"  ## chi2
  ,"czy_list_0_k"
#,"czy_sad_k"
#,"czy_wplata_0_k"
#,"czy_dekl_0_k"
#,"ile_wplat_przed_0_k"    ## chi2
  ,"czy_tel_0_k"
#,"czy_ugoda_0_k"
  ,"czy_bez_k"
#,"dt_bez_k"
)    ## ...

#######################################•°
## kategoryczne

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

###############################################################################•°
## efficiency -- VARY1 = "skutecznosc"

cat(names.regressors[varImp$eff[,"IncNodePurity"]],sep="\"\n,\"")  ## the higer the more important (according to RF)

regressors.efficiency = c( "skutecznosc_0_k"
#,"prop_ile_ugoda_rata_zaplacone_0_k"
  ,"prop_kwota_wplat_ugoda_0_k"
  ,"dpw_0_k"
#,"saldo_start_kom_k"
  ,"saldo_stop_kapital"
,"dp_list_0_k"
  ,"saldo_stop_odsetki"
#,"saldo_stop_0_k"
  ,"dp_tel_0_k"
#,"saldo_start_k_kapital"
#,"saldo_start_sad_k"
  ,"prop_wartosc_sporu_saldo_stop_0_k"
,"dpd_stop_0_k"
  ,"prop_kwota_wplat_dekl_0_k"
  ,"dp_dekl_0_k"
#,"ile_ugoda_rata_zaplacone_0_k"
,"wiek"
#,"prop_kwota_wplat_ugoda_saldo_start_0_k"
  ,"koszt_0_k"
  ,"prop_koszt_do_saldo_start_0_k"
#,"id_status_produkt_0_k"                     ## chi2
 #,"ile_wplat_0_k"
#,"czas_polaczen_sek_razem_0_k"
#,"prop_wartosc_sporu_saldo_start_pol_k"
#,"wartosc_sporu_0_k"
,"wojewodztwo_0_k"                           ## chi2
#,"saldo_start_0_k"   #? czy powinno zostaæ
#,"kwota_wplata_o_0_k"
  ,"ile_tel_0_k"
#,"saldo_start_k_odsetki"
#,"kwota_wplata_p_0_k"
  ,"dp_wiz_0_k"
#,"saldo_start_pol_k"
,"dpo_start_k"
,"ile_pglos_0_k"
  ,"dpd_start_k"
  ,"dzien_ugoda_0_k"
#,"ile_ugoda_rata_0_k"
#,"prop_kwota_ugoda_saldo_start_0_k"
#,"ile_telw_0_k"
,"dzien_tel_0_k"
#,"prop_kwota_wplat_dekl_saldo_start_0_k"
#,"ile_telp_0_k"
#,"kwota_wplat_0_k"
,"dtu"
#,"dt_ugoda_0_k"
#,"dpd_start_0_k"
,"dzien_wplata_0_k"
#,"kwota_sr_rata_ugoda_o_0_k"
,"saldo_start_k_koszty"
#,"kwota_ugoda_o_0_k"
#,"dzien_dekl_0_k"
  ,"skutecznosc_przed_0_k"
#,"kwota_wplat_dekl_0_k"
#,"kwota_wplat_ugoda_0_k"
  ,"dzien_wiz_0_k"
  ,"ile_list_0_k"
  ,"dt_sad_k"
  ,"dt_bez_k"
#,"saldo_start_ugoda_o_0_k"
,"prop_koszt_do_saldo_start_przed_0_k"
#,"kwota_wplat_przed_0_k"
#,"dt_pol_k"
,"dzien_list_0_k"
#,"dt_0_k"
#,"kwota_ugoda_0_k"
,"dt_kom_k"
#,"rodzaj_strony"                ## chi2
#,"prop_ile_dekl_zaplacone_0_k"
      ,"prop_kwota_dekl_saldo_start_0_k"
,"saldo_stop_koszty"
#,"kwota_wplata_o_przed_0_k"
#,"kwota_dekl_0_k"
#,"kwota_dekl_o_0_k"
#,"prop_ile_dekl_dt_0_k"
,"dp_ugoda_0_k"
#,"kwota_sr_dekl_0_k"
      ,"ile_ugoda_0_k"                 ## chi2
#,"dt_dekl_0_k"
#,"ile_wiz_0_k"
#,"koszt_przed_0_k"
#,"ile_list_przed_0_k"
#,"czy_tel_0_k"
      ,"id_bank_glowny"                ## chi2
#,"rank_rodzaj_telefon_0_k"      ## chi2
#,"kat_rodzaj_postepowania_0_k"  ## chi2
#,"czy_wiz_0_k"
,"czy_aktywny_telefon_0_k"
#,"kat_tryb_postepowania_0_k"    ## chi2
#,"czy_list_0_k"
  ,"czy_epu_0_k"
#,"typ_klienta_k"                ## chi2
#,"ile_wplat_przed_0_k"          ## chi2
#,"ile_dekl_zaplacone_0_k"
#,"czy_bez_k"
#,"ile_dekl_0_k"
#,"czy_kom_k"
#,"czy_sad_k"
#,"rank_rodzaj_adres_0_k"        ## chi2
#,"czy_wplata_0_k"
#,"czy_dekl_0_k"
#,"czy_ugoda_0_k"
#,"grupa_portfela_glowny"        ## chi2
)

#######################################•°
## kategoryczne

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

###################################################################################################•°
## b. Korelacje (grafy) - zmienne ci¹g³e ##########################################################•°
rho =.7 #  =.5  =.3   # wazne jest tylko r=.7
 windows()
 #! ponizsza funkcja znajduje korelacje jedynie dla zmiennych liczbowych !!!   I dobrze!
 (graf0 <- graf(zmienne.graf = regressors.efficiency, dane = Data.ref,
      r=rho,
      zmienne.bez.null = regressors.efficiency,
      target = "skutecznosc", ver = 2,               #! bledy dla  ver=1 ## wysypuje sie gdy nie ma zadnych korelacji (powyzej zadanego r)
      metoda = list( c(1) , c("pearson", "kendall", "spearman") ) ))

 graf0$`korelacja zmiennych zaleznych`[c(5,6,8,9),c(5,6,8,9)]

   setdiff(regressors.binary,regressors.binomial)
   setdiff(regressors.binomial,regressors.binary)

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
chisqs = chisq.multi.test(Data.ref[,regressors.efficiency])
  ## nie przejmujemy sie ostrzezeniami
chisqs$cor.pairs ## "skorelowane" (a raczej zalezne) pary zmiennych kategorycznych, tj. tylko te pary dla których p<0.05 (tak male P(para zmiennych niezale¿na))

###################################################################################################•°


########################################################################################################################•°
## 8. Random Forest -- ponowna ocena istotnosci zmiennych  #############################################################•°
##                     tylko wybrane zmienne               #############################################################•°

rf_payments_1 = randomForest( y = Data.ref[,VARY], x = Data.ref[,regressors.payments] , importance=TRUE)
windows();varImpPlot(rf_payments_1)

rf_efficiency_1 = randomForest( y = Data.ref[,VARY1], x = Data.ref[,regressors.efficiency] , importance=TRUE)
windows();varImpPlot(rf_efficiency_1)

   save.image(file=FILEmodel.environment)           ## NIE NADPISZ BEZMYŒLNIE !!!


########################################################################################################################•°
########################################################################################################################•°
## III. BUDOWA MODELU
########################################################################################################################•°

########################################################################################################################•°
## UWAGA: Poszukuj¹c wzorca do budowy modelu idŸ do pkt.   2. model gam(,family=poisson)
## (pkt 1. ju¿ OK ale nie przetestowany - jednak nie powinno byæ niespodzianek)
########################################################################################################################•°

########################################################################################################################•°
## 1. model gam(,family=binomial(link=logit))  #########################################################################•°
VARY.binomial = VARefficiency_binomial

###########################################################•°
## zmienna pomocnicza do zbalansowanego podzialu Data.ref
Data.ref$czy_wplata = Data.ref[,VARY] == 0 ; table(Data.ref$czy_wplata)
nulls0.table(Data.ref)
  #
###########################################################•°
#!#!  Listy: summary. , regressors. , formula.
#!#!    summary.binomial.list = list()   ## nowa lista z tabelami modeli ## uwazaj by nie usunac!!!!!
#!#!    regressors.binomial.list = list()
#!#!    formula.binomial.list = list()
###########################################################•°

cat(regressors.efficiency,sep="\"\n,\"")

k = 4
regressors.binomial.list[[k]] = c( "skutecznosc_0_k"
,"prop_kwota_wplat_ugoda_0_k"
,"dpw_0_k"
,"saldo_stop_kapital"
#,"dp_list_0_k"                               #3
,"saldo_stop_odsetki"
,"dp_tel_0_k"
#,"prop_wartosc_sporu_saldo_stop_0_k"         #3
#,"dpd_stop_0_k"                              #3
#,"prop_kwota_wplat_dekl_0_k"                 #3
,"dp_dekl_0_k"
#,"wiek"                                     #2
#,"koszt_0_k"                                #2
#,"prop_koszt_do_saldo_start_0_k"            #2
,"wojewodztwo_0_k"
#,"ile_tel_0_k"                             #1
#,"dp_wiz_0_k"                               #2
#,"dpo_start_k"                             #1
#,"ile_pglos_0_k"
,"dpd_start_k"                               #2
#,"dzien_ugoda_0_k"                          #2
#,"dzien_tel_0_k"                            #2
#,"dtu"                                     #1
#,"dzien_wplata_0_k"                         #2
,"saldo_start_k_koszty"
#,"skutecznosc_przed_0_k"                    #2
#,"dzien_wiz_0_k"                            #2
#,"ile_list_0_k"                            #1
#,"dt_sad_k"
#,"dt_bez_k"                                 #2
#,"prop_koszt_do_saldo_start_przed_0_k"     #1
#,"dzien_list_0_k"                          #1
#,"dt_kom_k"                                 #2
#,"prop_kwota_dekl_saldo_start_0_k"          #2
#,"saldo_stop_koszty"         #1
,"dp_ugoda_0_k"
,"ile_ugoda_0_k"
,"id_bank_glowny"
#,"czy_aktywny_telefon_0_k"  #1
#,"czy_epu_0_k"    #1
)

cat(regressors.binomial.list[[4]],sep=")\"\n,\"s(")

k=7
regressors.binomial.list[[k]] = c( "s(skutecznosc_0_k)"
,"s(prop_kwota_wplat_ugoda_0_k)"
,"s(dpw_0_k)"
,"s(saldo_stop_kapital)"
,"s(saldo_stop_odsetki)"    #5
,"s(dp_tel_0_k)"
,"s(dp_dekl_0_k)"
,"wojewodztwo_0_k"
,"s(dpd_start_k)"
,"s(saldo_start_k_koszty)"
,"s(dp_ugoda_0_k)"
,"ile_ugoda_0_k"
,"id_bank_glowny"           #6
)
formula.binomial.list[[k]] = paste0(VARY.binomial," ~ ",paste(regressors.binomial.list[[k]],collapse=" + "))

rm(summary.df_k)
for(j in 1:10){
   Data.ref_kj = split.rand.portion(Data.ref,split=TRUE,variable="czy_wplata",portion=.9)
   model_kj = gam(formula(formula.binomial.list[[k]]),family=binomial,data=Data.ref_kj)
   summary.df_k = summary.gam.panova(model_kj,what="coef",append.to="summary.df_k")
}
summary.df_k

## zapisujemy wyniki dla kazdego zestawu zmiennych do listy
summary.binomial.list[[k]] = summary.df_k
names(summary.binomial.list)[k] = paste( k , paste0("mean(AIC) = " , mean.aic(summary.df_k))   ## mean.aid() , mean.mse() zdefiniowane  w  summary.gam.ak()
                                           , paste0("mean(MSE) = " , mean.mse(summary.df_k)) , sep = " ; ")
summary.binomial.list

## MODEL NA CALOŒCI DANYCH (dla porównania)
model.gam_k = gam(formula(formula.binomial.list[[k]]),family=poisson,data=Data.ref)
summary.gam.panova(model.gam_k,what=c("coef","pval"))

#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
#!#  Wybieramy model 5  #!#!#!#!#!#!#!#
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#

regressors.gam.binomial = regressors.binomial.list[[5]]
formula.gam.binomial = paste0(VARY.binomial," ~ ",paste(regressors.gam.binomial,collapse=" + "))
model.gam.binomial = gam(formula(formula.gam.binomial),family=binomial(link=logit),data=Data.ref)


###########################################################•°
## odzyski faktyczne a przewidywane z binomial

   prediction.gam.binomial = predict(model.gam.binomial,type="response")*Data.ref[,VARdebt1]
	sum(prediction.gam.binomial)
	sum(Data.ref[,VARpays])

  windows(height = 6, width = 5 ); par.my()
  plot( Data.ref[,VARpays] ~ prediction.gam.binomial , pch='.' , col = "yellow" , ylab = VARpays )
  	title("wp³aty rzeczywiste vs przewidywane binomial")
	lm.pois = lm(Data.ref[,VARpays] ~ prediction.gam.binomial)
	abline(lm.pois,col="white");
	text(	  x = max(prediction.gam.binomial)/2 , y = max(Data.ref[,VARpays])*.9
			, labels = paste0( "y = " , paste( paste0( round( lm.pois$coef, 3 ) , c(""," * x") ), collapse = " + ") )
		 )
	#  abline(h=0,col="white")
	#  legend( "topleft" , legend = unique(Data.ref$grupa_portfela) , col = unique(as.numeric(Data.ref$grupa_portfela)) + 1, pch = 20 )



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
#,"prop_koszt_do_saldo_start_przed_0_k"   #1
#,"kwota_sr_dekl_0_k"                      #3
#,"dtu"                                   #1
,"saldo_start_k_koszty"
#,"ile_telw_0_k"                           #3
#,"ile_telp_0_k"                        #1
,"dzien_list_0_k"
#,"dzien_wplata_0_k"                    #2
,"wojewodztwo_0_k"
#,"saldo_stop_koszty"                   #2
,"prop_wartosc_sporu_saldo_stop_0_k"
#,"dzien_ugoda_0_k"                     #1
,"saldo_stop_0_k"
#,"prop_kwota_wplat_dekl_saldo_start_0_k"  #3
#,"ile_pglos_0_k"                       #3
,"dt_sad_k"
#,"skutecznosc_przed_0_k"      #1
,"id_bank_glowny"
#,"dt_dekl_0_k"                #1
#,"dt_kom_k"                   #1
#,"czy_epu_0_k"                   #3
#,"czy_aktywny_telefon_0_k"    #1
#,"czy_list_0_k"                  #2
#,"czy_tel_0_k"                   #3
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

(variables = mget(ls(pattern='^names\\.')))
names(variables) = gsub('^names\\.','',names(variables))

(condlists = mget(ls(pattern='\\.condlist$')))
names(condlists) = gsub('\\.condlist$','',names(condlists))

ls(pattern='^trans\\.')
trans = mget(ls(pattern='^trans\\.'))
names(trans) = gsub('^trans\\.','',names(trans))
   names(trans)[2] <- "tofactor"

      #!#!#!#!#! To powinno byæ w E1 !!!
      #!#!#!#!#!  lista oryginalnych poziomów zmiennych kategorycznych (data.raw) - bêdzie potrzebna do prognozy aby uzgodniæ poziomy tych zmiennch dla  data.new
      #    levels.raw = list()    #!#!
      for(fac in c( "typ_klienta_k" , "id_bank_glowny" , "id_status_produkt_0_k" , "ile_wplat_przed_0_k" , "ile_ugoda_0_k" , "rodzaj_strony"
                  , "kat_rodzaj_postepowania_0_k" , "kat_tryb_postepowania_0_k" , "wojewodztwo_0_k" , "rank_rodzaj_telefon_0_k" , "rank_rodzaj_adres_0_k" ) ){  #names(trans$toimprove.list)){
         levels.raw[[fac]] = levels(as.factor(data.ref[,fac]))
      }
      #!#!#!#!#!#!#!#!#!#!#!#!#!

ls(pattern='^model')

cat(ls(pattern='^regressors'),sep="\" , \"");cat("",sep="\n")

regressors = list()
regressors[["gam.binomial.s"]] = regressors.gam.binomial
regressors[["gam.poisson.s"]] = regressors.gam.poisson

formulas = list()
formulas$gam.binomial =  formula.binomial.list[[5]]
formulas$gam.poisson =  formula.poisson.list[[8]]


OBJECTS = c( ## z E0
             # "fill.condlist" , "greater_to_.condlist" , "greater_toNA.condlist" , "less_to_.condlist" , "less_toNA.condlist"
             ## z E1
              "Data.ref" , "data.ref" #, "Data.ref.nas" #, "data.ref.nas" , "data.ref.outs"
                  #   , ls(pattern='^names')
                  #   , "ref.names.vec"           ## grupy nazw zmiennych z E1 (nazwy wektorów nazw)
                  #   , "ref.names.list"          ## grupy (wektory) nazw z E1 zapisane do listy
                  #   , "ref_not_mod.names.list"  ## zmienne (w odpowiednich grupach), które byly w E1 ale zostaly usuniête (bo byly slabe)
                  #   , "mod.names.list"          ## zmienne (w odpowiednich grupach), które przeszly do E2
                  #   , ls(pattern='^VAR')
                  #   , "trans.list" , "trans.list.tofactor"
                  #   , "variables.new.condlist"
            , "variables" , "condlists" , "trans" , "levels.raw"
            ## E2
            , ls(pattern='^VAR')
            , "model.gam.binomial" , "model.gam.poisson"
            , "regressors"  ## = list("regressors.gam.binomial" , "regressors.gam.poisson")  ## finalne zestawy zmiennych
               ## poni¿sze nie potrzebne
               #   , "regressors.binomial.list" , "regressors.poisson.list"
               #   , "regressors.poisson" , "regressors.binomial" , "regressors.payments" , "regressors.efficiency"
               #   , "summary.binomial.list" , "summary.poisson.list"
            , "formulas"
               #   , "prediction.gam.binomial" , "prediction.gam.poisson"
            ##
            #, DFtrans )   ## informacja o przekszta³ceniach cech : lista przekszta³ceñ i df z t¹ list¹ i innymi statystykami
            )
OBJECTSmod = OBJECTS
OBJECTS = c(OBJECTS,"OBJECTSmod")

	save( list = OBJECTS , file = FILEmodel.Rdata , compress = "xz" )

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


        
        
        
        
