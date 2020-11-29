########################################################################################################################•
## DZIA£ ANALIZ ## ANALIZY ## MODELE STATYSTYCZNE ## A.K. ## 150413
########################################################################################################################•
  rm(list=ls())     ## czyszczenie przestrzeni roboczej
########################################################################################################################
#! MODEL ###############################################################################################################
########################################################################################################################
## CEL 0:  dane do modelu na skutecznosc wplatowa w 12mo po wysy³ce do s¹du w oparciu o zmienne "polubowne"
##
## OPIS: referencja to etap polubowny dla bezskutków:
##		BZWBK : id_bank = 60, id_paczki = 8, id_portfel = '4'
##		Dialog : id_bank = 80
##		BOŒ : id_bank = 105
##
## DOCELOWY MODEL: wysylka do s¹du dla bezskutków,
##		- scoring na skutecznoœæ wplatow¹ w 12m po wysy³ce do s¹du
##    W trakcie analizy okazalo siê ze mamy wystarczajaca ilosc danych o wplatach jedynie na 10mo po s¹dzie.
##
########################################################################################################################
########################################################################################################################
ID = "150427"   ## takie samo jak nazwa katalogu ##  w przybli¿eniu data powstania;
DATEmodel = as.POSIXct("2015-04-27") ## data modelu
ENTITY = ""
##
IDdataref = "150427"						 ## katalog z danymi referencyjnymi na których by³ zudowany model
IDref = "1"
Ysuffix = "wplaty_sad12mo_kostka"      		 ## przyrostek nazwy tabeli ze zmienn¹ objaœnian¹ ## TYLKO JEDEN W CA£YM PLIKU !!!  inny Y = inny model = inny plik i katalog modelu
##
METHOD = "score"
prediction.name = "score"            ## np: "score" w przypadku scoringu ; "prognoza"  dla innych metod
##
##  BD :  toudi.modele.
##
##	 Utworzono zmienne nazw tabel:
##	               TABLE                                                  
##	TABLEdataref   "modele.referencja_150427"                      
##	TABLEdataref_Y "modele.referencja_150427_wplaty_sad12mo_kostka"
##	TABLEmodel     "modele.referencja_150427_model_150427"         
##	
##	 Utworzono zmienne nazw plików:
##	                      FILE                              
##	FILEdataref.Rdata     "referencja_150427_1.Rdata"
##	FILEmodel_Y           "model_150427_Y.sql"              
##	FILEmodel.environment "model_150427_environment.Rdata"  
##	FILEmodel.Rdata       "model_150427.Rdata"              
##	FILEmodel.sql         "model_150427.sql"                
##	FILEmodel.xls         "model_150427.xls"                
##	FILEmodel.csv         "model_150427.csv"                      
##	
##	 Utworzono zmienne nazw katalogów:
##	            PATH                                                     
##	PATHdataref "//10.1.1.5/PK_Analitycy/Modele/Dane Referencyjne/150427"
##	PATHmodel   "//10.1.1.5/PK_Analitycy/Modele/Modele/150427"           
##	PATHwd      "//10.1.1.5/PK_Analitycy/Modele/Modele/150427"   
##
##
########################################################################################################################•
########################################################################################################################
## 0. Do poprawnego dzialania TinnR (powinno ladowac siê samo - trzeba naprawic jakies .ini)
#  .trPaths <- paste(paste(Sys.getenv('APPDATA'), '\\Tinn-R\\tmp\\', sep=''),
#            c('', 'search.txt', 'objects.txt', 'file.r', 'selection.r', 'block.r', 'lines.r'), sep='')
########################################################################################################################


########################################################################################################################
#! I. KATALOGI, ZMIENNE, PAKIETY, SKRYPTY, £ADOWANIE DANYCH i MODELI ###################################################
########################################################################################################################


########################################################################################################################
## 1. KATALOGI I PLIKI #################################################################################################


## KATALOGI ############################################################################################################
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


## PLIKI ###############################################################################################################

fun.names.model()  ## tworzy zmienne nazw plików, katalogów, tabel
  ls()

  ## Gdyby trzeba by³o przerwaæ prace wróc do tego miejsca i zapisz œrodowisko:
  #!	save.image(file=FILEmodel.environment)           ## NIE NADPISZ BEZMYŒLNIE!!!
  ## lub za³aduj œrodowisko gdy wznawiasz prace
    load(file=FILEmodel.environment) ; detach("package:TinnRcom", unload=TRUE) ; library(TinnRcom) ## unloadNamespace("TinnRcom")  may work too

  ## tylko modele (i dane)
    load(file=FILEmodel.Rdata)

########################################################################################################################


## c. ŒRODOWISKO #######################################################################################################
## Na wypadek gdyby trzeba by³o przerwaæ / wznowiæ pracê przed ukoñczeniem pliku.

        #!###########################################################################
        ## Gdyby trzeba bylo przerwac prace wróc do tego miejsca i zapisz œrodowisko:
        #!   save.image(file=FILEdataref0.environment )				## NIE NADPISZ BEZMYŒLNIE!!!
        ## lub zaladuj srodowisko gdy wznawiasz prace
          load(file=FILEdataref0.environment) ; detach("package:TinnRcom", unload=TRUE) ; library(TinnRcom) ## unloadNamespace("TinnRcom")  may work too
        #!###########################################################################

########################################################################################################################

########################################################################################################################•
#! 2. UWAGI WSTEPNE ####################################################################################################
########################################################################################################################
##   A. PODSTAWOWYM ZADANIEM niniejszego pliku jest zbudowanie MODELU na danych z pliku  FILEdataref.Rdata (ale NIE  FILEdataref.environment !!!)
##    Dane FILEdataref.Rdata s¹ w 100% gotowe do analizy statystycznej: badania korelacji, wykluczania zmiennych, budowy modelu.
##    1. pobranie danych  Data.ref , Data.pay  z  FILEdataref.Rdata
##    2. badanie korelacji
##    3. wykluczanie zmiennych
##    4. ...
##    5. stabilny model (byc moze wiele wersji)##
########################################################################################################################
##   B.
########################################################################################################################
##   C.
##    1. Niektore komendy sa zakomentowane jako potencjalnie niebezpieczne.
##       Wywolujemy je stawiajac kursor po znaku komentarza i klikajac "play" na pasku.
##       Czynimy to po upewnieniu sie, ze uruchomienie danej komendy
##       jest dokladnie tym czego wlasnie chcemy.
##    2. Komendy niebezpieczne to te, ktore uruchomione nieprawidlowo beda trudne do korekty
##       i konieczne bedzie przeprowadzenie od nowa niemal calej analizy.
##    3. Surowe dane z bazy wystepuja pod nazwa 'data.raw'.
##       W trakcie analizy pracujemy na ich kopii pod nazwa 'Data.all' dzielonych dalej na
##       'Data.ref' i 'Data.wyc'.
##       Dzieki temu, nawet w przypadku fatalnej pomylki nie bedzie konieczne ponowne
##       ladowanie danych z bazy i wystarczy cofnac sie do punktu:
##          Data.all = data.raw[,-rm.ones]    (calosc danych bez zmiennych z jedna wartoscia na REF.)
##    4. Dane dotyczace wplat, tj. Data.MOref nie podlegaja obrobce
##       (z wyjatkiem dodawania zmiennych grupujacych, ktore latwo usunac)
##       zatem nie ma koniecznosci tworzenia ich kopii.
##    5.
##
########################################################################################################################




########################################################################################################################
#! 2. UWAGI WSTEPNE ####################################################################################################

##
########################################################################################################################
##
########################################################################################################################

## 3. PAKIETY I SKRYPTY ################################################################################################

## PAKIETY
packages = c( "RPostgreSQL","RODBC",#"vioplot", #"tree",
              "randomForest","car",
              "ROCR","RODBC","igraph",
              "gam","rattle","igraph",
              "stats","graphics","grDevices",
              "datasets","methods","base",
              "caret","gdata","sqldf"
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


########################################################################################################################
## 4. POBRANIE DANYCH ##################################################################################################

##############################################################
## DANE REFERENCYJNE

## pierwsze ³adowanie danych referencyjnych z	DOMAIN/Dane Referencyjne/YYMMDD

	## POBRANIE z .Rdata (jesli bylo wczesniej zapisane)  ## szybsze
	load(file = paste(PATHdataref,FILEdataref.Rdata,sep="/") )
	## lub z .csv
	# Data.ref = read.csv(paste0(FILEdataref0,".csv"),sep=";",dec=",")

	##################################################################
	## POPRAWKI do danych z BD  ## oby jak najmniej!
	#  ch <- odbcConnect("PostgreSQL35W", uid = "akasprzyk", pwd = "akasprzyk") ## IKTORN
	#  ch <- odbcConnect("PostgreSQL35W-T", uid = "arkadiusz.kasprzyk", pwd = "akasprzyk123") ## TOUDI
	#  ...

		dim(Data.ref)   ## 2348  100


##############################################################
## DANE REFERENCYJNE Y ## zazwyczaj wp³aty w zadanym okresie

## pierwsze ³adowanie danych referencyjnych z	DOMAIN/Dane Referencyjne/YYMMDD
   ch <- odbcConnect("PostgreSQL35W-T", uid = "arkadiusz.kasprzyk", pwd = "akasprzyk123") ## TOUDI

	( query = paste("SELECT * FROM ", TABLEdataref_Y , sep = " ") )

   data.ref_Y <- sqlQuery(ch, query,  errors = TRUE)

     dim(data.ref_Y)    ##  30792     4


  #  dpd_k <- sqlQuery(ch, paste0("SELECT id_produkt , dpd_k from akasprzyk.",TABELA))
  #  dpd_sad <- sqlQuery(ch, paste0("SELECT id_produkt , dpd_sad from akasprzyk.",TABELA))
  #  dpd_kom <- sqlQuery(ch, paste0("SELECT id_produkt , dpd_kom from akasprzyk.",TABELA))
  #
  odbcClose(ch)

########################################################################################################################
## 5. £¥CZENIE DANYCH : [ X , Y ] ######################################################################################
##
## Zanim po³¹czymy X z Y sprawdzamy czy wszystko w porzadku z Y (X jest ju¿ opracowany na etapie "Dane Referencyjne").
##
	nulls.table(Data.ref)
	nulls.table(data.ref_Y)

	## NIE USUWAMY NULLi z   data.ref_Y   bowiem oznaczaj¹ one ¿e sprawa nie byla tak dlugo w obsludze po s¹dzie
	##	data.ref_Y[ is.na(data.ref_Y[,"suma_wplat_mo"]), "suma_wplat_mo" ] = 0

	table(data.ref_Y[,"suma_wplat_mo"] == 0,useNA = "ifany")
	#	FALSE  TRUE  <NA>
	#	 6491 21021  3280

	#########################################
	## zmienna nazwowa ID -- pozosta³e zmienne nazwowe w p.7.

ID_variable = "id_produkt_k"

	#########################################

   rownames(Data.ref) = Data.ref[,ID_variable]
	rownames(Data.ref)

   length(unique(data.ref_Y[,ID_variable]))   ## 2566  <   30792    => kostka!!!

	## jeœli TABLEdataref_Y jest kostk¹ to trzeba przekszta³ciæ do tabeli  id_produkt_k  x  Y_1,...Y_p

   data.ref_Y_kostka = data.ref_Y     ##
	head(data.ref_Y,50)

	data.ref_Y = with(  data.ref_Y_kostka ,
		{	data.ref_Y_mo = as.data.frame(tapply(suma_wplat_mo,list(id_produkt_k,mo),"sum"))
         data.ref_Y_mo = cbind(id_produkt_k = rownames(data.ref_Y_mo),data.ref_Y_mo)
      }
	)
	head(data.ref_Y,50); dim(data.ref_Y)
	nulls.table(data.ref_Y)

	data.ref_Y = with(  data.ref_Y ,
		{  for(k in 5:12){
				data.ref_Y[,paste0("suma_wplat_",k,"mo")] = rowSums(data.ref_Y[,-c(1,1+(k+1):12)],na.rm=TRUE)
			}
         data.ref_Y
      }
	)
	head(data.ref_Y,50); dim(data.ref_Y)
	nulls.table(data.ref_Y)

	#	rownames(data.ref_Y) = data.ref_Y[,ID_variable]

	grep('^\\d',names(data.ref_Y))
	names(data.ref_Y) = sub('^(mo)(\\d{1})$','\\10\\2',sub('^(\\d+)','mo\\1',names(data.ref_Y)))
	head(data.ref_Y)

   ## wspólny porz¹dek rekordów i tylko rekordy maj¹ce wspólne id dla obu tabel
   id.ref = intersect(rownames(Data.ref),rownames(data.ref_Y))
		any(!rownames(Data.ref)%in%id.ref)  ## FALSE  OK!!! => dla wszystkich rekordów z Data.ref mamy wplaty
	Data.ref = Data.ref[id.ref,]

	#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
	Data.ref_Y = data.ref_Y[ id.ref ,  ]
	#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#

	dim(Data.ref_Y) ; dim(Data.ref)     ## 2348   21  ;  2348  100

	names.ref_Y = names(Data.ref_Y)
	( names.ref_Y = names.ref_Y[names.ref_Y != ID_variable]  )  #! bez id_produkt !!!


################################################################################
## a. SZYBKI PRZEGL¥D ##########################################################

	attach(Data.ref_Y)     ## dla wygody, ALE nalezy uwazac by uzupelniac w Data.ref, tj.  Data.ref$zmienna[ index ] = ...
   ## po X zmiennych

# d = 1               ## nr serii zmiennych
## lub zamiast numery specyficzne grupy nazw
# 1.
(names.toplot_d <- names.mo <- grep('^mo',names.ref_Y,value=TRUE) )
# 2.
(names.toplot_d <- names.suma <- grep('^suma',names.ref_Y,value=TRUE) )

   rows = 3 ; cols =5  ## ile wierszy i kolumn wykresow w jednym oknie
   inches = 3           ## ile cali na jeden wykres
   windows( width = cols*inches + 1  , height = rows*inches + 1 )
   margins.my();coldef.black();par(mfrow=c(rows,cols))
   for(k in ((rows*cols*(d-1))+1):(rows*cols*d)){
      var.name = names.toplot_d[k]
      plot.variable( var.name
            ,what = c("cloud")     ## "bars", "ecdf", "hist", "density", "cvs", "hist",
            ,pch = "." , cex = 1
            ,user.window = TRUE
            #,col = referencja + 5   ## ref -> magenta, wyc -> cyan
            )
   }

   detach(Data.ref_Y)



################################################################################
## a. £¥CZENIE #################################################################

	## Jeœli wszystko OK to:

	#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
	Data.ref = merge( Data.ref , Data.ref_Y , by = "id_produkt_k" )   ## NIE POWTÓRZ !!!
	#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#

		## ju¿ nie potrzebne bo bylo wy¿ej dla Data.ref_Y
		grep('^\\d',names(Data.ref))
		names(Data.ref) = sub('^(mo)(\\d{1})$','\\10\\2',sub('^(\\d+)','mo\\1',names(Data.ref)))

	nulls.table(Data.ref)
	nulls.table(Data.ref[,names.regressors])         ## no NAs !!!

   save.image(file=FILEmodel.environment)           ## NIE NADPISZ BEZMYŒLNIE !!!

   Dataref = Data.ref  ## kopia bezpieczeñstwa

	dim(Data.ref)    ## 2348  120

names.ref = names(Data.ref)

########################################################################################################################
## 6. Zmienne nazwowe i grupy skutecznosci #############################################################################

  ls(pattern='^(D|d)ata\\.')
  ls(pattern='^names')
  ( names.saldo = grep('^saldo_',names.ref,value=TRUE) )
  names.ref_Y

  names.ref

ID_variable = "id_produkt_k"
	names.ID = c( ID_variable , "id_produkt_bank_k" , "id_dluznik_k" )  ## nie myliæ z  'names.id'

names.mo <- grep('^mo',names.ref_Y,value=TRUE)
	nulls.table(Data.ref[,names.mo])
names.suma <- grep('^suma',names.ref_Y,value=TRUE)
	nulls.table(Data.ref[,names.suma])

Payment_variable = "suma_wplat_10mo"   ## wybieramy zmienn¹ z jak najdlu¿szym horyzontem czasowym i z jak najmniejsz¹ iloœci¹ NA
Target_variable = "CzyDobra"
Debt_variable = "saldo_stop_pol_k"
##
Efficiency_variable = "skutecznosc"
EfficiencyGLM_variable = "skutecznosc_glm"
EffGroups_variable = "skutecznosc_grupy"
EffGLMGroups_variable = "skutecznosc_glm_grupy"

	names.efficiency = c(Efficiency_variable,EffGroups_variable,EfficiencyGLM_variable,EffGLMGroups_variable)

	names.target = c(Payment_variable,Target_variable,Debt_variable,names.efficiency)

## error & outlier ## tutaj nieobecne
#	ERROR_variable = "error"
#	OUTLIER_variable = "outlier"
#  names.error = c(ERROR_variable,OUTLIER_variable)

GoodBadThreshold = 1     ## wartoœæ wstêpna, mo¿e byæ poprawiona

#################################################################################
## a1. skutecznosc, CzyDobra ####################################################

#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!
	sum(Data.ref[,Debt_variable]==0)   ## oooops...
	sum(Data.ref[,Debt_variable]<10)  ## oooops...
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!

#! 1. najpierw wykluczamy zerowe salda
Data.ref = Data.ref[Data.ref[,Debt_variable]>0,]

#! 2. liczymy skutecznoœci
Data.ref[,Efficiency_variable] = Data.ref[,Payment_variable] / Data.ref[,Debt_variable]

#! 3. odrzucamy ekstremalne skutecznoœci (zaznaczaj¹c w zmiennej outlier jako TRUE)
range(Data.ref[,Efficiency_variable])

	table(Data.ref$outlier)

uplim = 1000
with( Data.ref ,
	{ xx = get(Efficiency_variable)
		plot.variable(xx[xx<uplim])
		print(sum(xx>=uplim))
	}
)

Data.ref$outlier = with( Data.ref ,
	{  xx = get(Efficiency_variable)
		outlier = outlier| xx>=uplim
	}
)
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!


		table(is.na(Data.ref[,Efficiency_variable]))   ## powinny byæ tylko FALSE    ## OK
		windows();par.my()
			plot( skutecznosc ~ skutecznosc_pol_k ,data = Data.ref[Data.ref$skutecznosc<2,],pch=".")

## kopia bezpieczeñstwa
Dataref = Data.ref  ## Data.ref = Dataref
Data.ref = Data.ref[!Data.ref$outlier,]

   save.image(file=FILEmodel.environment)           ## NIE NADPISZ BEZMYŒLNIE !!!


################################################################################
## b. grupy skuteczosci ########################################################

## 1.
	range(Data.ref[,Efficiency_variable])

( breaks.1 = monetary(maximum=1000,min.power = -2 ) )

attach( Data.ref )
	plot.variable(   Efficiency_variable
						, breaks = breaks.1
						, breaks.lines=TRUE
						, main = Efficiency_variable
						, cvs.covariate = Debt_variable
					)
detach(Data.ref)

Data.ref[,EffGroups_variable] = cut(  Data.ref[,Efficiency_variable]
												, breaks=c(breaks.1,max(Data.ref[,Efficiency_variable]))
												, include.lowest = TRUE )

 ( table.effgroups.debt = as.table(tapply(Data.ref[,Debt_variable]   ,Data.ref[,EffGroups_variable],sum)) )
 ( table.effgroups.pay	= as.table(tapply(Data.ref[,Payment_variable],Data.ref[,EffGroups_variable],sum)) )

 as.table(tapply(Data.ref[,Debt_variable],Data.ref[,EffGroups_variable],length))
 as.table(tapply(Data.ref[,Debt_variable],Data.ref[,EffGroups_variable],max))
 as.table(tapply(Data.ref[,Debt_variable],Data.ref[,EffGroups_variable],mean))

 windows(height=4,width=12); par.my(); par(mfrow=c(1,2))
 barplot(table.effgroups.debt);title("total debt within groups of efficiency");
 barplot(table.effgroups.pay);title("total payments within groups of efficiency") 

## inaczej
formula.agg.lm = as.formula(paste0("cbind(", Debt_variable  , "," , Payment_variable , ") ~ ", EffGroups_variable))
agg.lm = aggregate( formula.agg.lm
				,FUN = function(x){ y = c( sum(x), length(x), max(x), mean(x) , min(x) ) ; names(y) = c("sum","number","max","mean","min") ; y }
 				,data = Data.ref
				#,simplify = FALSE
				)
format( agg.lm  , scientific = FALSE	 , digits = 6 	 #, nsmall = 1
		)

#! !!!!!!!! WORK IT BETER !!!!!!!!!

Data.ref[,Target_variable] = as.numeric( Data.ref[,Efficiency_variable] > GoodBadThreshold )

table(Data.ref[,Target_variable])
##    0     1
## 1816   509

names.ref = names(Data.ref)

################################################################################
## a2. skutecznosc_gam, ########################################################

succ = Data.ref[,Payment_variable]
debtpay_max = pmax(Data.ref[,Debt_variable],Data.ref[,Payment_variable])
fail = debtpay_max - succ
succfail.mat = round(cbind(succ,fail)); 
rownames(succfail.mat) = rownames(Data.ref)


TargetBinomial_variable = "succfail.mat"


Data.ref[,EfficiencyGLM_variable] = Data.ref[,Payment_variable] / debtpay_max

	plot( skutecznosc_glm ~ skutecznosc_pol_k ,data = Data.ref,pch=".")

 ( breaks.2 = monetary( 1 , power.range = 2 ) )
Data.ref[,EffGLMGroups_variable] = cut( Data.ref[,EfficiencyGLM_variable] , breaks = breaks.2 , include.lowest = TRUE)

 ( table.effglmgroups.debt = as.table(tapply(Data.ref[,Debt_variable]   ,Data.ref[,EffGLMGroups_variable],sum)) )
 ( table.effglmgroups.pay	= as.table(tapply(Data.ref[,Payment_variable],Data.ref[,EffGLMGroups_variable],sum)) )

 as.table(tapply(Data.ref[,Debt_variable],Data.ref[,EffGroups_variable],length))
 as.table(tapply(Data.ref[,Debt_variable],Data.ref[,EffGroups_variable],max))
 as.table(tapply(Data.ref[,Debt_variable],Data.ref[,EffGroups_variable],mean))

 windows(height=4,width=12); par.my(); par(mfrow=c(1,2))
 barplot(table.effglmgroups.debt);title("total debt within groups of efficiency (glm)");
 barplot(table.effglmgroups.pay);title("total payments within groups of efficiency (glm)") 

## inaczej
agg.glm = aggregate(saldo_stop_pol_k~skutecznosc_glm_grupy
				,FUN = function(x){ y = c( sum(x), length(x), max(x), mean(x) , min(x) ) ; names(y) = c("sum","number","max","mean","min") ; y }
 				,data = Data.ref
				#,simplify = FALSE
				)
format( agg.glm  , scientific = FALSE	 , digits = 6 	 #, nsmall = 1
		)

names.ref = names(Data.ref)

########################################################################################################################
## 7. Grupy zmiennych ##################################################################################################

	(table.ref = nulls.table(Data.ref))
   table.ref[table.ref$`NAs/all`>0,]
   nulls.table(Data.ref[,names.regressors])

ls(pattern='^names')

	mget(ls(pattern='^names'))  ## duuu¿y wynik

head(names.ref,20)
  ## zmienne ID ## nie usuwamy (o ile nie s¹ puste)
names.finance = c(  "id_bank_k" , "id_paczka_k" , "id_bank_paczka_k"
						, "id_nazwa_finansowa_k" , "nazwa_finansowa_k" , "nazwa_biznesowa_k" , "typ_klienta_k" , "grupa_portfela"
						, "podmiot_kupujacy"
					  )

names.id = c(names.ID, names.finance)

( names.dates = names.ref[grep('^data_',names.raw)] )

## z etapu 'Dane Referencyjne'
names.poor
names.tosee
names.regressors
	with( table.ref , table.ref[name %in% names.poor,] )
	with( table.ref , table.ref[`NAs/all`>0.3,] )


names.saldo
names.ref_Y

( names.mo = grep('^mo',names.ref,value=TRUE) ) ## wplaty w kolejnych MO na komorniku

  	cat(setdiff(names.ref,names.id),sep="\"\n,\"")

	table.ref[ !( table.ref$name %in% union(names.dates,names.id)), ]

( names.fac = as.character(table.ref[table.ref$class == "factor","name"]) )
( names.null = as.character(table.ref[table.ref$`NAs/all`>0,"name"]))
( names.full = as.character(table.ref[table.ref$`NAs/all`==0,"name"]))

## zmienne nienadaj¹ce siê do u¿ycia jako zmienne objaœniaj¹ce
#	names.out = unique( c(names.id, names.dates , names.poor , names.ref_Y
#				, names.target , names.error
#				, "kat_kontakt_atmosfera_pol_k" , "saldo_start_pol_k" , "saldo_start_sad_k" ))
#
#
#	( regressors.all = c( setdiff(names.ref,names.out) , "grupa_portfela" ) )

## zmienne nadaj¹ce siê do modelu mamy z etapu "Dane referencyjne"
regressors.all = names.regressors
( table.reg.all = nulls.table(Data.ref[,regressors.all]) )



########################################################################################################################
## 8. Ustalenie poziomu GoodBadThreshold  ##############################################################################
## Objasnienie:
		## - tworzymy zmienna skutecznosc ( wplaty/saldo_pocz lub cos podobnego)
		## - tworzymy zmienna CzyDobra = 1  jesli skutecznosc >= GoodBadThreshold  ( 'good' )
		##                             = 0  jesli skutecznosc <  GoodBadThreshold  ( 'bad'  )
		##  GoodsAllRatio = #good/#all  \in  (0,1)
		#!  GoodBadThreshold powinien byc taki ze  GoodsAllRatio >= .1

		## (*). Robocze ustalenie poziomu GoodBadThreshold oraz
		#! utworzenie zmiennej Target_variable = "CzyDobra" i "skutecznosc"
		#! (zmienne do niej potrzebne nie moga miec NULLi na referencji: 'saldo_...' i 'wplaty_...').
		## Tu nastêpuje tez podzia³ Data.all na referencjê - Data.ref i  portfel wyceniany - Data.wyc,
		windows();margins.my();coldef.black()
		# GoodBadThreshold.choice(goodbadthreshold = 1)
		## Ustawia GoodBadThreshold na now¹ wartosc = goodbadthreshold;
		## tworzy zmienne w Data.all:
		##   - skutecznosc
		##   - CzyDobra
		## nastepnie dzieli Data.all na Data.ref i Data.wyc i dodaje do nich zmienna
		##   - waga  (=1)

		## a. Poszukiwanie optymalnego GoodBadThreshold jako funkcji GoodsAllRatio
		## Function threshold.tables() gives proper tables and plots (explanation in the function file header).
		## Tables may be quite big so it's better to divide variables in groups.

		(nt = nulls.table( Data.ref[,regressors.all] ))
		factors  = as.character( nt$name[  nt$class == "factor"   &   nt$unique_values>1   ] )
		numerics = as.character( nt$name[  nt$class == "numeric"  &   nt$unique_values>1   ] )
		integers = as.character( nt$name[  nt$class == "integer"  &   nt$unique_values>1   ] )

		  do.usuniecia.fac = c( "..." )
		  do.usuniecia.num = c( "..."  )
		  do.usuniecia.int = c( "..." )

		thresh.df = threshold.tables( datfram = Data.ref
		                            , goodsallratio = seq( 0, 1, 0.2 )
		                            , efficiency.var = Efficiency_variable
		                            , variables = numerics #[!numerics%in%do.usuniecia.num]
		                            , graphics = TRUE
		                            , what = NULL  ## some of "p.val.num" "p.val.fac","p.val.num","gini","auc";  NULL means 'all of them'
		                            , integer.as.factor = FALSE
		                            )

		thresh.df
		##  optymalne alpha dla factorow    GoodsAllRatio = .14
		##                  dla numerics    GoodsAllRatio = .14 , .18!
		##                                  GoodsAllRatio = .14 , .48!

#!!!!!!!!!!
## Ta funkcje wywolujemy jedynie w przypadku gdy  GoodBadThreshold = 1 daje mniej 'dobrych' niz 1/10 calosci przypadkow.
windows(); par.my()
thresh.stat = set.threshold(  variable = Data.ref[,Efficiency_variable]
                      , goodsallratio = .1                 ## porzadany poziom 'dobrych', tj. obserwacji dla ktorych variable >=  threshold
                      , old.threshold = GoodBadThreshold   ## dotychczasowa wartosc  GoodBadThreshold ## dla porownania starej i nowej wartosci
                      , graphics = TRUE
                      #, threshold.name = "GoodBadThreshold"   ## nowa wartosc GoodBadThreshold bedzie zwrocona do tej zmiennej
                    )

    #GoodBadThreshold= 0.844850153840923
    ## ciekawe tabelki
thresh.aggr.table(  variable = Data.ref[,Efficiency_variable]
                  , covariate = Data.ref[,Payment_variable]
                  , goodsallratio = seq(.1,.9,.1)
                  , FUN = sum
                 )

		                 #!!!!!!!
		## mozna tu przygladnac sie dokladniej jak GoodBadThreshold wplywa na poszczegolne zmienne
		zz = split.stats( #variable = as.factor( Data.all$czy_wplata_30[Data.all$referencja==1] )
		                  variable = Data.ref[,Payment_variable]
		                  , split = Data.ref[,Target_variable] # thresh.stat$GoodsBads
		                  , graphics = TRUE
		                  , message = TRUE
		                )
		#! only if needed
		#!   GoodBadThreshold = thresh.stat$threshold


		# number of good = 3462
		# number of bad = 28523
		# good/all reference ratio = 20.4977%

		##   bads        goods
		## 0.89176   0.10824


				##########################
				##  names( Data.all )
				##    dim( Data.wyc )
				##########################

		dim( Data.ref )   ## 31985   205

		#! Powracamy tu po usunieciu ekstremalnych skutecznosci (etap 6. ponizej).


########################################################################################################################
## 8. Eliminacja zmiennych skorelowanych  ##############################################################################

rf_binary 	= randomForest( y = Data.ref[,Target_variable], x = Data.ref[,regressors.all] , importance=TRUE)
windows();varImpPlot(rf_binary)

rf_binomial = randomForest( y = Data.ref[,EfficiencyGLM_variable], x = Data.ref[,regressors.all] , importance=TRUE)
windows();varImpPlot(rf_binomial)

rf_payments = randomForest( y = Data.ref[,Payment_variable], x = Data.ref[,regressors.all] , importance=TRUE)
windows();varImpPlot(rf_payments)


################################################################################
## a. binary

cat(sort(regressors.all), sep="\"\n,\"")

regressors.binary = c(
#"czas_polaczen_sek_razem_pol_k"
#,"czy_dekl_pol_k"
#,"czy_list_pol_k"
#,"czy_tel_pol_k"
#,"czy_ugoda_pol_k"
 "czy_wiz_pol_k"
#,"czy_wplata_pol_k"
,"dp_dekl_pol_k"
#,"dp_list_pol_k"
#,"dp_tel_pol_k"
#,"dp_ugoda_pol_k"
,"dp_wiz_pol_k"
#,"dpd_start_pol_k"
#,"dpd_stop_pol_k"
,"dpo_start_pol_k"
#,"dpw_pol_k"
#,"dt_pol_k"
#,"dt_ugoda_pol_k"
,"dtu"
,"dzien_dekl_pol_k"
,"dzien_list_pol_k"
#,"dzien_tel_pol_k"
,"dzien_ugoda_pol_k"
,"dzien_wiz_pol_k"
#,"dzien_wplata_pol_k"
,"grupa_portfela"
#,"ile_dekl_pol_k"
,"ile_kontakt_a_pol_k"
,"ile_kontakt_b_pol_k"
#,"ile_kontakt_d_pol_k"
,"ile_list_pol_k"
,"ile_pglos_pol_k"
,"ile_tel_pol_k"
,"ile_telp_pol_k"
#,"ile_telw_pol_k"
#,"ile_ugoda_pol_k"
#,"ile_ugoda_rata_pol_k"
#,"ile_wiz_pol_k"
#,"ile_wplat_pol_k"
,"kat_proces_kom_pol_k"
#,"koszt_pol_k"
#,"kwota_dekl_o_pol_k"
,"kwota_dekl_pol_k"
#,"kwota_sr_dekl_pol_k"
#,"kwota_sr_ugoda_o_rata_pol_k"
#,"kwota_ugoda_o_pol_k"
#,"kwota_ugoda_pol_k"
,"kwota_wplat_pol_k"
#,"kwota_wplata_o_pol_k"
#,"kwota_wplata_p_pol_k"
#,"prop_koszt_do_saldo_start_pol_k"
#,"prop_wplaty_do_koszt_pol_k"
#,"saldo_start_k"
#,"saldo_start_k_kapital"
,"saldo_start_k_koszty"
#,"saldo_start_k_odsetki"
#,"saldo_start_pol_k"
#,"saldo_start_ugoda_o_pol_k"
,"saldo_stop_pol_k"
,"skutecznosc_pol_k")

################################################################################
## a. binomial

cat(sort(regressors.all), sep="\"\n,\"")

regressors.binomial = c(
"czas_polaczen_sek_razem_pol_k"
#,"czy_dekl_pol_k"
#,"czy_list_pol_k"
#,"czy_tel_pol_k"
,"czy_ugoda_pol_k"
,"czy_wiz_pol_k"
#,"czy_wplata_pol_k"
,"dp_dekl_pol_k"
#,"dp_list_pol_k"
#,"dp_tel_pol_k"
#,"dp_ugoda_pol_k"
,"dp_wiz_pol_k"
#,"dpd_start_pol_k"
#,"dpd_stop_pol_k"
,"dpo_start_pol_k"
#,"dpw_pol_k"
,"dt_pol_k"
#,"dt_ugoda_pol_k"
,"dtu"
#,"dzien_dekl_pol_k"
,"dzien_list_pol_k"
#,"dzien_tel_pol_k"
#,"dzien_ugoda_pol_k"
,"dzien_wiz_pol_k"
#,"dzien_wplata_pol_k"
,"grupa_portfela"
#,"ile_dekl_pol_k"
,"ile_kontakt_a_pol_k"
,"ile_kontakt_b_pol_k"
#,"ile_kontakt_d_pol_k"
,"ile_list_pol_k"
,"ile_pglos_pol_k"
#,"ile_tel_pol_k"
,"ile_telp_pol_k"
#,"ile_telw_pol_k"
#,"ile_ugoda_pol_k"
#,"ile_ugoda_rata_pol_k"
#,"ile_wiz_pol_k"
#,"ile_wplat_pol_k"
,"kat_proces_kom_pol_k"
#,"koszt_pol_k"
#,"kwota_dekl_o_pol_k"
,"kwota_dekl_pol_k"
#,"kwota_sr_dekl_pol_k"
#,"kwota_sr_ugoda_o_rata_pol_k"
#,"kwota_ugoda_o_pol_k"
#,"kwota_ugoda_pol_k"
,"kwota_wplat_pol_k"
#,"kwota_wplata_o_pol_k"
#,"kwota_wplata_p_pol_k"
#,"prop_koszt_do_saldo_start_pol_k"
#,"prop_wplaty_do_koszt_pol_k"
#,"saldo_start_k"
#,"saldo_start_k_kapital"
#,"saldo_start_k_koszty"
#,"saldo_start_k_odsetki"
#,"saldo_start_pol_k"
#,"saldo_start_ugoda_o_pol_k"
,"saldo_stop_pol_k"
,"skutecznosc_pol_k")

################################################################################
## a. payments

regressors.payments = c()    ## ...

################################################################################
## b. Korelacje (grafy) - zmienne ci¹g³e #######################################
rho =.5 #  =.5  =.3   # wazne jest tylko r=.7
 windows()
 #! ponizsza funkcja znajduje korelacje jedynie dla zmiennych liczbowych !!!   I dobrze!
 graf(zmienne.graf = regressors.binomial, dane = Data.ref,
      r=rho, 
      zmienne.bez.null = regressors.binomial,
      target = EfficiencyGLM_variable, ver = 2,               #! bledy dla  ver=1 ## wysypuje sie gdy nie ma zadnych korelacji (powyzej zadanego r)
      metoda = list( c(1) , c("pearson", "kendall", "spearman") ) )

setdiff(regressors.binary,regressors.binomial)
setdiff(regressors.binomial,regressors.binary)

################################################################################
## c. chi^2 - zmienne kategoryczne #############################################
##
## Ponizej tesujemy hurtowo hipoteze ze zmienne kategoryczne (factors) sa niezalezne.
## Odrzucamy ja dla p.value<0.05.
## Postêpujemy tak jak dla zmiennych numerycznych tj. odrzucamy zmienne a¿ bêdzie brak 'korelacji'
##  na poczatek te ktore koreluja z duza iloscia innych
chisqs = chisq.multi.test(Data.ref[,regressors])
  ## nie przejmujemy sie ostrzezeniami
chisqs$cor.pairs ## "skorelowane" (a raczej zalezne) pary zmiennych kategorycznych
###################################

	plot.variable(Data.ref$kat_rodzaj_postepowania_sad_k)
	plot.variable(Data.ref$kat_tryb_postepowania_sad_k)


################################################################################
## d. Ranking si³y predykcyjnej zmiennych za pomoc¹ lasu losowego
## (dla zmiennych pelnych i uzupelnionych)
###################################
windows();margins.my()
kolejnosc.binary <- rank.of.variables(target = Target_variable, dane = Data.ref, a = 4/5, b = 2,
		                  variables = regressors.binary)#, Ntree = 500)

kolejnosc.binomial <- rank.of.variables(target = Target_variable, dane = Data.ref, a = 4/5, b = 2,
                  variables = regressors.binomial)#, Ntree = 500)
## Wg tej kolejnosci decydujemy któr¹ ze zmiennych skorelowanch wyrzucic a ktora zostawic.

   save.image(file=FILEmodel.environment)           ## NIE NADPISZ BEZMYŒLNIE !!!


########################################################################################################################
## 9. Uzupelnianie NULLi (dla zmiennych z akceptowalnym poziomem NULLi ) ###############################################

##  odbylo siê na etapie "Dane Referencyjne"

save.image(file=FILEmodel.environment)           ## NIE NADPISZ BEZMYŒLNIE!!!


########################################################################################################################
## 10. MODEL 1. ################################################################################################

################################################################################
## a. Zamiana zmiennych kategorycznych na zmienne typu czy_ (binarne) ##########

#! Niestety,  logistic.regression() nie dziala dobrze dla zmiennych kategorycznych dlatego nalezy je
## zamienic na zmienne numeryczne:

( table.binary = nulls.table(Data.ref[,regressors.binary]) )

( regressors_fac.binary = as.character(table.binary[table.binary$class=="factor","name"]) )

regressors.binary = regressors.binary[!regressors.binary %in% regressors_fac.binary]

################################################################################
## for( k in ... ){

k = 2 ; ( name.fac = regressors_fac.binary[k] )


fac_k = Data.ref[,name.fac]

	unique(fac_k)
		fac_k

	levels(fac_k)

#######################################
## tylko dla zmiennej  "kat_proces_kom_pol_k"
	## rêcznie !!!
	## UWAGA: poprawiæ  "mulidluznik"	na  "multidluznik"
	levels(fac_k) = c("emeryt" , "multidluznik" ,"niewspolpracujacy" , "wspolpracujacy" , "z niska zaliczka" )
#######################################

		head(fac_k,100)

	lev_k = c("intercept" ## dolaczamy srednia
				,levels(fac_k) )
	fac_k = factor( as.character(fac_k) , levels = lev_k , ordered = TRUE )

		head(fac_k,100)

	contrasts(fac_k) = "contr.treatment"
   df_k = contrasts(fac_k)[as.character(fac_k),] ;  head(df_k,30) ;	unique(fac_k)

	( new_col_names = gsub(" ","_",paste0("czy ", colnames(df_k) )) ) ;

colnames(df_k) = new_col_names
	head( df_k )
		# new_col_names[new_col_names %in% names.mod]

Data.ref = cbind(Data.ref,df_k)
# Data.ref[,name.fac] = NULL ## nie wyrzucamy oryginalnej zmiennej
	tail(names(Data.ref),10)

#######################################
## tylko dla zmiennej  'grupa_portfela'
## ogólniej: tylko dla zmiennych maj¹cych tylko 2 poziomy
   new_col_names = "czy_telekom"
	Data.ref[,new_col_names] = ifelse(Data.ref[,new_col_names] %in% c("bank","bank detal","hipoteka","MŒP"),0,1)
	table(Data.ref[,new_col_names])
		##	   0    1
		##	1462  883
	sum(is.na(Data.ref[,new_col_names]) 	## 0
#######################################


regressors.binary = c( regressors.binary , new_col_names )
## }

	grep('^czy_',names(Data.ref),value=TRUE)
	# regressors.binary = c( regressors.binary , "czy_telekom" , "czy_emeryt" ,"czy_multidluznik" ,"czy_niewspolpracujacy" , "czy_wspolpracujacy" , "czy_z_niska_zaliczka" )

################################################################################
table(Data.ref$kat_proces_kom_pol_k)


save.image(file=FILEmodel.environment)				## NIE NADPISZ BEZMYŒLNIE!!!

nulls.table(Data.ref[,regressors.binary])

################################################################################
## b. PODZIA£ NA ZBIÓR UCZ¥CY I TESTOWY ########################################

LearnTstList<-build.random.sets(Target_variable, Data.ref, 4/5, 10) #trzeci argument = 4/5 oznacza, ze zbiór ucz¹cy stanowi 4/5 zbioru wylosowanego, 1/5 testowy
                                                    #A = {GOOD: GOOD in DaneRef} 
                                                    #B = {sample{DaneRef\A}: |B|=balans * |A|} <-tu zbilansowanie              
                                                    #balans: czwarty argument = stosunek #BAD/#GOOD, maksymalnie (nb/ng)
Data.trn <- data.frame(LearnTstList[[1]])
Data.tst <- data.frame(LearnTstList[[2]])

sum(Data.ref[Payment_variable])/sum(Data.ref[Debt_variable])     ## 0.3959782
sum(Data.trn[Payment_variable])/sum(Data.trn[Debt_variable])     ## 0.3956082
sum(Data.tst[Payment_variable])/sum(Data.tst[Debt_variable])     ## 0.4022166

    str(Data.trn[, regressors.binary])

################################################################################
## c. Selekcja zmiennych wg. istotnoœci ########################################

kolejnosc.binary <- rank.of.variables(target = Target_variable, dane = Data.ref, a = 4/5, b = 2,
		                  variables = regressors.binary)#, Ntree = 500)

cat( sort(kolejnosc.binary[[1]]), sep = "\"\n,\"" )

regressors.binary.final = c(
 "czy_emeryt"
,"czy_multidluznik"
,"czy_niewspolpracujacy"        # 5
,"czy_telekom"
#,"czy_wiz_pol_k"             # 1
,"czy_wspolpracujacy"
#,"czy_z_niska_zaliczka"      # 0
,"dp_dekl_pol_k"
#,"dp_wiz_pol_k"              # 1
,"dpo_start_pol_k"              # 6
,"dtu"                          # 4
#,"dzien_dekl_pol_k"          # 1
#,"dzien_list_pol_k"          # 1
#,"dzien_ugoda_pol_k"         # 1
#,"dzien_wiz_pol_k"           # 1
#,"ile_kontakt_a_pol_k"       # 1
,"ile_kontakt_b_pol_k"
,"ile_list_pol_k"
,"ile_pglos_pol_k"
#,"ile_tel_pol_k"
#,"ile_telp_pol_k"            # 3
#,"kwota_dekl_pol_k"          # 1
,"kwota_wplat_pol_k"
#,"saldo_start_k_koszty"      # 2
,"saldo_stop_pol_k"
,"skutecznosc_pol_k"
)

length(regressors.binary.final);
	# regressors.binary.final[ ! regressors.binary.final %in% row.names(summary(model.score_0)$param) ]

#!#!#!#!    summary.binary.list = list()   ## nowa lista z tabelami modeli ## uwazaj by nie usunac!!!!!
k = 7
summary.all = data.frame("lp" = 1:(length(regressors.binary.final)+2)); rownames(summary.all) = c(regressors.binary.final,"MSE","AIC");

#! wykonujemy kilkakrotnie sprawdzajac stabilnosc wspolczynnnikow
## za kazdym razem dodaja sie kolejne kolumny informacyjne do df 'summary.all'
## W zasadzie to jest to samo co w petli wykonuje wielokrotna_budowa_modelu_GAM() ## ktora jednak zwraca wynik w malo czytelnej formie
##    
graphics.off()
model.score_0 = logistic.regression(
                #training.set <- Data.trn,  test.set = Data.tst,
					 predicted.set = Data.ref,
                target.variable= Target_variable, explanatory.variables = regressors.binary.final,
                dir="none",plots=TRUE, Nonparametric=TRUE, 
                random = list(TRUE,3/5,10), rys.test.ind = 1
                )            
#! patrzymy na zmienne które maj¹ niewlasciwe znaki z punktu widzenia biznesowego (tj. ekspercko) #################################################################################################   
summary.all = cbind( summary.all, summary.ak.gam(model.score_0) )
summary.all           #! 
  # summary.all[,ncol(summary.all)] = NULL # x2  ## usuwamy modele zdegenerowane

# to co dalej jest po tym jak juz wylosuje wystarczajaca liczbe modeli
 
## zapisujemy wyniki dla kazdego zestawu zmiennych do listy
summary.binary.list[[k]] = summary.all
names(summary.binary.list)[k] = paste(k, mean(as.numeric(summary.binary.list[[k]]["AIC",][(1:3)*2])),sep=", mean(AIC) = ")
summary.binary.list

save.image(file=FILEmodel.environment)				## NIE NADPISZ BEZMYŒLNIE!!!

##############
## UWAGA: Model 10.jest równie¿ dobry, niskie AIC jednak fikaj¹ce znaki przy 2 zmiennych
##############

##################################
for(i in 1:length(summary.binary.list)) {
  names(summary.binary.list)[i] = paste(i, mean(as.numeric(summary.binary.list[[i]]["AIC",][(1:3)*2])),sep=", mean(AIC) = ")
}

################################################################################
## d. Ranking si³y predykcyjnej zmiennych za pomoc¹ lasu losowego ##############
## (dla zmiennych które wesz³y do modelu) ######################################
##
windows();margins.my()
kolejnosc.binary.final <- rank.of.variables(target = Target_variable, dane = Data.ref, a = 4/5, b = 2,
                  variables = regressors.binary.final)#, Ntree = 500)


#!##################################################################################################

						Data.wyc = Data.ref[sample(nrow(Data.ref),100),]    ## bez tego nie dzia³aj¹ poni¿sze skrypty...
                                                                              
                                                                              #wykresy GAM
                                                                              windows();margins.my()
                                                                              rysuj_predict_GAM( model=model.score_0, predicted.set=rbind(Data.wyc,Data.trn) )
                                                                                   #!!! BLAD !!!    plot.new() : figure margins too large
                                                                              
                                                                              #! modele bez s() ## pomijamy tu bowiem uwzglednilismy wnioski z p. 6.
                                                                              
                                                                              ## Powtarzamy  a. b.  i  c.  az dojedziemy do zadowalajacych rezultatow. ########################################################
                                                                              
                                                                              
                                                                              ## d. Wstepne szacowanie skutecznosci na Wycenianym wg. podzialu na grupy scorow ##########################################
                                                                              preliminary.SR(Data.wyc,Data.ref,model.score_0,Payment_variable,Debt_variable,1,FALSE)
                                                                              #! 0.1272081
                                                                              
                                                                              ## ????????
                                                                              #lynx.SR.2(data.all.and.group = Data.all, model = model.score_0 ,  nr_of_groups = 10,group.variable = c("CzyPKDWPonad20000"),  min.grupa = 50)  ##nr_of_groups = 0 <- automatyczny dobór zmiennych
                                                                              
                                                                              graphics.off()

			    graphics.off()
			    wielokrotna_budowa_modelu_GAM(predicted.set = Data.ref, bad.goods = c(rep(2,a <- 50)), K=c(rep(4/5, 50)),
			        direction = "both",
			        explanatory.variables = regressors.final,
			        los = TRUE, plot.pdf = TRUE )
 

########################################################################################################################
########################################################################################################################
## 11. MODEL 2. ## Wielokrotna budowa modelu GAM #######################################################################
##     Eliminacja zmiennych niestabilnych z modelu.
#! to trzeba jakos udoskonalic wizualnie

## wielokrotna budowa modelu GAM
cat(regressors.binary.final, sep = "\", \n \"")    ## lub  (powinno byc to samo)
cat(names(model.score_0$coefficients), sep = "\", \n \"")   ## zmienne z regresji wstêpnej


Data.wyc = Data.ref[ sample(1:nrow(Data.ref),1000) , ]     ## BO NIE DZIALA BEZ TEGO
  
  
  Stats_file = paste("model",ID,"wielokrotna_budowa_modelu_GAM",sep="_")
                 
    #!!! Poprawic zeby bylo czytelne - zastosowac summary.ak.gam() 
    graphics.off()
    wielokrotna_budowa_modelu_GAM(predicted.set = Data.ref, bad.goods = c(rep(2,a <- 50)), K=c(rep(4/5, 50)), 
        direction = "none",
        explanatory.variables = regressors.binary.final,
        los = TRUE,
		  plot.pdf = TRUE ) 
        ## .txt i inne pliki w zlym katalogu....
        
        ## MODEL STABILNY I ....!!!!!!!!!!!!!!!!!!
        
    
        
########################################################################################################################
########################################################################################################################
## 14. MODEL 3 ## Model GLM / GAM finalny ##############################################################################
##    

## a. Model wlasciwy

## Jeœli losujemy w ciele funkcji logistic.regression uzywaj¹c random = list(TRUE,...) 
## to ponizsze losowanie jest zbêdne

      #LearnTstList<-build.random.sets(Target_variable, Data.ref, 4/5, 10)
      LearnTstList<-build.random.sets( Target_variable , Data.ref , proporcja_nbLrn_do_nbTst = 4/5 , balans = 9 )            
      Data.trn <- LearnTstList[[1]]
      Data.tst <- LearnTstList[[2]]
      sum(Data.ref[Payment_variable])/sum(Data.ref[Debt_variable])     ## 0.0855271
      sum(Data.trn[Payment_variable])/sum(Data.trn[Debt_variable])     ## 0.08694343



########################################################################################################################
## 00 ##################################################################################################################

	unique( Data.ref[,"grupa_portfela"] )

	grouping = Data.ref[ , "grupa_portfela" ]
	cols = as.numeric(grouping) + 1
		coldf =  data.frame( grupa_portfela = grouping , kolor = palette()[cols] , kolor_nr = cols , ilosc = rep( 1 , length(grouping) ) )
	   print(aggregate( ilosc ~ . , data = coldf, sum) )



	plot.variable(Data.ref[,Payment_variable], col = cols)
	plot.variable(Data.ref[,Payment_variable], col = cols , xlim = c(0,5000))  ## g³adko!!!!!
	plot.variable(Data.ref[,Efficiency_variable], col = cols)

	with( Data.ref , {
				pv = get(Payment_variable)
				print( breaks <- monetary(maximum = max(pv),power.range = 2) )
			  	plot.variable( pv[pv>0]
				  				, col = cols[pv>0]
								, breaks = breaks  )
		}
	)


########################################################################################################################
## 01 ##################################################################################################################
## glm_logit_binary_1
## score binarny ("w³aœciwy") ## regresja logistyczna na zmiennej CzyDobra
##
##	k = 1

## MODEL
formula.glm_logit_binary_1 = as.formula(paste0(Target_variable,"~",paste(regressors.binary.final,collapse="+")))

model.glm_logit_binary_1 = glm( formula.glm_logit_binary_1 , family=binomial(link=logit), data=Data.ref )
 summary(model.glm_logit_binary_1)

pred.glm_logit_binary_1		= predict( model.glm_logit_binary_1,type="response" )
	plot.variable(pred.glm_logit_binary_1, col = cols )


	#######################################################
	## utworzenie tabeli z wynikami prognoz
  	##############################
	## bardzo wa¿ne i tylko raz!!!
	##
		model.df	= data.frame( id_produkt_k = Data.ref[,ID_variable] , glm_logit_binary_1 = pred.glm_logit_binary_1)
		row.names(model.df) = model.df[,ID_variable] ; model.df[,ID_variable] = NULL
	##
	##############################

	head(model.df)

#######################################################
## odzyski faktyczne a przewidywane ze score'a binary

pred.glm_logit_binary_wplaty = pred.glm_logit_binary_1 * Data.ref[,Debt_variable]
model.df[,"glm_logit_binary_wplaty"] = pred.glm_logit_binary_wplaty
	head(model.df)
	plot.variable(pred.glm_logit_binary_wplaty, col = cols )

  windows(); par.my()
  plot( Data.ref[,Payment_variable] ~ pred.glm_logit_binary_wplaty , pch='.' , col = cols )
	abline()
	#  abline(h=0,col="white")
	#  legend( "topleft" , legend = unique(Data.ref$grupa_portfela) , col = unique(as.numeric(Data.ref$grupa_portfela)) + 1, pch = 20 )



########################################################################################################################
## UWAGA: inne modele robimy tymczasowo dla zmiennych   regressors.binary.final
## co nie jest do koñca wlaœciwe.
## Dla wlaœciwych zestawów zmiennych zrobimy póŸniej.

########################################################################################################################
## 02 ##################################################################################################################
## glm_logit_binomial_1
## succfail  [succeses-failures]
##
## k = 2

## to ju¿ bylo ale powtórzymy
succ = Data.ref[,Payment_variable]
debtpay_max = pmax(Data.ref[,Debt_variable],Data.ref[,Payment_variable])
fail = debtpay_max - succ
succfail.mat = round(cbind(succ,fail));
rownames(succfail.mat) = rownames(Data.ref)

TargetBinomial_variable = "succfail.mat"

Data.ref[,EfficiencyGLM_variable] = Data.ref[,Payment_variable] / debtpay_max


	head(succfail.mat,20)

	class(succfail.mat)

	succfail.list = as.list(as.data.frame(t(succfail.mat)))
	succfail.list = lapply(succfail.list,function(x){names(x) = c("succ","fail"); x})
	   any(names(succfail.list) != rownames(Data.ref))  ## FALSE, OK!!!

      length(succfail.list)     ## 2345

				Data.ref.xxx = Data.ref
				Data.ref.xxx$succfail.mat =  succfail.list
				head(Data.ref.xxx)
			   Data.ref.xxx[1:10,"succfail.mat"]


## MODEL
formula.glm_logit_binomial_1 = as.formula(paste( "succfail.mat", paste(regressors.binary.final,collapse="+") ,sep="~"))

model.glm_logit_binomial_1 = glm( formula.glm_logit_binomial_1 , data = Data.ref , family = binomial( link=logit ) )
  summary(model.glm_logit_binomial_1)
  
pred.glm_logit_binomial_1 = predict(model.glm_logit_binomial_1,type="response")
	plot.variable(pred.glm_logit_binomial_1, col = cols )


#######################################################
## odzyski faktyczne a przewidywane ze score'a binomial

pred.glm_logit_binomial_wplaty = pred.glm_logit_binomial_1 * Data.ref[,Debt_variable]
	plot.variable(pred.glm_logit_binomial_wplaty, col = cols )

  windows(); par.my()
  plot( Data.ref[,Payment_variable] ~ pred.glm_logit_binomial_wplaty , pch='.' , col = cols )
	#  abline(h=0,col="white")
	#  legend( "topleft" , legend = unique(Data.ref$grupa_portfela) , col = unique(as.numeric(Data.ref$grupa_portfela)) + 1, pch = 20 )


#######################################################
## zapis do tabeli jeœli warto
model.df[,"glm_logit_binomial_1"] = pred.glm_logit_binomial_1
model.df[,"glm_logit_binomial_wplaty"] = pred.glm_logit_binomial_wplaty
	head(model.df)


########################################################################################################################
## 01 vs 02 ############################################################################################################
## binarny vs binomial
##

	sum(pred.glm_logit_binary_wplaty)
	sum(pred.glm_logit_binomial_wplaty)
	sum(Data.ref[,Payment_variable])

  windows(height = 6, width = 10 ); par.my(); par(mfrow = c(1,2))
  plot( Data.ref[,Payment_variable] ~ pred.glm_logit_binary_wplaty , pch='.' , col = cols , ylab = Payment_variable )
  	title("wp³aty rzeczywiste vs przewidywane binary")
	lm.binary = lm(Data.ref[,Payment_variable] ~ pred.glm_logit_binary_wplaty)
	abline(lm.binary,col="white");
	text(	  x = max(pred.glm_logit_binary_wplaty)/2 , y = max(Data.ref[,Payment_variable])*.9
			, labels = paste0( "y = " , paste( paste0( round( lm.binary$coef, 3 ) , c(""," * x") ), collapse = " + ") )
		 )
  plot( Data.ref[,Payment_variable] ~ pred.glm_logit_binomial_wplaty , pch='.' , col = cols , ylab = Payment_variable )
	title("wp³aty rzeczywiste vs przewidywane binomial")
	lm.binomial = lm(Data.ref[,Payment_variable] ~ pred.glm_logit_binomial_wplaty)
	abline(lm.binomial,col="white")
	text(	  x = max(pred.glm_logit_binomial_wplaty)/2 , y = max(Data.ref[,Payment_variable])*.9
			, labels = paste0( "y = " , paste( paste0( round( lm.binomial$coef, 3 ) , c(""," * x") ), collapse = " + ") )
		 )


#######################################################
## wp³aty przewidywane logit-binary ~ zad³u¿enie
  windows(height = 5, width = 5 ); par.my()
  plot( pred.glm_logit_binary_wplaty ~ Data.ref[,Debt_variable] , pch='.' , col = cols , xlab = Debt_variable )
  	title("wp³aty przewidywane logit-binary vs zad³u¿enie")
	abline(c(0,1))

#######################################################
## wp³aty przewidywane logit-binomial vs zad³u¿enie
  windows(height = 5, width = 5 ); par.my()
  plot( pred.glm_logit_binomial_wplaty ~ Data.ref[,Debt_variable] , pch='.' , col = cols , xlab = Debt_variable )
  	title("wp³aty przewidywane logit-binomial vs zad³u¿enie")
	abline(c(0,1))


########################################################################################################################
## TUTAJ KONIEC MODELI -- dalsza czêsæ pliku do przetworzenia;
##  zapis danych w pkcie 15.
########################################################################################################################






########################################################################################################################
## 03 ##################################################################################################################
## glm_cloglog_binomial_1
## succfail  [succeses-failures]
##
## k = 2

Payment_variable

succ = Data.ref[,Payment_variable]
fail = pmax(Data.ref[,Debt_variable],Data.ref[,Payment_variable]) - succ
succfail.mat = round(cbind(succ,fail)); row.names(succfail.mat) = rownames(Data.ref)
	head(succfail.mat,20)


## MODEL
formula.glm_cloglog_binomial_1 = as.formula(paste( "succfail.mat", paste(regressors.final,collapse="+") ,sep="~"))

model.glm_cloglog_binomial_1 = glm( formula.glm_cloglog_binomial_1 , data = Data.ref , family = binomial( cloglog ) )
  summary(model.glm_cloglog_binomial_1)

pred.glm_cloglog_binomial_1 = predict(model.glm_cloglog_binomial_1,type="response")
	plot.variable(pred.glm_cloglog_binomial_1, col = cols )
  windows( height = 6, width = 6 ); par.my()
  plot( pred.glm_logit_binomial_1 ~ pred.glm_cloglog_binomial_1 , pch='.' , col = cols )


#######################################################
## odzyski faktyczne a przewidywane ze score'a cloglog binomial

pred.glm_cloglog_binomial_1_wplaty = pred.glm_cloglog_binomial_1 * Data.ref[,Debt_variable]

	sum(pred.glm_cloglog_binomial_1_wplaty)
	sum(Data.ref[,Payment_variable])

  windows(); par.my()
  plot( Data.ref[,Payment_variable] ~ pred.glm_cloglog_binomial_1_wplaty , pch='.' , col = cols )
	#  abline(h=0,col="white")
	#  legend( "topleft" , legend = unique(Data.ref$grupa_portfela) , col = unique(as.numeric(Data.ref$grupa_portfela)) + 1, pch = 20 )

#######################################################
## wp³aty przewidywane logit-binomial vs zad³u¿enie
  windows(height = 5, width = 5 ); par.my()
  plot( pred.glm_cloglog_binomial_1_wplaty ~ Data.ref[,Debt_variable] , pch='.' , col = cols , xlab = Debt_variable )
  	title("wp³aty przewidywane logit-binomial vs zad³u¿enie")
	abline(c(0,1))



#######################################################
## zapis do tabeli jeœli warto

	model.df[,"glm_cloglog_binomial_1"] = pred.glm_cloglog_binomial_1
	model.df[,"glm_cloglog_binomial_1_wplaty"] = pred.glm_cloglog_binomial_1_wplaty
		head(model.df)

########################################################################################################################
## 03 ##################################################################################################################
## glm_log_poisson_1
## wp³aty
##
## k = 3

Payment_variable

## MODEL
formula.glm_log_poisson_1 = as.formula(paste( Payment_variable, paste(regressors.final,collapse="+") ,sep="~"))

model.glm_log_poisson_1 = glm( formula.glm_log_poisson_1 , data = Data.ref , family = poisson( link=log ) )
  summary(model.glm_log_poisson_1)

pred.glm_log_poisson_1 = predict(model.glm_log_poisson_1,type="response")
	plot.variable(pred.glm_log_poisson_1, col = cols )

#######################################################
## odzyski faktyczne a przewidywane z poisson( log )

	sum(pred.glm_log_poisson_1)
	sum(Data.ref[,Payment_variable])

  windows(height = 6, width = 5 ); par.my()
  plot( Data.ref[,Payment_variable] ~ pred.glm_log_poisson_1 , pch='.' , col = cols , ylab = Payment_variable )
  	title("wp³aty rzeczywiste vs przewidywane poisson")
	lm.pois = lm(Data.ref[,Payment_variable] ~ pred.glm_log_poisson_1)
	abline(lm.pois,col="white");
	text(	  x = max(pred.glm_log_poisson_1)/2 , y = max(Data.ref[,Payment_variable])*.9
			, labels = paste0( "y = " , paste( paste0( round( lm.pois$coef, 3 ) , c(""," * x") ), collapse = " + ") )
		 )
	#  abline(h=0,col="white")
	#  legend( "topleft" , legend = unique(Data.ref$grupa_portfela) , col = unique(as.numeric(Data.ref$grupa_portfela)) + 1, pch = 20 )

#######################################################
## wp³aty przewidywane log-poisson vs zad³u¿enie
  windows(height = 5, width = 5 ); par.my()
  plot( pred.glm_log_poisson_1 ~ Data.ref[,Debt_variable] , pch='.' , col = cols , xlab = Debt_variable )
  	title("wp³aty przewidywane log-poisson vs zad³u¿enie")
	abline(c(0,1))
	#!!!!!!!!!!!!!!!


#######################################################
## zapis do tabeli jeœli warto

	model.df[,"glm_log_poisson_1"] = pred.glm_log_poisson_1
		head(model.df)


########################################################################################################################
## 04 ##################################################################################################################
## glm_log_quasipoisson_1
## wp³aty
##
## k = 4

Payment_variable

## MODEL
formula.glm_log_quasipoisson_1 = as.formula(paste( Payment_variable, paste(regressors.final,collapse="+") ,sep="~"))

model.glm_log_quasipoisson_1 = glm( formula.glm_log_quasipoisson_1 , data = Data.ref , family = quasipoisson( link=log ) )
  summary(model.glm_log_quasipoisson_1)
  summary(model.glm_log_poisson_1)

pred.glm_log_quasipoisson_1 = predict(model.glm_log_quasipoisson_1,type="response")
	plot.variable(pred.glm_log_quasipoisson_1, col = cols )
  windows(); par.my()
  plot( pred.glm_log_poisson_1 ~ pred.glm_log_quasipoisson_1 , pch='.' , col = cols , ylab = Payment_variable )

## prognoza dla quasipoisson()  jest identyczna, zmienia siê jedynie estymator b³êdu ##
model.df[,"glm_log_quasipoisson_1"] = pred.glm_log_quasipoisson_1
	head(model.df)

########################################################################################################################
## 05 ##################################################################################################################
## glm_ident_quasi_1
## wp³aty
##
## k = 3

Payment_variable

## MODEL
formula.glm_quasi_1 = as.formula(paste( Payment_variable, paste(regressors.final,collapse="+") ,sep="~"))

model.glm_quasi_1 = glm( formula.glm_quasi_1 , data = Data.ref , family = quasi( link=logit ) )
  summary(model.glm_quasi_1)

pred.glm_quasi_1 = predict(model.glm_quasi_1,type="response")
	plot.variable(pred.glm_quasi_1, col = cols )


#######################################################
## odzyski faktyczne a przewidywane z poisson( log )

	sum(pred.glm_quasi_1)
	sum(Data.ref[,Payment_variable])

  windows(height = 6, width = 5 ); par.my()
  plot( Data.ref[,Payment_variable] ~ pred.glm_quasi_1 , pch='.' , col = cols , ylab = Payment_variable )
  	title("wp³aty rzeczywiste vs przewidywane poisson")
	lm.quasi = lm(Data.ref[,Payment_variable] ~ pred.glm_quasi_1)
	abline(lm.pois,col="white");
	text(	  x = max(pred.glm_quasi_1)/2 , y = max(Data.ref[,Payment_variable])*.9
			, labels = paste0( "y = " , paste( paste0( round( lm.pois$coef, 3 ) , c(""," * x") ), collapse = " + ") )
		 )
	#  abline(h=0,col="white")
	#  legend( "topleft" , legend = unique(Data.ref$grupa_portfela) , col = unique(as.numeric(Data.ref$grupa_portfela)) + 1, pch = 20 )

#######################################################
## zapis do tabeli jeœli warto

	model.df[,"glm_log_poisson_1"] = pred.glm_log_poisson_1
		head(model.df)


########################################################################################################################
## 05 ##################################################################################################################
## glm_inv_gamma_1
## wp³aty
##
## PROBLEMY ## K£OPOTY ## KOMPLIKACJE... TEMAT DO PRZESTUDIOWANIA
## k = 4

	plot.variable( (rpois(nrow(Data.ref),lambda = .2) +1) / 100)
wplaty_gamma = Data.ref[,Payment_variable] + (rpois(nrow(Data.ref),lambda = .2) +1) / 100
names(wplaty_gamma) = row.names(Data.ref)
	head(wplaty_gamma)
any(wplaty_gamma == 0)


## MODEL
formula.glm_inv_gamma_1 = as.formula(paste( "wplaty_gamma", paste(regressors.final,collapse="+") ,sep="~"))

model.glm_inv_gamma_1 = glm( formula.glm_inv_gamma_1 , data = Data.ref , family = Gamma( link="inverse" ) ,start=rep(.0001,length(regressors.final)+1) )  ## inverse is a default link
  summary(model.glm_inv_gamma_1)

pred.glm_inv_gamma_1 = predict(model.glm_inv_gamma_1,type="response")

  sum(pred.glm_inv_gamma_1)
  sum(Data.ref[,Payment_variable])

	plot.variable(pred.glm_inv_gamma_1, col = cols )
	plot.variable(pred.glm_inv_gamma_1, col = cols , xlim  = c(0,30000))

model.df[,"glm_inv_gamma_1"] = pred.glm_inv_gamma_1
	head(model.df)


#######################################################
## odzyski faktyczne a przewidywane z gamma("inverse")

	sum(pred.glm_inv_gamma_1)
	sum(Data.ref[,Payment_variable])

  windows(height = 6, width = 5 ); par.my()
  plot( Data.ref[,Payment_variable] ~ pred.glm_inv_gamma_1 , pch='.' , col = cols , ylab = Payment_variable )
  	title("wp³aty rzeczywiste vs przewidywane Gamma")
	lm.gamma = lm(Data.ref[,Payment_variable] ~ pred.glm_inv_gamma_1)
	abline(lm.gamma,col="white");
	text(	  x = max(pred.glm_inv_gamma_1)/2 , y = max(Data.ref[,Payment_variable])*.9
			, labels = paste0( "y = " , paste( paste0( round( lm.gamma$coef, 3 ) , c(""," * x") ), collapse = " + ") )
		 )
	#  abline(h=0,col="white")
	#  legend( "topleft" , legend = unique(Data.ref$grupa_portfela) , col = unique(as.numeric(Data.ref$grupa_portfela)) + 1, pch = 20 )


########################################################################################################################
## 06 ##################################################################################################################
## glm_ident_gamma_1
## wp³aty
##
## PROBLEMY ## K£OPOTY ## KOMPLIKACJE... TEMAT DO PRZESTUDIOWANIA
## k = 4

	plot.variable( (rpois(nrow(Data.ref),lambda = .2) +1) / 100)
wplaty_gamma = Data.ref[,Payment_variable] + (rpois(nrow(Data.ref),lambda = .2) +1) / 100
names(wplaty_gamma) = row.names(Data.ref)
	head(wplaty_gamma)
any(wplaty_gamma == 0)


## MODEL
formula.glm_ident_gamma_1 = as.formula(paste( "wplaty_gamma", paste(regressors.final,collapse="+") ,sep="~"))

model.glm_ident_gamma_1 = glm( formula.glm_ident_gamma_1 , data = Data.ref , family = Gamma( link="identity" ) ,start=rep(.1,length(regressors.final)+1) )  ## inverse is a default link
  summary(model.glm_ident_gamma_1)

pred.glm_ident_gamma_1 = predict(model.glm_ident_gamma_1,type="response")

  sum(pred.glm_ident_gamma_1)
  sum(Data.ref[,Payment_variable])

	plot.variable(pred.glm_ident_gamma_1, col = cols )
	plot.variable(pred.glm_ident_gamma_1, col = cols , xlim  = c(0,30000))

model.df[,"glm_inv_gamma_1"] = pred.glm_inv_gamma_1
	head(model.df)


#######################################################
## odzyski faktyczne a przewidywane z gamma("identity")

	sum(pred.glm_ident_gamma_1)
	sum(Data.ref[,Payment_variable])

  windows(height = 6, width = 5 ); par.my()
  plot( Data.ref[,Payment_variable] ~ pred.glm_ident_gamma_1 , pch='.' , col = cols , ylab = Payment_variable )
  	title("wp³aty rzeczywiste vs przewidywane Gamma")
	lm.gamma_id = lm(Data.ref[,Payment_variable] ~ pred.glm_ident_gamma_1)
	abline(lm.gamma_id,col="white");
	text(	  x = max(pred.glm_ident_gamma_1)/2 , y = max(Data.ref[,Payment_variable])*.9
			, labels = paste0( "y = " , paste( paste0( round( lm.gamma_id$coef, 3 ) , c(""," * x") ), collapse = " + ") )
		 )

	#  abline(h=0,col="white")
	#  legend( "topleft" , legend = unique(Data.ref$grupa_portfela) , col = unique(as.numeric(Data.ref$grupa_portfela)) + 1, pch = 20 )




########################################################################################################################
#!######################################################################################################################
########################################################################################################################
##  15. ZAPIS DANYCH ###################################################################################################
########################################################################################################################

save.image(file=FILEmodel.environment)				## NIE NADPISZ BEZMYŒLNIE!!!

   Variables_df = "log.variables.df"
		get(Variables_df)
		get(Variables_df)[,7]
		get(Variables_df)[,-7]
	Transform_ls = "trans.list"
		get(Transform_ls)
  
#####################

	write.table(model.df, FILEmodel.csv , sep = ";", na="" ,dec = ",", qmethod = "double", row.names = TRUE)

		mget(ls(pattern = '_variable$'))


   (objects.towrite = c (  ls( pattern = '^model\\.' )
								, ls( pattern = '^names\\.' )
                        , ls( pattern = '^regressors\\.' )
								, ls( pattern = 'variable$')
								, "Transform_ls" , "Variables_df", Transform_ls , Variables_df      ## lista / df   przeksztalceñ
								, "summary.binary.list"   ## ls( pattern = '^summary\\.' )
								, "succfail.mat"
								, "Data.ref"
							 ) )

  save( list = objects.towrite , file = FILEmodel.Rdata )
 
   #ch = odbcConnect("PostgreSQL35W",uid="akasprzyk",pwd="akasprzyk") ## IKTORN
	ch = odbcConnect("PostgreSQL35W-T",uid="arkadiusz.kasprzyk",pwd="akasprzyk123") ## TOUDI
   # sqlQuery(ch,paste0("DROP TABLE IF EXISTS ",TABLEmodel))
      TABLEmodel
	sqlSave( channel = ch,tablename = TABLEmodel , dat =  model.df , rownames = "id_produkt_k" , verbose = TRUE )                  ##rownames = names(wynik1[[1]])
	odbcClose(ch)

#########################################################################################################################
#########################################################################################################################
##  16. END  ############################################################################################################
#########################################################################################################################
#########################################################################################################################
 
## TRZEBA JESZCZE POSKLEJAC WYNIKI MODELI W BD ##> PGAdmin !!!!



 
 
 
 
 
 
 
 
  ## not used
  graphics.off()
  log.reg.model.final <- logistic.regression( 
        training.set = Data.trn, 
        Data.tst, Data.ref, Target_variable, 
        random = list(TRUE, 4/5, 12), rys.test.ind = 1,      ##rys.test.ind = 1 => gini na zbiorze testowym, =0 na ucz¹cym
        explanatory.variables = zmienne.final , dir="none", TRUE, TRUE )

  # informacja: test  P(Chi)   -  Hipoteza zerowa: zale¿noœæ jest liniowa - (p.value < 0.05) <- odrzucamy Hipotezê zerow¹
 

summary(log.reg.model.final)
summary.ak.gam(log.reg.model.final) 

  model_variables = list(GoodBadThreshold = GoodBadThreshold, Target_variable = Target_variable, Payment_variable = Payment_variable, Debt_variable = Debt_variable, Ref_variable = Ref_variable )



save(Data.ref, model_variables, log.reg.model.final, regressors, regressors.s, summary.list ,file="model_141223-01.Rdata")


#!#######################################################################################################################
## b. Uaktualnianie modelu ## w razie potrzeby gdy brakuje zmiennych na referencji.
  ## PRZYKLADOWO, gdy nie ma zmiennej "wiek"


#!#######################################################################################################################


        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
## USUN¥C PONIZSZE!!! (ale ostroznie)
        


##################################################################
##################################################################



plot.gam(log.reg.model.final,residuals=FALSE,rug=TRUE,se=TRUE,ask=TRUE)
    
    graphics.off()
    rysuj_predict_GAM (model=log.reg.model.final,predicted.set=rbind(Data.wyc,Data.trn))
    rysuj_predict_GAM (model=log.reg.model.final,predicted.set=Data.trn)

#Scorowanie danych
score.ref = predict(log.reg.model.final,Data.ref,type="response")
score.wyc = predict(log.reg.model.final,Data.wyc,type="response")
score.all = predict(log.reg.model.final,Data.all,type="response")

score.ref2 = predict(log.reg.model.final,Data.ref2,type="response")
score.wyc2 = predict(log.reg.model.final,Data.wyc2,type="response")
score.all2 = predict(log.reg.model.final,Data.all2,type="response")

ref.id_bank = Data.ref2$id_bank
names(ref.id_bank) = Data.ref2$id_produkt
Data.MOref$id_bank = ref.id_bank[as.character(Data.MOref$id_produkt)]
table(Data.MOref$id_bank)

    ### names(score.ref) = Data.ref$id_produkt     #!  WAZNE! do polaczenia z Data.MOref
#
#write.table(Data.all , "Data.all.banki.kom.csv", sep = ";", dec = ",", qmethod = "double", row.names = F)
sciezka = paste(getwd(),"/Data.all.banki.kom.csv", sep = "")     ### œcie¿ka do pliku CSV
Data.all2 <- read.csv(sciezka, dec = ",", sep = ";", row.names = c()) 

head (Data.all2)
Data.all$score = score.all
Data.ref = Data.all[Data.all$referencja==1,]
Data.wyc = Data.all[Data.all$referencja==0,]

Data.ref2 = Data.all2[Data.all2$referencja==1,]
Data.wyc2 = Data.all2[Data.all2$referencja==0,]
dim(Data.all)
dim(Data.all2)
head(Data.all2)
head(Data.all)

  windows();margins.my(); coldef.black()
  par(mfrow=c(3,2))
  plot(score.all~Data.all$saldo_pocz,pch=".")#,xlim=c(0,40000))
    plot(density(score.all)) 
  plot(score.ref~Data.ref$saldo_pocz,pch=".")#,xlim=c(0,40000)) 
    plot(density(score.ref))
  plot(score.wyc~Data.wyc$saldo_pocz,pch=".")#,xlim=c(0,40000)) 
    plot(density(score.wyc))
    



save(log.reg.model.final,file=paste(Model_file,"_",format(Sys.time(),"%Y%m%d_%H%M%S"),".Rdata",sep=""))

    range(Data.all$saldo_pocz[Data.all$referencja==1])   ## 3 736.61 34 384.54
    range(Data.all$saldo_pocz[Data.all$referencja==0])   ## 1 332.04 93 471.01
    
    breaks = c(0,10,15,20,100)*1000
    plot.variable( Data.all$saldo_pocz[Data.all$referencja==1]                                                                 
          ,FUN = function(x)x                                                                   
          ,what = c("bars","cvs","density","cloud") ## "hist", "cvs"                           
          ,pch = "." , cex = 1                                                                  
          ,coldef = "black"                                                                     
          ,horizontal = FALSE                                                                   
          #,breaks = breaks                                                                     
          #,breaks.lines = TRUE                                                                 
          #,xlim=c(0,100000)                                                                     
          ) 
    plot.variable( Data.all$saldo_pocz[Data.all$referencja==0]                                                                 
          ,FUN = function(x)x                                                                   
          ,what = c("bars","cvs","density","cloud") ## "hist", "cvs"                           
          ,pch = "." , cex = 1                                                                  
          ,coldef = "black"                                                                     
          ,horizontal = FALSE                                                                   
          #,breaks = breaks                                                                     
          #,breaks.lines = TRUE                                                                 
          #,xlim=c(0,100000)                                                                     
          )    
              
#!#######################################################################################################################
#!#######################################################################################################################
#! 15. Prognoza na zachowanie portfela Wycenianego ######################################################################
##    
preliminary.SR(Data.wyc,Data.ref,log.reg.model.final,Payment_variable,Debt_variable,1,TRUE)
preliminary.SR(Data.wyc2,Data.ref2,log.reg.model.final,Payment_variable,Debt_variable,1,TRUE)
#!  0.3379146

  any( !Data.MOref$id_produkt %in% Data.ref$id_produkt )
  sum( !Data.ref$id_produkt %in% Data.MOref$id_produkt )    #!!!  289
  table(Data.all2$referencja)
   ##      0     1
   ##   27510  5425
  length(unique(Data.MOref$id_produkt))
   ##   5136    =  5425 - 289




##############################################################################
## Wersja 1: najpierw podzial na REFERENCJI potem przylozenie grup do WYCENIANEGO
km.score.groups <- kmeans(  score.ref, centers = 3  , iter.max = 10 , nstart = 25 )
    names(km.score.groups)
  ( glim = sort(tapply(score.ref,km.score.groups$cluster,max)) )
  ( breaks = c(0,glim[1:2],1) )  #   breaks = c(0,0.2505091,0.5392719,1)

km.score.groups2 <- kmeans(  score.ref2 , centers = 3  , iter.max = 10 , nstart = 25 )
    names(km.score.groups2)
  ( glim = sort(tapply(score.ref2, km.score.groups2$cluster,max)) )
  ( breaks2 = c(0,glim[1:2],1) ) 
    
Data.all$grupy_score = cut( Data.all$score , breaks = breaks , include.lowest = TRUE)
Data.ref = Data.all[Data.all$referencja==1,]
Data.wyc= Data.all[Data.all$referencja==0,]

table(Data.ref$grupy_score)

Data.all2$grupy_score2 = cut( Data.all2$score , breaks = breaks2 , include.lowest = TRUE)
  table( Data.all$referencja,Data.all$grupy_score )    #! OK!!!!
  table( Data.all2$referencja,Data.all2$grupy_score2 )    #! OK!!!!
##       [0,0.251]    (0.251,0.539]    (0.539,1]
##  0      5043          6931            15536
##  1      2151          1705             1569

 #[0,0.273] (0.273,0.624] (0.624,1]
 # 0      4907          5909      4604
 # 1      2202          1613       603
  
Data.ref = Data.all[Data.all$referencja==1,]
Data.wyc = Data.all[Data.all$referencja==0,]

Data.ref2 = Data.all2[Data.all2$referencja==1,]
Data.wyc2 = Data.all2[Data.all2$referencja==0,]
 sum(Data.wyc2$saldo_pocz)

      table(Data.wyc$grupy_score) ; table(Data.ref$grupy_score)
      head(Data.MOref)
      dim(Data.MOref)

grupy_score.ref = Data.ref$grupy_score
grupy_score.ref2 = Data.ref2$grupy_score2
names(grupy_score.ref) = Data.ref$id_produkt
names(grupy_score.ref2) = Data.ref2$id_produkt
#!!!
Data.MOref$grupy_score = grupy_score.ref[as.character(Data.MOref$id_produkt)]
Data.MOref$grupy_score2 = grupy_score.ref2[as.character(Data.MOref$id_produkt)]

SR1 = curves.by.groups(   Data.wyc[ , c("saldo_pocz","grupy_score") ]
                     ,  Data.MOref[ , c("saldo_pocz","grupy_score","mies","wpl_kom_mies","id_produkt") ]  
#                    , amount.var = NULL , group.var = NULL
#                    , time.var = NULL , yield.var = NULL , id.var = NULL
                     , file = "krzywe_nowe_telco_kom.csv" )
SR1   ## OK
 head( Data.wyc)
table(Data.MOref$grupy_score)
head( Data.MOref)

plot.curves.by.groups(SR1,"normal")
plot.curves.by.groups(SR1,"cumulated")

                     
scbg = smooth.curves.by.groups( SR1
                     #, interest = .03
                     , horizon = 120
                     , wiggliness = 1
                     , method = "cumulated"#"normal" ## ## 
                     , file = NULL
                     , graphics = TRUE
                     #, xlim = c(0,40)
                     )
                     
colSums(SR1$counts)
##   reference  valuated
##     2597     27510               OK

table(Data.MOref$grupy_score)

##############################################################################
## Wersja 2: najpierw podzial na WYCENIANYM potem przylozenie grup do REFERENCJI
km.score.groups <- kmeans(  score.wyc ,centers = 3  ,   iter.max = 10 ,nstart = 25 )
     names(km.score.groups)
  sort(tapply(score.wyc,km.score.groups$cluster,max))
#   breaks_2 = c( 0 , 0.3749465 , 0.7749735 , 1 )

Data.all$grupy_score_2 = cut( Data.all$score , breaks = breaks_2 , include.lowest = TRUE)

  table( Data.all$referencja,Data.all$grupy_score_2 )    ## OK!!!!
##       [0,0.251]    (0.251,0.539]    (0.539,1]
#  0      7600         11155      8755
#  1      2918          2157       350

Data.ref = Data.all[Data.all$referencja==1,]
Data.wyc = Data.all[Data.all$referencja==0,]
      table(Data.ref$grupy_score_2)
      table(Data.wyc$grupy_score_2)
      ##

      head(Data.MOref)
      dim(Data.MOref)

grupy_score_2.ref = Data.ref$grupy_score_2
names(grupy_score_2.ref) = Data.ref$id_produkt


Data.MOref$grupy_score_2 = grupy_score_2.ref[as.character(Data.MOref$id_produkt)]

  any( !Data.MOref$id_produkt %in% Data.ref$id_produkt )
  sum( !Data.ref$id_produkt %in% Data.MOref$id_produkt )    #!!!  2828

SR2 = curves.by.groups(    Data.wyc[ , c("saldo_pocz","grupy_score_2") ]
                     ,  Data.MOref[ , c("saldo_pocz","grupy_score_2","mies","wpl_kom_mies","id_produkt") ]
#                    , amount.var = NULL , group.var = NULL
#                    , time.var = NULL , yield.var = NULL , id.var = NULL
                     , file = "krzywe2.csv" )

SR2   ## OK

plot.curves.by.groups(SR2,"normal")
plot.curves.by.groups(SR2,"cumulated")

                     
scbg = smooth.curves.by.groups( SR2
                     #, interest = .03
                     , horizon = 120
                     , wiggliness = 2
                     , method = "normal" # ## ##  "cumulated"
                     , file = NULL
                     , graphics = TRUE
                     , xlim = c(0,20)
                     )
                     
colSums(SR1$counts)

graphics.off()



################################################################################
################################################################################

Data.all <- create.new.variable.with.PKDW(dane = Data.all, nazwa.nowa = "CzyPKDWPonad20000", przedzialy = breaks)

Data.all <- create.new.variable.with.PKDW(dane = Data.all, nazwa.nowa = "PKDWprzedzial", przedzialy = breaks)

Data.all$group.variable = 1

####################################################################################################

Data.all$grupy_salda2 = as.numeric(Data.all$grupy_salda)

SR.from.ref <- predict.SR.in.horizon(data.all.and.group = Data.all, model = log.reg.model.final,
 group.variable = c("group.variable"),  ##zmienna wobec której zostan¹  
id_var = "id_produkt",  ##zmienna bêd¹ca id w Dane.ll
 min.grupa = 50, nr_of_groups = 3,
    debt.wyc.val = 0.000000001, debt.ref.val = 0.00000000001,
  wplaty_mies_tabela = Data.MOref, 
  wplaty_mies_Payment_var = "wpl_kom_mies",
  wplaty_mies_Debt_var = "saldo_pocz",
  wplaty_mies_Period_var = "mies",
  wplaty_mies_id_var = "id_produkt" ,      ##zmienna w tabeli wp³at kompatybilna z id_var
wstecz = list(FALSE, 12),
liczba.losowan = 25 ##liczba.losowan domyœlnie 50, czym wiêcej tym lepiej, niestety wyd³uza to dzia³anie procedury
)


predykcja <- predict(log.reg.model.final,type='terms',Data.trn)


###Dane w grupach scorów:

Data.wyc.ze.scor = data.all.and.group[data.all.and.group[,Ref_variable]==0,]            ##group.variable odpowiada grupie scorow z funkcji predict.SR.and.process in horizon
write.table(Data.wyc.ze.scor, "Data.wyc.ze.scor.csv", sep = ";", dec = ",", qmethod = "double", row.names = F)

Data.ref.ze.scor = data.all.and.group[data.all.and.group[,Ref_variable]==1,]            ##group.variable odpowiada grupie scorow z funkcji predict.SR.and.process in horizon
write.table(Data.ref.ze.scor, "Data.ref.ze.scor.csv", sep = ";", dec = ",", qmethod = "double", row.names = F)

Data.all.ze.scor = data.all.and.group                                                   ##group.variable odpowiada grupie scorow z funkcji predict.SR.and.process in horizon
write.table(Data.all.ze.scor, "Data.all.ze.scor.csv", sep = ";", dec = ",", qmethod = "double", row.names = F)






##ekstrapolacje na miesi¹ce ponad MO referencji
extrapolation(wynik = SR.from.ref,         ##wynik funkcji predict.SR.in.horizon               ##Uwagi: Data.all musz¹ mieæ kolumnê IDSprawa
dane = data.all.and.group,
group.variable = c("group.variable"),   ##zmienna grupuj¹ca
time.horizon = c(72,72),                ##horyzont czasowy wyceny dla odpowiednich grup
by.par = 0.1,                             ##przedzia³ (0,1) parametru wyg³¹dzaj¹cego dzielone na odcinki co 0.1
wsp = list(c(0.95), c(0.95)),                      ##wspó³czynnik spadku po MO referencji w poszczególnych grupach (nastepna = 0.95 * poprzednia :) )
first.change = list(c(0.5, 0.65, 0.9, rep(1,120)),  #first.change - kady element skutecznosci (!w horyzoncie obs³ugi referencji) mnozony jest przez t¹ wartoœæ jednak suma zostaje zachowana
c(0.5, 0.65, 0.9, rep(1,120))),
iteracji = 10000
)

################################################################
################################################################

extrapolation(wynik = SR.from.ref1,         ##wynik funkcji predict.SR.in.horizon               ##Uwagi: Data.all musz¹ mieæ kolumnê IDSprawa
dane = Data.all,
group.variable = c("PKDWprzedzial"),   ##zmienna grupuj¹ca
time.horizon = c(rep(36,2),120),                ##horyzont czasowy wyceny dla odpowiednich grup
by.par = 0.1,                             ##przedzia³ (0,1) parametru wyg³¹dzaj¹cego dzielone na odcinki co 0.1
wsp = list(c(0.95), c(0.95)),                      ##wspó³czynnik spadku w poszczególnych grupach (nastepna = 0.95 * poprzednia :) )
first.change = list(c(0.5, 0.65, 0.9, rep(1,120)),  #first.change - kady element skutecznosci mnozony jest przez t¹ wartoœæ jednak suma zostaje zachowana
c(0.5, 0.65, 0.9, rep(1,120))),
iteracji = 1000
) 

