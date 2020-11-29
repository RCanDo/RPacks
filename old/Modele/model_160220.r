########################################################################################################################
## DZIA£ ANALIZ ## ANALIZY ## MODELE STATYSTYCZNE ## A.K. ## 150413
########################################################################################################################
  rm(list=ls())     ## czyszczenie przestrzeni roboczej
########################################################################################################################
#! MODEL ###############################################################################################################
########################################################################################################################
## CEL 0:
##
## OPIS: referencja to
##
## DOCELOWY MODEL:
##		- scoring na odnowienie przerwanych wplat
##
#################################################################
########################################################################################################################
ID = "160220"   ## takie samo jak nazwa katalogu ##  w przybli¿eniu data powstania;
DATEmodel = as.POSIXct("2016-02-19") ## data modelu
ENTITY = ""
##
IDdataref = "160104"		## katalog z danymi referencyjnymi na których by³ zudowany model
IDref = "1"
Ysuffix = ""      		## przyrostek nazwy tabeli ze zmienn¹ objaœnian¹ ## TYLKO JEDEN W CA£YM PLIKU !!!  inny Y = inny model = inny plik i katalog modelu
##
METHOD = ""
prediction.name = ""    ## np: "score" w przypadku scoringu ; "prognoza"  dla innych metod
##
##  BD :  toudi.modele.
##
##    Utworzono zmienne nazw tabel:
##                  TABLE
##   TABLEdataref   "modele.referencja_160104"
##   TABLEdataref_Y "modele.referencja_160104_"
##   TABLEmodel     "modele.referencja_160104_model_160220"
##
##    Utworzono zmienne nazw plików:
##                         FILE
##   FILEdataref.Rdata     "referencja_160104_1.Rdata"
##   FILEmodel_Y           "model_160220_Y.sql"
##   FILEmodel.environment "model_160220_environment.Rdata"
##   FILEmodel.Rdata       "model_160220.Rdata"
##   FILEmodel.sql         "model_160220.sql"
##   FILEmodel.xls         "model_160220.xls"
##   FILEmodel.csv         "model_160220.csv"
##
##    Utworzono zmienne nazw katalogów:
##               PATH
##   PATHdataref "//10.1.1.5/PK_Analitycy/Modele/Dane Referencyjne/160104"
##   PATHmodel   "//10.1.1.5/PK_Analitycy/Modele/Modele/160220"
##   PATHwd      "//10.1.1.5/PK_Analitycy/Modele/Modele/160220"
##
########################################################################################################################


########################################################################################################################
#! I. KATALOGI, ZMIENNE, PAKIETY I SKRYPTY #############################################################################
########################################################################################################################


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
ROOT = "D:/ROBOCZY/PK_Analitycy"
#ROOT = "//10.1.1.5/PK_Analitycy"
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
  #ls()

  ## Gdyby trzeba by³o przerwaæ prace wróc do tego miejsca i zapisz œrodowisko:
  #!	save.image(file=FILEmodel.environment)           ## NIE NADPISZ BEZMYŒLNIE!!!
  ## lub za³aduj œrodowisko gdy wznawiasz prace
    load(file=FILEmodel.environment) ; detach("package:TinnRcom", unload=TRUE) ; library(TinnRcom) ## unloadNamespace("TinnRcom")  may work too

  ## tylko modele (i dane)	
    load(file=FILEmodel.Rdata)

########################################################################################################################

########################################################################################################################
#! 2. UWAGI WSTEPNE ####################################################################################################
## Podstawowym zadaniem niniejszego pliku jest zbudowanie MODELU na danych z pliku  FILEdataref.Rdata (ale NIE  FILEdataref.environment !!!)
## Dane FILEdataref.Rdata s¹ w 100% gotowe do analizy statystycznej: badania korelacji, wykluczania zmiennych, budowy modelu.
##    1. pobranie danych  Data.ref , Data.ref_y  z  FILEdataref.Rdata
##    2. badanie korelacji
##    3. wykluczanie zmiennych
##    4. ...
##    5. stabilny model (byc moze wiele wersji)
##
########################################################################################################################
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
## 3. PAKIETY I SKRYPTY ################################################################################################

## PAKIETY
packages = c( "MASS",
			  	  "RPostgreSQL","RODBC"#,"vioplot"
                , "rpart", "earth" ,
              "randomForest","car",
              "ROCR","RODBC","igraph",
              "gam","rattle","igraph",
              "stats","graphics","grDevices",
              "datasets","methods","base",
              "caret","gdata"#,"sqldf"
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

   dim(Data.ref)   ## 48027   154
   names.ref = names(Data.ref)
   (nulls.tab = nulls.table(Data.ref))

##############################################################


##############################################################
## DANE REFERENCYJNE Y ## zazwyczaj wp³aty w zadanym okresie
   #!
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

########################################################################################################################
## 5. £¥CZENIE DANYCH : [ X , Y ] ######################################################################################
##
## Zanim po³¹czymy X z Y sprawdzamy czy wszystko w porzadku z Y (X jest ju¿ opracowany na etapie "Dane Referencyjne").
   #!
   ## TUTAJ NIEPOTRZEBNE - y jest w Data.ref

   	nulls.table(data.ref_y)
   	# nulls.table(data.ref_y)

   	## NIE USUWAMY NULLi z   data.ref_y   bowiem oznaczaj¹ one ¿e sprawa nie byla tak dlugo w obsludze po s¹dzie
   	##	data.ref_Y[ is.na(data.ref_Y[,"suma_wplat_mo"]), "suma_wplat_mo" ] = 0

   	# table(data.ref_Y[,"suma_wplat_mo"] == 0,useNA = "ifany")

   	#########################################
   	## zmienna nazwowa ID -- pozosta³e zmienne nazwowe w p.7.

      VARid

   	#########################################

      rownames(data.ref_y) = data.ref_y[,VARid]
   	names.ref_y = names(data.ref_y)

      Data.ref = update.df( Data.ref , data.ref_y , variables = names.ref_y , action = "append" )


########################################################################################################################
## 6. SZYBKI PRZEGL¥D ##################################################################################################

	attach(Data.ref)     ## dla wygody, ALE nalezy uwazac by uzupelniac w Data.ref, tj.  Data.ref$zmienna[ index ] = ...
   ## po X zmiennych

   names.tosee = setdiff(names.ref, union(names.id,names.dates) )

   d = 8               ## nr serii zmiennych
   ## lub zamiast numery specyficzne grupy nazw

   rows = 3 ; cols =5  ## ile wierszy i kolumn wykresow w jednym oknie
   inches = 3           ## ile cali na jeden wykres
   windows( width = cols*inches + 1  , height = rows*inches + 1 )
   margins.my();coldef.black();par(mfrow=c(rows,cols))
   for(k in ((rows*cols*(d-1))+1):(rows*cols*d)){
      var.name = names.tosee[k]
      plot.variable( var.name
            , what = c("cloud")     ## "bars", "ecdf", "hist", "density", "cvs", "hist",
            , pch = "." , cex = 1
            , user.window = TRUE
            #, col = referencja + 5   ## ref -> magenta, wyc -> cyan
            , as.factor = 9
            )
   }

   detach(Data.ref)


########################################################################################################################
## 7. GRUPY ZMIENNYCH ##################################################################################################

   ls(pattern='^names\\.')
   names.ref

###################
## identyfikatory
   cat(names.id , sep = "\"\n,\"")
   nulls.tab[nulls.tab$class == "factor","name"]

names.id = c(  "id_produkt_k"
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
## dodatkowe
 ,"wojewodztwo_0_k"
)

names.regressors  ## ustalone w pliku z etapu obróbki danych  dane_referencyjne_YYMMDD_1.r
                  ## tak jak i poni¿sze grupy zmiennych:
   intersect(names.regressors,names.id)
##
( names.dates = grep('^data',names.ref,value=TRUE) )
( names.saldo = grep('^saldo',names.ref,value=TRUE) )
names.full
names.tofill

## pomocnicze grupy zmiennych
(names.dt = grep('^dt',names.ref,value=TRUE))
(names.dp = grep('^dp',names.ref,value=TRUE))
(names.czy = grep('^czy',names.ref,value=TRUE))
(names.kat = grep('^kat',names.ref,value=TRUE))
(names.kwota = grep('^kwota',names.ref,value=TRUE))

   save.image(file=FILEmodel.environment)           ## NIE NADPISZ BEZMYŒLNIE !!!

########################################################################################################################


########################################################################################################################
########################################################################################################################
## II. PRESELEKCJA ZMIENNYCH
########################################################################################################################
## Bêdziemy stosowaæ model earth, który sam automatycznie selekcjonuje zmienne.
########################################################################################################################


########################################################################################################################
########################################################################################################################
## III. BUDOWA MODELU
########################################################################################################################

########################################################################################################################
## 8. MODEL earth ######################################################################################################
library(earth)
get.used.pred.terms = function(mod){
   names(which(apply(mod$dirs[mod$selected.terms,,drop=FALSE],2,function(x){any(x!=0)})))
}

models.table.update = function(mod){
   if(is.character(mod)){
      name.mod = mod
      mod = get(mod,pos=".GlobalEnv")
   }else{
      name.mod = deparse(substitute(mod))
   }
   ##
   names.pars = names(mod)[c(8,6,7,9)]
   mtabrow = as.data.frame(rbind(c(unlist(mod[names.pars]), n.pred = length(get.used.pred.terms(mod)),n.terms=length(mod$selected.terms))))
   rownames(mtabrow) = name.mod
   if(!exists("models.table")){
      #rnams = character(0)
      assign("models.table",mtabrow,pos=".GlobalEnv")
   }else{
      #rnams = rownames(mod.tab)
      models.table <<- rbind(models.table,mtabrow)
   }
   ##
   ##rownames(mod.tab) <<- c(rnams,name.mod)
   models.table
}


dim(Data.ref)   ## 48027   154
length(names.regressors)
VARY

# for(d in 1:9){ ## pêtal rêczna -- wszystko na raz jest niebezpieczne
d = 5 ; model.name = paste0("mod.0",d)

model = earth( y = Data.ref[,VARY] , x = Data.ref[,names.regressors] , glm = list( family = binomial )
               , degree = d)
assign(model.name,model)
models.table.update(model.name)

 summary(model)
windows()
plot(model,main=model.name)
plotmo(model,main=model.name)
plotd(model, hist=TRUE , main = model.name) #???
##
windows();par(mfrow=c(2,2))
plot(model$glm.list[[1]])

## variable importance
evimp(model)

## }


rm(models.table)
for(d in 1:9){
   models.table.update(paste0("mod.0",d))
}
models.table

graphics.off()
for(d in 1:8){
   windows()
   model.name = paste0("mod.0",d)
   plot(get(model.name),main=model.name)
}


model.final = mod.06


########################################################################################################################

######################
## gam
##



cat( names.regressors , sep = "\"\n,\"" )

	## w pierwszej kolelnoœci pozbywamy siê zmienych z 1 wartoœci¹, korzystaj¹c z nulls.table() powy¿ej
regressors.gam = c(
#  "s(dpd_start)"                  # 2
 "s(liczba_listow)"
, "s(liczba_sms)"
, "s(liczba_prob_tel_wych)"
#, "liczba_wizyt"
, "s(liczba_prob_tel_przych_dl)"
#, "liczba_wizyt_sk_dl"
#, "liczba_wych_odebr_dl"           # 1 # +2  # 3
#, "s(wiek)"                         # 5
, "s(saldo_pocz)"
#, "plec"                           # 4
)
(regressors.gam.uns = gsub('\\)','',gsub('^s\\(','',regressors.gam)))

	nulls.table(Data.ref[,regressors.gam.uns])  ## nie nakladaj s() na zmienne z mniej niz 4 roznymi wartoœciami

length(regressors.gam);

#!#!#!#!    summary.pay.list = list()   ## nowa lista z tabelami modeli ## uwazaj by nie usunac!!!!!
k = 5

#! wykonujemy kilkakrotnie sprawdzajac stabilnosc wspólczynnników
## za kazdym razem dodaja sie kolejne kolumny informacyjne do df 'summary.all'
## W zasadzie to jest to samo co w petli wykonuje wielokrotna_budowa_modelu_GAM() ## ktora jednak zwraca wynik w malo czytelnej formie
##
		#		graphics.off()
		#		model.score_0 = logistic.regression(
		#		                #training.set <- Data.trn,  test.set = Data.tst,
		#							 predicted.set = Data.ref ,
		#		                target.variable= Target_variable , explanatory.variables = regressors.binary.final ,
		#		                dir="none" , plots=TRUE, Nonparametric=TRUE,
		#		                random = list(TRUE,3/5,10) , rys.test.ind = 1
		#		                )
##
( formula.gam = as.formula( paste( Target_variable , paste( regressors.gam , collapse = " + ") , sep = " ~ " ) ) )


		# any(apply(df_1[,regressors.pay.gam],2,function(x)length(unique(x))<2))

	logit = function(p){p/(1-p)}

rm(summary.coefstars,summary.signif)
MSE_cv_1 = numeric(0) ; MSE_cv_2 = numeric(0)
ile = 20
for( j in 1:ile){
	repeat {
		ll = df.random.split(  datfram = Data.ref
									, portion = .8
									, variable = Target_variable ## most common is 0 and its frequency will be preserved  ## == "czy_sukces_30do"
									, envir = 1
									, new.names = c("df_1","df_2")
								  )
      check_k = apply(df_1[,regressors.gam.uns],2,function(x){length(unique(x))<2})
		# print(check_k)
	   if ( !any(check_k) ){ break }
	}
	mod.gam_k = gam::gam( formula.gam , data = df_1 , family = binomial(link = "logit") )
		print(summary(mod.gam_k)$parametric.anova)
	##
	cv_k_1 = predict( mod.gam_k , type = "response")
	cv_k_2 = predict( mod.gam_k , newdata = df_2 , type = "response")
	MSE_cv_1 = c( MSE_cv_1 , sum( logit( abs(cv_k_1 - df_1[,Target_variable]) )^2 )/(nrow(df_1) ) ) # - mod.gam_k$rank) )  # ?????????
	MSE_cv_2 = c( MSE_cv_2 , sum( logit( abs(cv_k_2 - df_2[,Target_variable]) )^2 )/(nrow(df_2) ) ) # - mod.gam_k$rank) )  # ?????????
	# summary(gam.01)
###########################################################################
#! patrzymy na zmienne które maj¹ niewlasciwe znaki z punktu widzenia biznesowego (tj. ekspercko) ######################
	summary.gam.coefstars(mod.gam_k , append.to = "summary.coefstars" , envir = 1 , print = FALSE)
   summary.gam.signif(mod.gam_k , append.to = "summary.signif" , envir = 1 , print = FALSE)
}

	summary.coefstars
	summary.gam.signs(summary.coefstars , envir = -1 )#, append = TRUE , print = TRUE)

	summary.gam.signif.agg(summary.signif , FUN = mean )#, envir = -1 , append = TRUE , print = TRUE)

fun.agg = function(x){ agg = c(min(x),mean(x),max(x),sd(x)) ; names(agg) = c("min","mean","max","sd") ; agg  }
summary.all = cbind(   summary.gam.coefstars.agg( summary.coefstars , FUN = fun.agg ) # , envir = -1 , append = FALSE , print = TRUE )
							, summary.gam.signs(summary.coefstars )
							, summary.gam.signif.agg(summary.signif)
						)
###
summary.MSE = rbind( summary.signif[c("AIC","MSE"),] , "MSE_cv_1" =  MSE_cv_1 , "MSE_cv_2" =  MSE_cv_2) ;
	colnames(summary.MSE) = paste0("sample_",1:ncol(summary.MSE))
summary.MSE = cbind( summary.MSE , t(apply(summary.MSE,1,fun.agg)))
###
	summary.all; summary.MSE


## to co dalej jest po tym jak juz wylosuje wystarczajaca liczbe modeli


## zapisujemy wyniki dla kazdego zestawu zmiennych do listy
summary.pay.list[[k]] = list( summary.all = summary.all , summary.MSE = summary.MSE)
	# names(summary.binary.list)[k] = paste(k, mean(as.numeric(summary.binary.list[[k]]["AIC",][(1:ile)*2])),sep=", mean(AIC) = ")
summary.pay.list

## PO KA¯DEJ ITERACJI : !!!!  (F12 jest niebezpiecznie blisko...)
save.image(file=FILEmodel.environment)				## NIE NADPISZ BEZMYŒLNIE!!!

##############
## UWAGI:
## 1. wybieramy model 10!!!
## 2.   ## ...
##############










#!#!#!#!    summary.binary.list = list()   ## nowa lista z tabelami modeli ## uwazaj by nie usunac!!!!!
k = 3
		summary.all = data.frame("lp" = 1:(length(regressors.gam)+2));   rownames(summary.all) = c(regressors.gam,"MSE","AIC");
		summary.all.signif = summary.all;

#! wykonujemy kilkakrotnie sprawdzajac stabilnosc wspólczynnników
## za kazdym razem dodaja sie kolejne kolumny informacyjne do df 'summary.all'
## W zasadzie to jest to samo co w petli wykonuje wielokrotna_budowa_modelu_GAM() ## ktora jednak zwraca wynik w malo czytelnej formie
##
		#		graphics.off()
		#		model.score_0 = logistic.regression(
		#		                #training.set <- Data.trn,  test.set = Data.tst,
		#							 predicted.set = Data.ref ,
		#		                target.variable= Target_variable , explanatory.variables = regressors.binary.final ,
		#		                dir="none" , plots=TRUE, Nonparametric=TRUE,
		#		                random = list(TRUE,3/5,10) , rys.test.ind = 1
		#		                )
##
( formula.gam = as.formula( paste0( "czy_sukces_30do ~ " , paste( regressors.gam , collapse = " + ")  ) ) )

rm(summary.coefstars,summary.signif)
ile = 9
for( j in 1:ile){
	ll = df.random.split(  datfram = Data.reg1
								, portion = .9
								, variable = "czy_sukces_30do"
								, envir = 1
								, new.names = c("df_1","df_2")
							  )
	mod.gam_k = gam( formula.gam , data = df_1 , family = binomial(link = logit) )
	# summary(gam.01)
#! patrzymy na zmienne które maj¹ niewlasciwe znaki z punktu widzenia biznesowego (tj. ekspercko) #################################################################################################
	summary.gam.coefstars(mod.gam_k , append.to = "summary.coefstars" , envir = 1 , print = FALSE)
   summary.gam.signif(mod.gam_k , append.to = "summary.signif" , envir = 1 , print = FALSE)

}
summary.coefstars
summary.signif

summary.gam.signif.agg(summary.signif)

# to co dalej jest po tym jak juz wylosuje wystarczajaca liczbe modeli

## zapisujemy wyniki dla kazdego zestawu zmiennych do listy
summary.binary.list[[k]] = summary.all
names(summary.binary.list)[k] = paste(k, mean(as.numeric(summary.binary.list[[k]]["AIC",][(1:ile)*2])),sep=", mean(AIC) = ")
summary.binary.list

save.image(file=FILEmodel.environment)				## NIE NADPISZ BEZMYŒLNIE!!!

##############
## UWAGI:
## 1.  ## 2.   ## ...
##############

##################################
#for(i in 1:length(summary.binary.list)) {
#  names(summary.binary.list)[i] = paste(i, mean(as.numeric(summary.binary.list[[i]]["AIC",][(1:ile)*2])),sep=", mean(AIC) = ")
#}

################################################################################
## d. Ranking si³y predykcyjnej zmiennych za pomoc¹ lasu losowego ##############
## (dla zmiennych które wesz³y do modelu) ######################################
##
windows();margins.my()
kolejnosc.binary.final <- rank.of.variables(target = Target_variable, dane = Data.ref, a = 4/5, b = 2,
                  variables = regressors.binary.final)#, Ntree = 500)





















########################################################################################################################
#!######################################################################################################################

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


########################################################################################################################
########################################################################################################################
## III. ZAPIS DO PLIKU
########################################################################################################################


  model_variables = list(GoodBadThreshold = GoodBadThreshold, Target_variable = Target_variable, Payment_variable = Payment_variable, Debt_variable = Debt_variable, Ref_variable = Ref_variable )


save(Data.ref, model_variables, log.reg.model.final, regressors, regressors.s, summary.list ,file="model_141223-01.Rdata")


#!######################################################################################################################
## b. Uaktualnianie modelu ## w razie potrzeby gdy brakuje zmiennych na referencji.
  ## PRZYKLADOWO, gdy nie ma zmiennej "wiek"


#!######################################################################################################################


        
        
        
        
