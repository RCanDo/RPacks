graf <- function(zmienne.graf = zmienne, dane = Data.ref, r=0.3, 
  zmienne.bez.null = intersect(without_null_Wyc, without_null_Ref), 
  target = Target_variable, ver = 2, metoda = list(c(1),c("pearson", "kendall", "spearman")))  
{

if (length(zmienne.bez.null)>0) {zmienne.graf =  intersect(zmienne.graf,zmienne.bez.null)}

zmienne_numeryczne = c()
for (i in zmienne.graf) {
zmienne_numeryczne = c(zmienne_numeryczne,is.numeric(dane[,i]))
}


zmienne.graf = zmienne.graf[zmienne_numeryczne]

metoda.indeks = c(metoda[[1]])
metoda.nazwa = c(metoda[[2]][metoda.indeks])

variance = apply(dane[,zmienne.graf],2, var)
zmienne.graf = names(variance)[which(variance != 0)]

M <- cor(data.frame(
dane[,zmienne.graf]
), method = metoda.nazwa)
bezp = 0


#while((TRUE %in% is.na(M)) || bezp <100) {
#    M <<- cor(data.frame(
#    dane[,zmienne.graf]
#    ), method = metoda.nazwa)
#
#
#    rows = dim(M)[1]
#    colm = dim(M)[2]
#
#    zmienne.graf = dimnames(M)[[1]]
#    for (j in 1:colm) {
#        if (length(which(is.na(M[,j])==FALSE))<2)  {zmienne.graf = zmienne.graf[-j]
#        j = colm + 1}
#    }
#
#    bezp = bezp + 1
#}


Y <- cor(
as.numeric(dane[, target]),
data.frame(
dane[,zmienne.graf]
), method = metoda.nazwa)

nazwy <- names(
data.frame(
dane[,zmienne.graf]
))

wspzm <- wsp_zmiennosci(data.frame(
dane[,zmienne.graf]
))

Data.graf <- skupienia(M,r,nazwy,Y,wspzm,ver)   ##2 - jeœli nowa wersja, 1 - stara wersja
list("wariancja" = variance, "korelacja zmiennych zaleznych" = M, "korelacja ze zmienn¹ objaœnian¹"=Y, "mean/dev" = wspzm, "summary" = summary(data.frame(dane[,zmienne.graf])))
}
