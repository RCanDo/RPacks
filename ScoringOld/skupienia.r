skupienia <- function (macierz,r_kryt,nazwy,Ry,V, a = 2){
  library(igraph)
  wiersze=dim(macierz)[1]
  kolumny=dim(macierz)[2]



  macierz_sasiedztwa = matrix(as.numeric(abs(macierz)>r_kryt),wiersze,kolumny)
  diag(macierz_sasiedztwa)<-0

  nazwy=sub('(.)+[.]','',nazwy)
  cl=heat.colors(101);##colors();
  graf.object=graph.adjacency(macierz_sasiedztwa,mode="undirected")
  ## Etykiety - korelacja ze zm. objasniana oraz wsp. zmiennosci
  nazwy=paste(nazwy,sep='\n r=',round(Ry,3))
  nazwy=paste(nazwy,sep='\n V=',round(V,3))
   #vertex.color= rainbow(10)  #... - nalozenie colormapy na wsp. korelacji
  V(graf.object)$label<-nazwy
  V(graf.object)$frame.color<-"black"
 # E(graf)[ V(graf)]$color <- "red"
#vertex.frame.color=cl[round(101-abs(Ry)*100)],

if (a ==1) {tkplot(graf.object,layout=layout.fruchterman.reingold,vertex.label.family="sans", vertex.color=cl[round(101-abs(Ry)*100)],  vertex.label.cex=(10/12))}

if (a ==2) {plot(graf.object,layout=layout.fruchterman.reingold,vertex.label.family="sans", vertex.color=cl[round(101-abs(Ry)*100)],  vertex.label.cex=(10/12), main = paste("korelacja ponad ", r_kryt))}
#plot(graf,layout=layout.graphopt,vertex.label.family="sans", vertex.color=cl[round(101-abs(Ry)*100)],  vertex.label.cex=(10/12))

#label.dist=1,
macierz_sasiedztwa
}
