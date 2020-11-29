## ---------------------------------------------------------------------------------------------------------------------•°
## FUNCTIONS HERE
##  transform.by.trans.list()
##  create.trans.list()
##
## DEPENDENCIES
##  ....r
##    ...() , ...
##
## ---------------------------------------------------------------------------------------------------------------------•°
transform.by.trans.list = function( datfram , trans.list ){
## ---------------------------------------------------------------------------------------------------------------------•°
##
## ---------------------------------------------------------------------------------------------------------------------•°

if(length(trans.list)==1 & is.character(trans.list) ){
  trans.list = get(trans.list,pos=1)
}

cat("\n")

names = intersect(names(datfram),names(trans.list))

if(length(names)>0){

  l_char.vec = character(0)

  for(k in names ){
    l = trans.list[[k]]
      l_char = as.character(as.expression(l)) ; l_char.vec = c(l_char.vec,l_char)
    cat(k," :: ",l_char,"\n")
    ##
  	#if( !( is.null(l) || is.na(l)) ){
  	if( is.language(l) ){
  		x = datfram[,k]
  		datfram[,k] = eval(l)
  	}
  }
  cat("\n")
  print(data.frame( variables = names , transformation = l_char.vec ) , justify = "left" )

}else{

  cat("\n There wasn't anything to transform. ", deparse(substitute(datfram)) ," left unchanged.\n")

}


datfram

		## alternatywna werjsa - wykorzystanie    trans.df[,7]    zamiast    trans.list
		##
		#	for(k in names(Data.ref)){
		#		l = trans.df[,7][[k]]
		#		if(!is.null(l)){
  		#			x = Data.ref[,k]
      #        Data.ref[,k] = eval(l) }
		#	}
		#	head(Data.ref) ; dim(Data.ref)

} ##----END----##
## ---------------------------------------------------------------------------------------------------------------------•°



## ---------------------------------------------------------------------------------------------------------------------•°
create.trans.list = function( datfram ){
## ---------------------------------------------------------------------------------------------------------------------•°
##
## ---------------------------------------------------------------------------------------------------------------------•°

trans.list = list()
for(var.name in names(datfram)){
   var.trans = function(x){x} 	#  {((x+1)^.55 -1)/.55} # {log(x+1)} #{log(x+.001)} #    {log(x)}  #     ## funkcja przekszta³caj¹ca np.  log(x+1),  log( log( x+1 ) ),  ( (x+1)^L - 1 )/L  [Box-Cox]...
	trans.list[var.name] = as.list(body(var.trans))[2]		## lista transformacji
}

trans.list

} ##----END----##
## ---------------------------------------------------------------------------------------------------------------------•°
