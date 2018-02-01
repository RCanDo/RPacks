nulls.by.id = function( datfram , id_var = NULL ){
## Distribution of NAs with respect to id variable(s).
##  datfram   data frame
##  is.var    identifier variable; may be left NULL (default) and then the first variable from datfram will be taken
##            as ID variable.
##            May be also a numeric or character vector in which several ID variables may be passed
##            as their numeric position in a data frame or their names.
##

names.df = names(datfram)

 if(is.null(id_var)){
    id.var = datfram[,1]
 }else if(is.character(id_var)||(is.numeric(id_var)&(floor(id_var)==id_var))){
    id.var = datfram[,id_var,drop = FALSE]
 }else{
    stop("Incorrect 'id.var' identifier: must be character or integer or left NULL.
          If left NULL then the 1st variable of data frame will be taken as ID variable.")
 }

names.id = names(id.var)
names.var = names.df[!names.df%in%names.id]

tab.na1 = apply(datfram[,names.var],2,function(x)as.numeric(!is.na(x)))


print( as.table(t(unique(cbind( id.var ,tab.na1)))) , zero.print = "." )

}


