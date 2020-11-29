## ---------------------------------------------------------------------------------------------------------------------•°
## FUNCTIONS HERE
##  capply()
##
## DEPENDENCIES
##
##
## ---------------------------------------------------------------------------------------------------------------------•°
capply = function( datfram , class , FUN = function(x){x} , within = FALSE , nrow = 1 ){
## ---------------------------------------------------------------------------------------------------------------------•°
## For a given data frame applies a given function to all variables having a given class
## and returns result as a data frame; in particular can return value to the given data frame.
## May be used e.g. to change within a given data frame a class of all variables having given class.
##
##    Arguments
## datfram        			data frame;
## class          			class of variable to search for within data frame; must be specified as character;
## FUN = function(x){x}		function to apply to each variable within 'datfram' with given argument;
##                         default is function(x){x} which together with within=FALSE (also default) returns
##                         part of 'datfram' having only variables of a given 'class';
##                         if NULL then the function returns only character vector with names of variables being in the 'class'
##                         (the same result is obtained by setting nrow = 0);
## within = FALSE    		if FALSE then only transformed variables will be returned;
##									if TRUE then transformed variables will be returned together with all other variables
##                   		but only when FUN returns a single value
##									or value of length equal to length of input (i.e. nrows(datfram));
## nrow                    if 0 then the function returns the same result as when FUN = NULL (regardless of actual FUN);
##                         if >0 then it matters only when FUN is an aggregate returning one value, like sum() or min();
##                         you may then set how many rows resulting data frame should consists of, each of which 
##                         is a replica of the first one.
##
##
##    Value
## Already described.
##
## ---------------------------------------------------------------------------------------------------------------------•°

is.class = sapply( datfram , function(x){ length(intersect(class(x),class))>0 } )


if( is.null(FUN) | nrow == 0 ){

   result = names(datfram)[is.class]

}else{

   ###################################################################################################•°
   ## FUN applied to the variables with given class

     ##df.name = deparse(substitute(datfram))

   result = apply( datfram[ ,is.class  ,drop = FALSE] , 2 , FUN )

   if(within){ nrow = nrow(datfram) }

   if( is.null(dim(result)) ){

   	result = matrix( result, ncol = length(result) , nrow , byrow = TRUE )

   	result = as.data.frame( result ,  stringsAsFactors = FALSE )

      names(result) = names(datfram)[is.class]

   }else if(  any( dim(result) != dim(datfram[,is.class]) ) ){

     if(within){
     	warning("Result cannot be returned within input data frame as its dimension is not NULL or number of rows is not equal to nrows(datfram).")
     	within = FALSE
     }
   	result = as.data.frame( result ,  stringsAsFactors = FALSE )

   }else{

   	result = as.data.frame( result ,  stringsAsFactors = FALSE )

   }

   ###################################################################################################•°
   ## result

   if(within){	datfram[,is.class] <- result ; result <- datfram }

}

result

} ##----END----##
## ---------------------------------------------------------------------------------------------------------------------•°


## ---------------------------------------------------------------------------------------------------------------------•°
## EXAMPLES ############################################################################################################•°
## ---------------------------------------------------------------------------------------------------------------------•°



## -------------------------------------------------------------------------------------------•°
dummy = function(){
## This is dummy function - it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## - in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
## -------------------------------------------------------------------------------------------•°

 apply(Data.raw[1:10,30:39],2,function(x){c(sum(x),min(x))})

## -----------------------------------
datfram = data.frame(  f1 = factor( sample (letters[1:4], 10, replace = TRUE ) )
							, f2 = rnorm(10)
							, f3 = as.Date(sample(10), origin = "2015-01-01")
							, f4 = sample(200,10,replace=TRUE)
							, f5 = as.Date(sample(300,10),origin = "2014-01-01")
							, f6 = factor( sample(LETTERS[1:4], 10, replace = TRUE) )
							, f7 = gl(2,2,10,labels = c("Control", "Treat"))
							, f8 = as.character(as.Date(sample(300,10), origin = "2014-01-01"))
							, f9 = rnorm(10)
						  )
datfram
sapply(datfram,class)
apply(datfram,2,class) ## ???

## ------------------------------------•°
##
capply( datfram , class="Date")
capply( datfram , class="factor")
capply( datfram , class="Date",within=TRUE )  ## nothing happens effectively

capply( datfram , class="Date" , FUN = as.character)              ## only transformed variables; needed to be assigned to an object explicetly
dfram = capply( datfram , class="Date" , FUN = as.character)      ## only transformed variables; needed to be assigned to an object explicetly
dfram
sapply(dfram,class) ## OK
rm(dfram)

##
capply( datfram , class="Date" , FUN = as.character , within = TRUE)  				## as above but all data from input 'datfram'
dfram = capply( datfram , class="Date" , FUN = as.character , within = TRUE )		## ""
dfram
sapply(dfram,class) ## OK
rm(dfram)

## ------------------------------------•°
##

capply(datfram, class = "numeric" )  ## simply extracts variable of given class
capply(datfram, class = "integer" )  ## simply extracts variable of given class
capply(datfram, class = "Date" )  ## simply extracts variable of given class
capply(datfram, class = "factor" )  ## simply extracts variable of given class
capply(datfram, class = "character" )  ## simply extracts variable of given class

capply(datfram, class = "factor", FUN=as.character )  ##

capply(datfram, class = "Date" , mean)
capply(datfram, class = "Date" , mean , within = TRUE)
capply(datfram, class = "Date" , max )
capply(datfram, class = "Date" , max , within = TRUE)
capply(datfram, class = "numeric" , max , within = TRUE)
capply(datfram, class = "numeric" , is.numeric , within = TRUE)
capply(datfram, class = "numeric" , summary , within = TRUE)
capply(datfram, class = "numeric" , function(x)(x^2) , within = TRUE)

}

## ---------------------------------------------------------------------------------------------------------------------•°

rm(dummy)

