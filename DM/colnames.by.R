########################################################################################################################—•°
## FUNCTIONS HERE
##  colnames.mode()
##  colnames.storage.mode()
##  colnames.typeof()
##  colnames.class()
##
## DEPENDENCIES
##
########################################################################################################################—•°
colnames.class = function( datfram , class ){
########################################################################################################################—•°
## Returnes names of all variables of the given 'class' in a data frame 'datfram'.
########################################################################################################################—•°
 is.class = sapply( datfram , function(x){ length(intersect(class(x),class))>0 } )
 result = names(is.class[is.class])
 result
} ##----END----##
########################################################################################################################—•°

########################################################################################################################—•°
colnames.typeof = function( datfram , typeof ){
########################################################################################################################—•°
## Returnes names of all variables of the given 'class' in a data frame 'datfram'.
########################################################################################################################—•°
 is.typeof = sapply( datfram , function(x){ length(intersect(typeof(x),typeof))>0 } )
 result = names(is.typeof[is.typeof])
 result
} ##----END----##
########################################################################################################################—•°

########################################################################################################################—•°
colnames.mode = function( datfram , mode ){
########################################################################################################################—•°
## Returnes names of all variables of the given 'class' in a data frame 'datfram'.
########################################################################################################################—•°
 is.mode = sapply( datfram , function(x){ length(intersect(mode(x),mode))>0 } )
 result = names(is.mode[is.mode])
 result
} ##----END----##
########################################################################################################################—•°

########################################################################################################################—•°
colnames.storage.mode = function( datfram , storage.mode ){
########################################################################################################################—•°
## Returnes names of all variables of the given 'class' in a data frame 'datfram'.
########################################################################################################################—•°
 is.storage.mode = sapply( datfram , function(x){ length(intersect(storage.mode(x),storage.mode))>0 } )
 result = names(is.storage.mode[is.storage.mode])
 result
} ##----END----##
########################################################################################################################—•°

########################################################################################################################—•°
## EXAMPLES ############################################################################################################—•°
########################################################################################################################—•°



##############################################################################################—•°
dummy = function(){
## This is dummy function - it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## - in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
##############################################################################################—•°

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

colnames.class(datfram,"factor")
colnames.class(datfram,"Date")
colnames.class(datfram,"character")
colnames.class(datfram,"numeric")

sapply(datfram,typeof)
colnames.typeof(datfram,"double")
colnames.typeof(datfram,"integer")

sapply(datfram,mode)
colnames.mode(datfram,"numeric")
colnames.mode(datfram,"name")

sapply(datfram,storage.mode)
colnames.storage.mode(datfram,"double")
colnames.storage.mode(datfram,"integer")
colnames.storage.mode(datfram,"symbol")

##############################################################################################—•°
}
rm(dummy)