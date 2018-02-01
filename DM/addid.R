########################################################################################################################—•°
## FUNCTIONS HERE
##  names2id() with alias   addid2df()
##  var2vec()  with alias   addid2vec()
##
## DEPENDENCIES
##  none
########################################################################################################################—•°

########################################################################################################################—•°
##addid2df = function(  data
names2id = function(  data   ## data frame or vector
                    , name.id = "ID"
                    , variable = NULL  ## name to be given to vector values in resulting data frame (if data is a vector)
                                       ## or variables to retain in resulting data frame (if data is a data frame)
                    , id.as   = "character"
                    , rownames = TRUE
                    , name.df = NULL
                    , envir = NULL
						){
########################################################################################################################—•°
## This function works in two modes, depending on a class of 'data' parameter.
## If 'data' is a vector then the function converts it into a two-column data frame
## where the first column (ID column) has the vector's names as its value and the second column is the vector itself.
## If the 'data' is already a data frame then the function adds to it an ID column (as the first one)
## constructed from the row names.
##
##    Arguments
##  data                vector or data frame.
##                      If vector then it will be converted into two column data frame, where the first column is created
##                      from vector names, while the second are the vector values
##                      (the first column has name 'name.id' and the second 'variable' or the vector's name if 'variable' left NULL).
##                      If data frame then the result of the function is a data frame with its rownames appended as the first column.
##  name.id = "ID"      name to be given to the first column containing names of the 'data' elements (id column).
##                      The default name of the ID column is 'ID' or, if such a column already exists (in case of data frame),
##                      'ID.k' where k is the lowest integer such that 'ID.k' is not present in names of data frame.
##                      Nonetheles, the ID name may be set to arbitrary value,
##                      which will be adjusted in the same manner as default value.
##  id.as = "character" a class of an ID column; if "numeric" and 'data's names cannot be coerced to numeric then
##                      they are left as character;
##  variable = NULL     if data is a vector - a name to be given to the column containing vector values in resulting data frame;
##                      if data is a data frame - variables to retain (as their names or numeric positions) in resulting data frame;
##                      in the latter case if 0 then the resulting data frame will consists only of ID variable;
##								      if left NULL, then for vector its name is retained, and for data frame all its variables are retained.
##  name.df = NULL      name to be given to data frame; if NULL then the name of 'data' is used;
##  rownames = TRUE     should the rownames be given (equal to ID variable) to the resulting data frame?
##  envir = NULL        If NULL (default) then the function returns only data frame with ID column (this is minimal possible output).
##                      If one of -1, 0, 1, 2, ..., the function returns a data frame and a list (called result) with additional informations:
##                        name.id, variable, id.as, rownames, name.df, envir[onment].
##                      Simply speaking, these are parameters of the call, however, name.id returned may differ from name.id passed
##                      as it is adjusted to not overwrite variable already present in 'data'.
##                      If 0 then a data frame with ID column is attached to the list as its first element.
##                      Otherwise 'envir' means a numeric position of an environment to which data frame should be returned;
##                      it is passed to assign() and has the same set of possible values as option 'pos' in assign().
##                      However the -1 sets environment to parent.frame() i.e. returnes data frame to the environment
##                      from which the function was called NOT to the parent environment of the function (executING environment)
##                      as in assign() -- this is useful when the function is called within another function.
##
##    Value
##  If 'data' is data frame then the value returned is a data frame of the following shape
##       data.frame( ID = rownames(data) , data[,variable] )
##  where 'variable' are all 'data' columns or its subset as described at 'variable' above.
##  If 'data' is vector then the value returned is a data frame of the following shape
##       data.frame( ID = names(data) , variable = data )
##  where 'variable' is given by the user or, if left NULL, variable = deparse(substitute(variable))
########################################################################################################################—•°

if(is.null(name.df)){
   name.df = deparse(substitute(data))  ## will be used only once (at the end, (1)) and only if 'envir' NOT NULL and !=0
}else{
   if(length(name.df)>1){warning("Only the first element of 'name.df' was used.") ; names.df = names.df[1] }
   if(!is.character(name.df)){warning("'name.df' was coerced to character.") ; name.df = as.character(name.df) }
}

if( "data.frame" %in% class(data) ){


#  if(length(data)>1){
#    warning("This function is designed to convert named vector 'data' to data frame with names as ID variable.
#  If 'data' is already a data frame then a new data frame consists of variables indicated by 'cols' parameter
#  while rownames(data) is taken as an ID variable and retained as rownames regardless of parameter 'rownames' value.")
#  }

	if(is.null(variable)){
		variable = names(data)
	}

	id.only=FALSE

	if(is.character(variable)){
	  	variable.notin = setdiff(variable,names(data))
		variable = intersect(variable, names(data))
	}else if(is.numeric(variable)){
		if(length(variable) == 1  && variable==0){
			id.only=TRUE
         variable.notin = numeric(0)
			variable = numeric(0)
		}else{
		  	variable.notin = setdiff(variable,1:length(data))
			variable = names(data)[intersect(variable,1:length(data))]
		}
	}else{
	 stop("Parameter 'variable' should be of class 'character' (names of the columns) or 'numeric' (position of the culumns) or left NULL.")
	}


	if( length(variable) == 0 & !id.only){
			warning("None of the columns indicated by 'variable' are present in 'data'.
  Resulting data frame consists only of ID variable.")
	}else if( length(variable.notin) > 0 ){
			warning("Parameter 'variable' indicates columns which are not present in the input data frame. These are: \n", paste(variable.notin,collapse="\n") , "\n" )
	}

   if( id.as == "character" ){
      id = rownames(data)
   }else{
      suppressWarnings( id <- as.numeric(rownames(data)) )      #!#!#! warning message should be supressed !!  Try some tryCatch(...) -- complicated!!!
      if(sum(is.na(id))>0){
         warning("rownames(data) cannot be coerced to numeric values.")
         id = rownames(data)
      }
   }

   if(length(variable)==0){

      datfram = data.frame( id , stringsAsFactors=FALSE )   ## rownames of datfram are NOT retained !!!
      names(datfram) = c( name.id )
      if(rownames){ rownames(datfram) = id }

   }else{

      datfram = data[,variable,drop=FALSE]
      datfram = cbind( id , datfram , stringsAsFactors=FALSE )   ## rownames of datfram are retained !!!

      ## if name.id already in use
      if(name.id %in% variable){  ## highly unlikely but may be disastruous
         k = 1 ; name.id_k = paste(name.id , 1 , sep=".")
         while( name.id_k %in% variable ){ k = k+1 ; name.id_k = paste(name.id , k , sep=".")}
         name.id = name.id_k
         warning("Name of ID variable was the same as a name of some variable and was changed to ",name.id,".")
      }

    names(datfram) = c( name.id , variable )
    
    if(!rownames){ rownames(datfram) = 1:nrow(data) }

   }

}else{  ## data is vector

  if( !is.null(dim(data)) ){ #& ncol(data)>1){
    stop("This function is designed to convert named vectors to data frame with names as ID variable.
  Vectors must have NULL dimension attribute: dim(data) must return NULL.")}


  if(id.as == "character"){
    if(is.null(names(data))){
      id = as.character(1:length(data))
    }else{
      id = names(data)
    }
  }else{
    if(is.null(names(data))){
      id = 1:length(data)
    }else{
      suppressWarnings( id <- as.numeric(names(data)) )      #!#!#! warning message should be supressed !!  Try some tryCatch(...) -- complicated!!!
      if(sum(is.na(id))>0){
        warning("names(data) cannot be coerced to numeric values.")
        id = names(data)
      }
    }
  }

  if(is.character(variable)){ variable = variable[1]
  }else if(!is.null(variable)){
    warning("If 'data' is a vector then values of 'variable' other then character are ignored and 'variable' is set to the name of a vector.")
    variable = NULL
  }

   ##  name.df = deparse(substitute(data))
  if(is.null(variable)){
    variable = deparse(substitute(data))
  }


  if(name.id == variable){
    name.id = paste(name.id,1,sep=".")
    warning("Name of ID variable 'name.id' was the same as a name of a variable and was changed to ",name.id,".")
  }

  datfram = data.frame( id , data , stringsAsFactors=FALSE )
  names(datfram) = c( name.id , variable )

  if(rownames){ rownames( datfram) = id }else{ rownames( datfram) = 1:length(data) }

}

########################################################################################################################—•°
## result

###############################################################################—•°
## envir checking

if(!is.null(envir)){
   if(length(envir)>1){
      envir = envir[1]
      warning("Only the first argument of 'envir' was taken.")
   }

   if(!( is.numeric(envir) && envir>=-1 && round(envir)==envir )){
      envir = -1
      warning("Parameter 'envir' set to NULL as it was none of the -1, 0, 1, ...
   Only data frame is returned.")
   }
}
###############################################################################—•°

if( is.null(envir) ){   ## minimal output -- only data frame, no information
   result = datfram
}else{
   if(envir==0){
      result = list( data.frame = datfram , name.id = name.id , variable = variable , id.as = id.as , rownames = rownames , name.df = name.df , envir = envir )
   }else{
      result = list( name.id = name.id , variable = variable , id.as = id.as , rownames = rownames , name.df = name.df , envir = envir )
      if( envir == -1 ){
         assign( name.df , datfram, pos =  parent.frame() )
      }else{
         assign( name.df , datfram, pos = envir )       ## (1)
         cat("\n")
         cat("Data frame is returned to variable \"",name.df,"\" in environment ",search()[envir],".",sep="")
         cat("\n\n")
      }
   }
}

result

} ##----END----##
########################################################################################################################—•°
addid2df = function(...){ df = names2id(...); df } ## just an alias for names2df() to retain old name in action
   #!!!!!! may not work properly when using  envir = -1  ????????
########################################################################################################################—•°

########################################################################################################################—•°
var2vec = function( datfram , variable = 1 , names = 0){
########################################################################################################################—•°
## An indicated variable of datfram is turned into a named vector.
## A smart wrapper for
##    vec <- datfram[,variable]
##      names(vec) <- rownames(datfram)   ## names = 0  or  names = NULL
##      ## or
##      names(vec) <- datfram[,names]     ## names in colnames(datfram) or 1:ncol(datfram)
##      ## or
##      names(vec) <- 1:length(vec)       ## names = -1
##
## If 'datfram' is already a vector then the resulting vector is this 'datfram'
## with names equal to 1:length(datfram) if names(datfram) is NULL.
##
##    Arguments
##  datfram       data frame of which one of the columns (the first by default) will be turned into a named vector;
##  variable = 1  which variable should be turned into a vector indicated by its numeric position or a name;
##                if 0 then rownames will be turned into vector.
##  names = 0     which variable should be used to give names to the resulting vector values;
##                if 0 or NULL then names of the resulting vector are set to be rownames(datfram);
##                if -1 then names will be set to 1:nrow(datfram);
##                other numeric values indicates position of the variable in a datfram,
##                while character value indicates name of the column;
##                if datfram is a vector and  variable=0  than any value of 'names' different from NULL, 0, -1
##						    will return the values of a datfram as a names of a resulting vector.
##
##    Value
## A named vector.
########################################################################################################################—•°


if( is.null(dim(datfram)) ){

   if(is.null(names(datfram))){ names(datfram) = 1:length(datfram) }

  if(is.numeric(variable) && variable == 0){

		vec = names(datfram)

      if( !is.null(names) ){
			if(names == -1){
            names(vec) = 1:length(vec) ## not necessary in fact
			}else if(names==0){
            names(vec) = names(datfram)
			}else{
				names(vec) = datfram
			}
		}else{
         names(vec) = names(datfram)
		}
  }else{
  		vec = datfram
	   if(is.null(names(vec))){
		    names(vec) = 1:length(vec)
		#    warning("This function is designed to convert single variable of data frame 'datfram' into a named vector with names(vector) = rownames(datfram).
		#  If datfram is a vector then its names are retained or set to 1:length(datfram) if names(datfram) is NULL.")
		}
  }
  ##


}else{

  if(length(variable)>1){
    variable = variable[1]
    warning("Parameter 'variable' should indicate the single variable of 'datfram' to be turned into named vector.
    Only the first value of 'variable' is taken.")
  }

  ## which variable should be turned into a vector
  if(is.character(variable) && !variable%in%names(datfram)){
    stop("There is no variable with the name '",variable,"' in 'datfram'.")
  }else if(is.numeric(variable) && !variable%in%0:length(datfram)){
    stop("Value of 'variable', wich is ",variable,", is out of bound 0:ncol(datfram) (0 means 'rownames').")
  }

  ## which variable will serve as names to vec
  if(is.null(names)){ ## the same as names == 0
    names = rownames(datfram)
  }else if(is.character(names)){
    if(!names%in%names(datfram)){
      stop("There is no variable with the name '",names,"' in 'datfram'.")
    }else{
      names = datfram[,names]
    }
  }else if(is.numeric(names)){
    if( names == 0 ){    ## the same as is.null(names)
      names = rownames(datfram)
    }else if( names == -1 ){
      names = 1:nrow(datfram)
    }else if(!names%in%1:length(datfram)){
      stop("Value of 'names', wich is ",names,", is out of bound 1:ncol(datfram).")
    }else{
      names = datfram[,names]
    }
  }else{
    stop("Parameter 'names' should be of class 'character' (name of the variable) or 'numeric' (position of the culumn) or left NULL.")
  }

  if(is.numeric(variable) && variable == 0){
		vec = rownames(datfram)
  }else{
  		vec = datfram[,variable]
  }
  names(vec) = names

}

vec

} ##----END----##
########################################################################################################################—•°
addid2vec = function(...){ vec = var2vec(...); vec } ## just an alias for var2vec() to retain old name in action
########################################################################################################################—•°



########################################################################################################################—•°
## EXAMPLES ############################################################################################################—•°
########################################################################################################################—•°

########################################################################################################################—•°
dummy = function(){
## This is dummy function —— it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## —— in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
###################################################################################################—•°

 ## RELOADER —— before it works you need to source("PacksAK.R"); it's best to use DM.R within pack's dir.
 loadPacksAK("DM")

###################################################################################################—•°

###################
##  names2id
###################
##  addid2df  (alias for names2id)
###################
## 1. vectors

## 1.1
( v1 = rnorm(10) )
(df0 = names2id(v1))
class(df0[,1])
(df0 = names2id(v1, id.as = "numeric"))
class(df0[,1])

## 1.2
v2 = v1 ; names(v2) = sample(100:200,10) ; v2
(df0 = names2id( v2 ))
class(df0[,"ID"])
(df0 = names2id( v2 , id.as = "numeric" ))
class(df0[,"ID"])
(df0 = names2id( v2 , name.id = "id" ))
(df0 = names2id( v2 , variable = "variable" ))
(df0 = names2id( v2 , name.id = "id" , variable = "variable" ))

(df0 = names2id( v2 , rownames = FALSE ))

## 1.3
v3 = v1 ; names(v3) = sample(letters[10:20],10) ; v3
(df0 = names2id( v3 ))
class(df0[,"ID"])
(df0 = names2id( v3 , id.as = "numeric" ))
class(df0[,"ID"])
(df0 = names2id( v3 , name.id = "id" ))
(df0 = names2id( v3 , variable = "variable" ))

(df0 = names2id( v3 , rownames = FALSE ))

## 1.4
( v4 = sample(letters[1:3],10,replace=TRUE) ) ; class(v4)
(df0 = names2id(v4))
sapply(df0,class)

## factor
( v4 = factor(v4) )
(df0 = names2id(v4))
sapply(df0,class)

## ordered
( v4 = factor(v4,ordered = TRUE) )
(df0 = names2id(v4))
sapply(df0,class)
   df0$v4


###################
## envir
names2id( v3 , envir = 0)
names2id( v3 , envir = 0)[[1]]
##
names2id( v3 , envir = 1)
   v3    ## v3 is now a data frame
   ## recreate v3 as vector
   v3 = v1 ; names(v3) = sample(letters[10:20],10) ; v3
names2id( v3 , envir = 1 , variable = "variable")
   v3    ## v3 is now a data frame with 'variable'

   ## recreate v3 as vector
   v3 = v1 ; names(v3) = sample(letters[10:20],10) ; v3
names2id( v3 , envir = 1 , name.df = "mydf" )
   v3    ## v3 is still a vector
   mydf
   rm(mydf)
   

## envir = -1 -- returning to parent frame (see ?parent.frame )

names2id( v3 , envir = -1 , variable = "variable") ## parent frame is .GlobalEnv;  no message!
v3


## espacially useful when using in another function
ff = function(data){
   ll = names2id( data , envir=-1 )  ## data frame returned to "data" within  ff execution environment
   result = list( dim(data) , ll$name.id , names(data))
   result
}

   v3 = v1 ; names(v3) = sample(letters[10:20],10) ; v3
ff(v3)
	v3 ## not changed in .GlobalEnv
##
	names2id(v3)
ff(names2id(v3))
	v3


## little different
ff = function(data){
   ll = names2id(data,envir=-1,name.df = "mydf" )  ## data frame returned to "mydf"
   result = list( dim(mydf) , ll$name.id  , names(data) , names(mydf) )
   result
}

ff(v3)
ff(names2id(v3))
## works exactly the same way, except names(...) returnes differrent results


###################
## 2. data frames
df1 = data.frame( aa = sample(10)
                , bb = sample(letters[1:10])
                , cc = sample(LETTERS[1:3],10,replace=TRUE)
                , dd = rnorm(10))
df1
   sapply(df1,class)

## 2.1
## notice that data frame has always some rownames, if not defined explicitely then they're just row numbers
##
( df10 = names2id( df1 ) )
   sapply(df10,class)
( df10 = names2id( df1 , id.as = "numeric" ) )
class(df10[,1])

##
 names2id( df1 , name.id = "id" )
 names2id( df1 , name.id = "id" , variable = "variable" )   ##  different meaning of 'variable' for 'data' a data frame
## 'variable' now indicates which variables of data frame should be retained
 names2id( df1 , name.id = "id" , variable = "aa" )   ##  different meaning of 'variable'
(df0 = names2id( df1 , name.id = "id" , variable = c("aa","bb") )  ) ##  different meaning of 'variable'
   sapply(df0,class)
## you may also use numeric values
 names2id( df1 , name.id = "id" , variable = 1:2 )   ##  different meaning of 'variable'
 names2id( df1 , name.id = "id" , variable = 1 )   ##  different meaning of 'variable'
##
## when data is a vector
v1
 names2id( v1 , name.id = "id" , variable = 1 )    ## the same as variable = NULL
 names2id( v1 , name.id = "id" , variable = 1:2 )
 names2id( v1 , name.id = "id" , variable = c("a","b") )   ## only the first value is taken
 names2id( v1 , name.id = "id" , variable = c(1,"b") )     ## in this case numeric value is turned into character (!) and only the first value is taken
 names2id( v1 , name.id = "id" , variable = as.character(1) )

## 2.3
## row names that cannot be coerced to numbers
df3 = df1 ; rownames(df3) = sample( letters[10:20],10 )  ; df3
( df30 = names2id( df3 ) )
class(df30[,1])
( df30 = names2id( df3 , id.as = "numeric" ) ) ##
class(df30[,1])

## you may get rid of rownames while turning into ID variable
names2id( df3 , rownames = FALSE )

## 2.4
df3
names2id(df3)
names2id(df3,variable=NULL) ## default option
names2id(df3,variable=0)  ## only ID variable
names2id(df3,variable=1)
names2id(df3,variable=1:3)
names2id(df3,variable=1:5)
names2id(df3,variable=1:7)
names2id(df3,variable=7)
names2id(df3,variable=numeric(0)) ## NOT the same as NULL
names2id(df3,variable="")         ##  "
## so this is the way of making data frame with one ID variable
names2id(df3,variable="",rownames = FALSE)  ## with natural enumeration
names2id(df3,variable=0)  ## better: no warning!
names2id(df3,variable=0,rownames = FALSE)  ## no warning!
names2id(df1,variable=0)
## BUT
names2id(df3,variable=c())        ## works as variable = NULL

	## by the way
	var2vec(names2id(df3,variable=0,rownames = FALSE))
   var2vec(df3,variable=0)
   var2vec(df3,variable=0,names=1)
   var2vec(df3,variable=0,names=-1)
	var2vec(df3)


names2id(df3,variable=c("aa","cc","ee","gg"))
names2id(df3,variable=c("ee","gg"))

##
( df4 = names2id(df3,name.id="aa"))
names2id(df4,name.id="aa")        ## and so on..
## BUT
(df44 = names2id(df4,name.id="aa.1"))
names2id(df44,name.id="aa.1")     ## and so on..

###################
## envir
##  -- works the same as in case of 'data' a vector
df1
names2id( df1 , envir = 0)
names2id( df1 , envir = 0 , name.id = "aa")



########################################################################################################################—•°

###################•°
##  var2vec
###################•°
##  addid2vec   (alias for  var2vec)
###################•°

###################•°
## data frame

##
df1
var2vec(df1)            ## the first variable is default
var2vec(df1,names = 2)  ## which variable to use to produce names
var2vec(df1,names = 1)
var2vec(df1,variable = 2 , names = 1)
var2vec(df1,variable = "cc", names = "dd")
var2vec(df1,variable = "dd", names = "cc")
names(var2vec(df1,variable = "dd", names = "cc")) #! notice that names(vector) do not have to be unique contrary to rownames(data.frame)

##
df1 ; sapply(df1,class)
df2 = cbind(df1,ee = factor(df1$cc,ordered=TRUE),ff = as.character(df1$bb) , stringsAsFactors=FALSE )
df2
sapply(df2,class)
(v0 = var2vec(df2,variable="bb")) ; class(v0)
(v0 = var2vec(df2,variable="cc")) ; class(v0)
(v0 = var2vec(df2,variable="dd")) ; class(v0)
(v0 = var2vec(df2,variable="ee")) ; class(v0)
(v0 = var2vec(df2,variable="ff")) ; class(v0)  ## OK

##
df3
var2vec(df3)       ## ronames turned into names
var2vec(df3,1)     ## default - the first variable is turned into vector
var2vec(df3[,1])   ## rownames lost
var2vec(df3[,1,drop=FALSE])
var2vec(df3[,10])
var2vec(df3,10)

##
var2vec(df3,2)
var2vec(df3,"bb")
var2vec(df3[,1],"bb")  ## variable "bb" des not exist in df3[,1] -- ignored
   df3[,1]
var2vec(df3[,1],"cc")  ##  "

##
df3
var2vec(df3)
var2vec( df3 , names = 0 )    ## names from rownames (default)
var2vec( df3 , names = NULL ) ##    "
var2vec( df3 , names = -1 )   ## names as numeric order
var2vec( df3 , names = 2 )    ## names from variable nr 2
var2vec( df3 , names = 1 )
var2vec( df3 , names = 1 , variable = 2 )
var2vec( df3 , names = "aa" , variable = "bb" )
##
var2vec( cbind(df3,sample(10)+20) , names = 5 )



###################
## vector

v1
var2vec(v1)
var2vec(v1,variable=0)
var2vec(v1,variable=0,names=NULL)
var2vec(v1,variable=0,names=0)
var2vec(v1,variable=0,names=-1)
var2vec(v1,variable=0,names=1)
var2vec(v1,variable=0,names="")
var2vec(v1,variable=0,names="dfarvtvr")
var2vec(v1,variable=0,names=989)


v2
var2vec(v2)
var2vec(v2,variable=0)
var2vec(v2,variable=0,names=NULL)
var2vec(v2,variable=0,names=0)
var2vec(v2,variable=0,names=-1)
var2vec(v2,variable=0,names=1)
var2vec(v2,variable=0,names="")
var2vec(v2,variable=0,names="werhw")
var2vec(v2,variable=0,names=3426)

}
rm(dummy)



