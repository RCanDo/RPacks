## ---------------------------------------------------------------------------------------------------------------------•
## FUNCTIONS HERE
##  split.rand.nrows()
##
## DEPENDENCIES
## ...
## ---------------------------------------------------------------------------------------------------------------------
split.rand.nrows = function( datfram , variable = NULL , nrows = 1
		, first.level = NULL		## if NULL all levels from 'variable' will be present in new data frame with equal quantities 'nrows',
		##########################
		, split = FALSE         ## if split = TRUE then 'datfram' is splitted in two data frames;
										## if FALSE then split factor is appended to 'datfram'
		, envir = NULL          ## environment to which return data frames
      , new.names = NULL      ## names to be given to new data frames (if split = TRUE)
										## or to factor levels indicating the split (if split = FALSE).
		, sort = TRUE           ## should resulting data frames be sorted wrt to original order of records?
      ##########################
      , times = 1             ## only for split = FALSE: How many times splitting procedure will be performed.
){
## ---------------------------------------------------------------------------------------------------------------------
##
## ---------------------------------------------------------------------------------------------------------------------

## --------------------------------------------------------

## -----------------------
## split
if( ! is.logical(split)  ){
	stop("The argument 'split' must be logical with possible values TRUE or FALSE.")
}

## -----------------------
## datfram
if(is.character(datfram)){
	name.df = datfram
	datfram = get(datfram)
}else{
	name.df = deparse(substitute(datfram))
}

N = nrow(datfram)

if(! "data.frame" %in% class(datfram)){
	stop("'datfram' must be of class \"data.frame\".")
}

## --------------------------------------------------------
## variable & first.level  preprocessing

if(is.null(variable)){stop("'variable' must be given: a factor(s) name(s) within 'datafram' (passed as a character vector),
  or a name of a factor from .GlobalEnv.
")}

#if(!is.null(variable)){

	############################
	## getting variable
	if( is.numeric(variable) | is.character(variable) ){

		if(is.character(variable)){
			name.variable = variable
		}else{
         name.variable = names(datfram)[variable]
		}

		variable = datfram[,name.variable]  ## 'variable' becomes a variable

		if(length(name.variable)>1){
      		variable = as.factor( apply( variable, 1, function(x){ paste(x,collapse=":") } ) )
				name.variable = paste0(name.variable,collapse="*")
      }

	}else if(is.factor(variable) | is.logical(variable)){

			name.variable = deparse(substitute(variable))

			if(length(variable) != N ){
				stop("Number of elements of 'variable' differs from nrows(datfram).")
			}

	} else{
		stop("The 'variable' should be specified as a vector of character names or numeric positions of 'datfram' variables
  or a name of a factor variable (from the enclosing environment - NOT from 'datfram') or left NULL.")
	}

	############################
	## first.level
	if( !is.null(first.level) ){

		#####################
		## first.level
		if( !( is.numeric(first.level) | is.character(first.level) ) )
		{  first.level = NULL
			warning("The argument 'first.level' is neither numeric nor character nor NULL.
  Its value is set to NULL thus all levels of the 'variable' will be present in new data frame with original proportions
  with assumption that each level will appear at least once."  )
	   }
	}

	if( !is.null(first.level) ){  ## repetition to avoid overstreched  else{}


	  variable = as.factor(variable) ## is order preserved when variable is already ordered factor ?  YES!!!
		levs = levels(variable)


		if( length(levs)>=2 ){

			####################
			##

			if( !first.level %in% levs & !is.numeric(first.level) ){
				warning("The argument 'first.level' must take values 0, integer (indicating level's numeric position) or a name of a level.
  None of these requirements is satisfied and 'first.level' is set to 0 what means that the most common level will be treated as the first one.")
				first.level = 0
			}


			if( is.numeric(first.level) ){
				first.level = round(first.level)
				if( first.level == 0 ){
		         first.level = sort(levs[ which.max(as.vector(table(variable))) ])[1]
				}else{
					first.level = levs[ max( min(first.level,length(levs)) , 1 ) ]
				}
			}


			if( length(levs) == 2 ){
			   second.level = setdiff(levs,first.level)
			}else{
	         second.level = paste0(first.level,"'")
				variable = ifelse( variable == first.level , first.level , second.level )
			}
			variable = factor(  variable
									, levels  = c( first.level , second.level )
									, ordered = TRUE
								  )

			levs = levels(variable)


		}else{
			warning("Number of levels of 'variable' is 1 thus 'balance' is ignored and split will be made only according to 'nrows' or 'portion'.")
	      variable = NULL
		}

	}

#} ## end of if(!is.null(variable))

## --------------------------------------------------------
## new.names


if( !is.null(new.names)){

	if( !( is.character(new.names) | is.logical(new.names) ) ){
		new.names = as.character(new.names)
		warning("The argument 'new.names' was coerced to character.")
	}

	if(length(new.names)>2){
		warning( "The argument 'new.names' has length > 2.
  Only the first 2 elements of 'new.names' are considered." )
		new.names = new.names[1:2]
	}

	if( length(new.names) < 2 ){
		new.names = NULL
		if(split){
		warning( "The argument 'new.names' must be specified as a vector of length at least 2.
  Otherwise its value is set to default: c(TRUE,FALSE) and if at the same time split = TRUE
  then 'datfram' is splitted in two with the same name as 'datfram' with suffixes \".1\" and \".0\"." )
      }else{
		warning( "The argument 'new.names' must be specified as a vector of length at least 2.
  Otherwise its value is set to default: c(TRUE,FALSE)." )
      }
	}

}

if(is.null(new.names)){
	new.names = c(TRUE,FALSE)
}


if(split & is.logical(new.names)){
	new.names.df = as.numeric(new.names)
	new.names.df = paste( name.df , new.names.df , sep = "." )
}else{
	new.names.df = new.names
}

## ---------------------------------------------------------------------------------------------------------------------
## splitting procedure #################################################################################################
## ---------------------------------------------------------------------------------------------------------------------


for(m in 1:times){

   ###########################################################
   ## main

   fac.list = as.list(levels(variable)) ;  names(fac.list) = levels(variable)

   fac.list = lapply(fac.list,function(x){which(variable == x)})

   fac.list = lapply(fac.list,function(x)as.numeric(sample(as.character(x),min(length(x),nrows))) )

   idx_1 = unlist(fac.list) ; if(sort){ idx_1 = sort(idx_1)}
	idx_0	= setdiff(1:N,idx_1)

   #datfram0 = datfram[ idx_1 , ]

   #datfram0

   ###########################################################
   ## result

   split.factor = 1:N %in% idx_1  ## boolean

   N_1 = sum(split.factor) ;
   N_2 = N - N_1

   rownames.1 = rownames( datfram[ split.factor,] )
   rownames.0 = rownames( datfram[!split.factor,] )

   split.factor = ifelse( split.factor , new.names[1] , new.names[2] )

   if(split){
   	datfram_1 = datfram[idx_1,]
   	datfram_0 = datfram[idx_0,]
   }else{
   	k = m ###; split_k = "split.1"
   	repeat{
          split_k = paste0("split." , k)
      if( split_k %in% names(datfram) ){ k = k+1 }else{ break } }
   	  ###while( split_k %in% names(datfram)){ k = k+1 ; split_k = paste0("split." , k) }

   	if(!is.logical(new.names)){
   		split.factor = factor( split.factor , levels = new.names )
   	}

   	datfram[,split_k] = split.factor
   }

} ## END OF for( m in 1:times ){ --splitting procedure--  }

## ---------------------------------------------------------------------------------------------------------------------
## output ##############################################################################################################
## ---------------------------------------------------------------------------------------------------------------------


## ----------------------------------------------------------------------------
## envir checking

if(!is.null(envir)){
    if(length(envir)>1){
      envir = envir[1]
      warning("Only the first argument of 'envir' was taken.")
   }

   if(!( is.numeric(envir) && envir>=-1 && round(envir)==envir )){
      envir = NULL
      warning("Parameter 'envir' set to NULL as it was none of the -1, 0, 1, ...
   Only data frame is returned.")
   }
}
## ----------------------------------------------------------------------------

if(split){

   if( is.null(envir) ){   ## minimal output -- only the first data frame, no information
      result = datfram_1
   }else{
      if(envir==0){
   		result = list( datfram_1 , datfram_0 , new.names.df , idx_1 , idx_0 , rownames.1 , rownames.0)
   		names(result) = c( "datfram.1" , "datfram.0", "names" , "indices.1" , "indices.0" , "rownames.1" , "rownames.0")
      }else{
         if( envir == -1 ){
      		assign( new.names.df[1] , datfram_1 , pos = parent.frame() )
      		assign( new.names.df[2] , datfram_0 , pos = parent.frame() )
         }else{
      		assign( new.names.df[1] , datfram_1 , pos = envir )
      		assign( new.names.df[2] , datfram_0 , pos = envir )
      		##
      		cat("\n")
      		cat("Data frame \"",name.df,"\" was splitted in two data frames with names \n\"",new.names.df[1],"\" and \"",new.names.df[2]
                  ,"\"\nwhich were returned to environment ",search()[envir],".",sep="")
      		cat("\n\n")
      		##
         }
   		result = list(new.names.df , idx_1 , idx_0 , rownames.1 , rownames.0 )
   		names(result) = c(  "names" ,"indices.1" , "indices.0" , "rownames.1" , "rownames.0")
      }
   }

}else{

   if( is.null(envir) ){   ## minimal output -- only data frame, no information
      result = datfram
   }else{
      if(envir==0){
   		result = list( datfram , name.df , idx_1 , idx_0 , rownames.1 , rownames.0)
   		names(result) = c( "datfram" , "names" , "indices.1" , "indices.0" , "rownames.1" , "rownames.0" )
      }else{
         if( envir == -1 ){
            assign( name.df , datfram, pos =  parent.frame() )
         }else{
      		assign( name.df , datfram , pos = envir )
      		##
      		cat("\n")
      		cat("Data frame \"",name.df,"\" was updated with variable \"",split_k,"\"\nand was returned to environment ",search()[envir],".",sep="")
      		cat("\n\n")
      		##
         }
   		result = list(name.df , idx_1 , idx_0 , rownames.1 , rownames.0)
   		names(result) = c( "names" , "indices.1" , "indices.0" , "rownames.1" , "rownames.0")
      }
   }
}

## ----------------

if( !is.null(envir) ){

  	result$split = split.factor
   result$portion = N_1/N      ##c( "call" = portion.call , "effective" = portion , "final" = N_1/N )
   result$nrows = c("datfram.1" = N_1,"datfram.0" = N-N_1 , "all" = N) # table(split.factor)##

   result$call = sys.call()

   if(!is.null(variable)){
      #result$priority = priority
      ##result$balance_final = balance_final
   	result$variable = variable
      #result$balance = balance.df
   	result$var.table = addmargins( table(variable,split.factor)[,2:1] , quiet=TRUE )
   }
}

## ----------------

return(result)


} ##----END----##

## ---------------------------------------------------------------------------------------------------------------------

## ---------------------------------------------------------------------------------------------------------------------
## EXAMPLES ############################################################################################################
## ---------------------------------------------------------------------------------------------------------------------

## ------------------------------------------------------------------------------------------------
dummy = function(){
## This is dummy function - it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## - in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
## ------------------------------------------------------------------------------------------------


## ------------------------------------------------------------------------------------------------

#datfram0 = data.frame( fac1 = sample( LETTERS[1:5] , 100 , replace = TRUE)
#                     , fac2 = sample( letters[1:4] , 100 , replace = TRUE)
#                     , num1 = sample( 1:5 , 100 , replace = TRUE)
#                     , num2 = sample( 1:100 ) )

datfram = data.frame( f1 = sample(c(rep("A",10),rep("B",7),rep("C",3)))
                     , f2 = sample( letters[1:4] , 20 , replace = TRUE)
                     , num1 = rnorm( 20 )
                     , num2 = sample( 1:5 , 20 , replace = TRUE )
                     )
datfram
table(datfram$f1)


## ------------------------------------
## 1. Basics

## ----------------
## envir = NULL -- minimal output

## ------
## minimal input -- dafault  nrows = 1
split.rand.nrows( datfram ,"f1") ## whole data frame returned with split variable -- split = FALSE
split.rand.nrows( datfram ,"f1" , split = FALSE)  ## the same result, split = FALSE is default

split.rand.nrows( datfram ,"f1" , split = TRUE)   ## only data frame consisting of records drawn from 'datfram'
## remnants/leftovers are not returned unless you set 'envir' properly

## ------
## other values of nrows
split.rand.nrows(datfram,"f1",2,split=FALSE)
split.rand.nrows(datfram,"f1",2,split=TRUE)
split.rand.nrows(datfram,"f1",2)     ## split = FALSE is defalut

datfram11 = split.rand.nrows(datfram,"f1",5)
table(datfram11$fac1,datfram11$split.1)

datfram11 = split.rand.nrows(datfram,"f1",8)
table(datfram11$fac1,datfram11$split.1)

## ----------------
## envir = 0 -- to the list

split.rand.nrows( datfram ,"f1" , envir = 0)        ## data frame not splitted but with additional split variable
split.rand.nrows( datfram ,"f1" , envir = 0 , split = TRUE)   ## two data frames -- 'datfram' splitted in two
   ## the first data frame are records drawn, the second data frame are records left

split.rand.nrows(datfram,"f1",2 , envir = 0)
split.rand.nrows(datfram,"f1",2,split=TRUE , envir = 0)

## Notice that you obtain the table at once

## ----------------
## envir = 1 -- to the .GlobalEnv
split.rand.nrows( datfram ,"f1" , envir = 1)        ## data frame not splitted but with additional split variable
   datfram
## do it again
split.rand.nrows( datfram ,"f1" , envir = 1)
   datfram
      datfram$split.1 = NULL ; datfram
      datfram$split.2 = NULL ; datfram

split.rand.nrows( datfram ,"f1" , envir = 1 , split = TRUE)   ## two data frames -- 'datfram' splitted in two
   datfram.1 ; datfram.0
## do it again
split.rand.nrows( datfram ,"f1" , envir = 1 , split = TRUE)
   datfram.1 ; datfram.0
   rm("datfram.1","datfram.0")

## There is also option envir = -1 -- returning to the parent frame (environment from which function was called).
## More on envir[onments] below in section 3.

## --------------------------
## splitting many times
## in a loop
for(k in 1:3)split.rand.nrows( datfram ,"f1" , nrows = 2 , envir = 1 , split = FALSE)
datfram
## statistics
for( k in grep('^split\\.',names(datfram),value=TRUE)){
	print(sd(datfram[ datfram[,k] , "num1" ]))
}

## but you may do internal loop (only if split = FALSE)
   ## generate new data before
split.rand.nrows( datfram ,"f1" , nrows = 2 , split = FALSE , times = 3)
split.rand.nrows( datfram ,"f1" , nrows = 2 , split = FALSE , times = 3 , envir = 0)
   ## Notice that a resulting list of statistics concerns only the last split.
split.rand.nrows( datfram ,"f1" , nrows = 2 , split = FALSE , times = 3 , envir = 0 , variable = "f1" , first.level = "b")

## split variables do not overwrite already present variables -- it is safe
   ( datfram = split.rand.nrows( datfram ,"f1" , nrows = 2 , split = FALSE , times = 3) )
   datfram[,c("split.1")] = NULL ;    datfram[,c("split.2")] = NULL
   datfram
   ## thus we have 'datfram' with variable split.3 already in it
split.rand.nrows( datfram ,"f1" , nrows = 2 , split = FALSE , times = 3)  ## new split variables are "split.1","split.2","split.4"

## internal loop and statistics
   ## generate new data before
split.rand.nrows( datfram ,"f1" , nrows = 2 , envir = 1 , split = FALSE , times = 10)
   ## REMEMBER that informations from the list concern only the last split
datfram
## statistics
for( k in grep('^split\\.',names(datfram),value=TRUE)){
	print(sd(datfram[ datfram[,k] , "num1" ]))
}
## internal loop is faster then external -- many conditions are checked only once


   for( k in grep('^split\\.',names(datfram),value=TRUE)){	datfram[ , k ] = NULL }  ; datfram


## if split = TRUE 'times' doesn't work (how could it?)
split.rand.nrows( datfram ,"f1" , nrows = 2 , split = TRUE ,  times = 3)

## ----------------
## new.names of data frames or levels of split factor
##
split.rand.nrows( datfram ,"f1" , nrows = 2 , new.names = c("df_1","df_2"))
split.rand.nrows( datfram ,"f1" , nrows = 2 , new.names = c("df_1"))      ## not enough arguments to new.names
##
split.rand.nrows( datfram ,"f1" , nrows = 2 , new.names = c("df_1","df_2") , envir = 0)
split.rand.nrows( datfram ,"f1" , nrows = 2 , new.names = c("df_1","df_2") , envir = 1)
   datfram
   datfram[,c("split.1")] = NULL
##
split.rand.nrows( datfram ,"f1" , nrows = 2 , split = TRUE , new.names = c("df_1","df_2")) ## new.names irrelevant when envir = NULL
split.rand.nrows( datfram ,"f1" , nrows = 2 , split = TRUE , new.names = c("df_1","df_2") , envir = 0) ## list entries have fixed names
                                                                                    ## but there is a 'names' entry
split.rand.nrows( datfram ,"f1" , nrows = 2 , split = TRUE , new.names = c("df_1","df_2") , envir = 1)
   df_1 ; df_2
   rm("df_1","df_2")

##
split.rand.nrows( datfram ,"f1" , nrows = 2 , split = TRUE , new.names = c("df_1"), envir = 0)  ## not enough arguments to new.names
##

## ------------------------------------
## 2. Splitting parameters

datfram

## ----------------
## first.level -- if you are interested only in one particular level of 'variable'

## try each option few times
##
split.rand.nrows( datfram ,"f1" , nrows = 2 , first.level = "A" , envir = 0 , split = TRUE )
split.rand.nrows( datfram ,"f1" , nrows = 2 , first.level = "B" , envir = 0 , split = TRUE )

## if first.level = 0  then the first.level is set to the most common in the factor
split.rand.nrows( datfram ,"f1" , nrows = 2 , first.level = 0 , envir = 0 , split = TRUE )

## you may also use numeric position of the level
split.rand.nrows( datfram ,"f1" , nrows = 2 , first.level = 2 , envir = 0 , split = TRUE )


## ----------------
## More variables
## When using many variables, say variable = c("f1","f2"), then the factor of its interactions is created : fac1*fac2
split.rand.nrows( datfram , variable = c("f1","f2") , envir = 0 , split = TRUE)
split.rand.nrows( datfram , variable = c(1,2) , envir = 0 , split = TRUE)
split.rand.nrows( datfram , variable = c("f1","f2") , first.level = 0 , envir = 0 , split = TRUE)
## be careful with levels
split.rand.nrows( datfram , variable = c("f1","f2") , first.level = "A" , envir = 0 , split = TRUE)
                                                ## there's no level "A" in f1*f7
split.rand.nrows( datfram , variable = c("f1","f2") , first.level = "A:a" , envir = 0 , split = TRUE) ## OK

## factor*integer
split.rand.nrows( datfram , variable = c("f1","num2") , envir = 0 , split = TRUE)


## ----------------
## more variables from environment
f1 = datfram$f1 ; num2 = datfram$num2
   ## this will not work
split.rand.nrows( datfram , variable = c(f1,num2) , envir = 0 , split = TRUE)
   ## strange result appeard, because concatenating of factors gives numeric vector
   c(f1,num2)
   ## while split.rand.nrows() reads it as position of variables in data frame... a mess is inevitable
## you need
variable = as.factor(paste(f1,num2,sep=":" ))
split.rand.nrows( datfram , variable = variable , envir = 0 , split = TRUE)  ## OK !!!
   class(variable)
## Notice that
variable = paste(f1,num2,sep=":" )   ## IS NOT ENOUGH:
split.rand.nrows( datfram , variable = variable , priority ="nrows" , envir = 0 , split = TRUE) ## error because
   class(variable)
   ## character strings are interpreted as names of variables in 'datfram'


## ------------------------------------
## 3. More on envir[onments].

   datfram = data.frame( f1 = sample(c(rep("A",10),rep("B",7),rep("C",3)))
                        , f2 = sample( letters[1:4] , 20 , replace = TRUE)
                        , num1 = rnorm( 20 )
                        , num2 = sample( 1:5 , 20 , replace = TRUE )
                        )
   datfram
   table(datfram$f1)

##
split.rand.nrows( datfram ,"f1" , nrows = 2 , envir = 1)
datfram

split.rand.nrows( datfram ,"f1" , nrows = 2 , envir = 1 , split  = TRUE)
datfram
datfram.1
datfram.0
rm(datfram.1,datfram.0)

split.rand.nrows( datfram ,"f1" , nrows = 2 , envir = 1 , split = FALSE)
datfram
datfram.1   ## does not exist
datfram.0   ## does not exist

## repeat
split.rand.nrows( datfram ,"f1" , nrows = 2 , envir = 1 , split = FALSE)
datfram

## ------
## output may be written to some variable
ll = split.rand.nrows( datfram ,"f1" , nrows = 2 , envir = 1 , split = TRUE)
ll          ## no data frames
datfram     ## unchanged
datfram.1   ## does exist
datfram.0   ## does exist
rm(datfram.1,datfram.0)

ll = split.rand.nrows( datfram ,"f1" , nrows = 2 , envir = 1 , split = FALSE)  ## split = FALSE is default
ll
datfram     ## 'split.X' added
datfram.1   ## does not exist
datfram.0   ## does not exist

## ------
## names of data frames are served
 mydf = datfram
split.rand.nrows( datfram = mydf ,"f1" , nrows = 2 , envir = 1 , split  = TRUE)
 mydf
mydf.1
mydf.0
rm(mydf.1,mydf.0)

ll = split.rand.nrows( datfram = mydf ,"f1" , nrows = 2 , envir = 1 , split  = TRUE)
ll["names"]
 mydf
mydf.1
mydf.0
rm(mydf.1,mydf.0)


## --------------------------
## envir = NULL -- minimal output
## build a new datfram
   datfram = data.frame( f1 = sample(c(rep("A",10),rep("B",7),rep("C",3)))
                        , f2 = sample( letters[1:4] , 20 , replace = TRUE)
                        , num1 = rnorm( 20 )
                        , num2 = sample( 1:5 , 20 , replace = TRUE )
                        )
   datfram
   table(datfram$f1)


split.rand.nrows( datfram ,"f1" , nrows = 2 , envir = NULL , split = FALSE)
   datfram  ## not changed
## envir = NULL is default
split.rand.nrows( datfram ,"f1" , nrows = 2 , split = FALSE)
   datfram  ## not changed
   ## split = FALSE is default
   split.rand.nrows( datfram ,"f1" , nrows = 2 )
   datfram
## if split = TRUE only the first data frame is returned
split.rand.nrows( datfram ,"f1" , nrows = 2 , split = TRUE)
   datfram  ## not changed

## --------------------------
## envir = 0 -- to the list
## build a new datfram

ll = split.rand.nrows( datfram ,"f1" , nrows = 2 , envir = 0 )
ll
datfram
datfram.1   ## does not exist
datfram.0   ## does not exist
ll["datfram"]

ll = split.rand.nrows( datfram ,"f1" , nrows = 2 , split = TRUE , envir = 0 )
ll
datfram     ## unchanged
datfram.1   ## does not exist
datfram.0   ## does not exist
ll["datfram.1"]
ll["datfram.0"]

## other names of data frame
 mydf = datfram
ll = split.rand.nrows( datfram = mydf ,"f1" , nrows = 2 , split = TRUE , envir = 0 )
ll
ll["names"]
 mydf     ## unchanged
mydf.1   ## does not exist
mydf.0   ## does not exist
ll["names"]
ll["datfram.1"]
ll["datfram.0"]
ll["names"]

## --------------------------
## envir = -1  --  returning resulting data frames to the parent frame -- environment from which the functin was called
##   see ?parent.frame

## ----------------
## parent.frame -> .GlobalEnv
split.rand.nrows( datfram , "f1" , nrows = 2 , envir = -1 , split = FALSE)
   datfram  ## changed

## if split = TRUE
split.rand.nrows( datfram , "f1" , nrows = 2 , envir = -1 , split = TRUE)
   datfram  ## not changed
   datfram.1
   datfram.0
   rm("datfram.1","datfram.0")

## Notice lack of any message about new variables or data frames

## ----------------
## But the reason for this option is returning results to the function calling split.rand.nrows()

ff = function(df){
   ll = split.rand.nrows(datfram = df ,"f1" , nrows = 2 , split = TRUE , envir = -1)
   vv = c(nrow(df.1),nrow(df.0))
   names(vv) = ll$names
   vv
}

ff(datfram)
##!!! Notice that INTERNAL NAME of data frame is used i.e. "df" not "datfram".
## Moreover, df.1, df.0 do not exist in .GlobalEnv
df.1   ## does not exist
df.0   ## does not exist

## ------
## envir = 1 will leave results in .GlobalEnv
gg = function(df){
   ll = split.rand.nrows(datfram = df ,"f1" , nrows = 2 , split = TRUE , envir = 1)
   vv = c(nrow(df.1),nrow(df.0))
   names(vv) = ll$names
   vv
}

gg(datfram)
df.1
df.0
rm("df.1","df.0")

## ------
## returning records not drawn, datfram.0, rather then drawn, datfram.1
hh = function(df,variable,nrows){
   split.rand.nrows(datfram = df , variable , nrows = nrows  , split = TRUE , envir = -1)
   df.0
}

hh(datfram,"f1",2)

## -------------------------------------------------------
## END OF EXAMPLES
## -------------------------------------------------------

}
