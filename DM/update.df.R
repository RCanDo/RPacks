## ---------------------------------------------------------------------------------------------------------------------•°
## FUNCTIONS HERE
##  update.df()
##
## DEPENDENCIES
##  addid.R
##   names2id() with alias   addid2df()
##   var2vec()  with alias   addid2vec()
## ---------------------------------------------------------------------------------------------------------------------•°

## ---------------------------------------------------------------------------------------------------------------------•°
update.df = function(  datfram      ## data frame
							, data                ## vector or data frame to be used to update 'datfram'
							, id = NULL           ## which variable of 'datfram' will be used as an ID variable
                                    ## if NULL or 0 then rownames(datfram) is used as ID variable
                     , variable = NULL     ## which variables of 'data' to use to update 'datfram';
                                    ## as numeric position or names;
                                    ## if 'data' is a vector the 'variable' value will be used as a name of the variable
                                    ## created from this vector
							, action = "append"   ## "replace" , "update"
							, join = "left"       ## the way of joining 'datfram' with 'variable'; analog of INNER/LEFT/RIGHT/OUTER JOIN in SQL
                                    ## where 'datfram' is always on the LEFT and 'data' is on the RIGHT
                     , na.omit = FALSE     ## matters only if action = "update"; if TRUE then only those values will
                                    ## be updated which are NOT NA in 'data';
                                    ## default is FALSE, thus non-NA values in 'datfram' will be always overwritten
                                    ## with values from 'data' even if there is NA in 'data'.
                     , suffix_start = 1  ## starting value for suffixes added to names of appended variables
                                    ## having the same as some variable in datfram
							){
## ---------------------------------------------------------------------------------------------------------------------•°
## Updating data frame 'datfram' by appending or replacing given variable with care for order of observations
## implied by some key in 'datfram' (its rownames or so called "ID variable")
## and [row]names of 'data' (vector or data frame).
## May be seen as a smart wrapper for
##  merge( datfram , as.data.frame(data) , by = ?? , all.x = ?? , all.y = ??)
## where ?? indicates some "problems" to solve by this function.
## User may indicate any column of 'datfram' (also its rownames) to be the key according to which values of 'datfram' will be ordered
#! while ONLY [row]names(data) may serve as a counterpart key in 'data'.
##
## The key is called here "ID variable" and this phrase also referes to a [row]names of 'data'.
##
## It is up to the user to check if the ID variable has all values unique, i.e. if it is a real identifier;
## however, warning will appear if this is not a case.
##
## Below "non-matching ID values" means records of 'datfram' for which values of an ID variable
## has no counterparts in [row]names of 'data'; it may also mean elements/records of 'data' for which values of [row]names
## has no counterparts in values of ID variable of 'datfram'.
##
##    Arguments
##  datfram             data frame which we need to update, i.e. add a new variable
##                      or replace the values of some columns with the columns of 'data' having the same (indicated) name;
##                      in both cases it is to be done according to an order implied by ID variable indicated by 'id'.
##  data                vector or data frame with values we want to be inserted into 'datfram' according to its [row]names;
##                      if vector is not named then numeric positions of its elements are used to match an ID variable of 'datfram';
##                      'data' may also be a data frame in which case its rownames serve as an ID variable
##                      which will be checked to match an ID variable in 'datfram' (indicated by 'id').
##                      ...
##  id = NULL           which variable of 'datfram' will be used as an ID variable;
##                      if NULL or 0 then rownames(datfram) is used as ID variable.
##  variable            which variables of data frame 'data' to use to update 'datfram';
##                      as numeric position or names; if NULL all variables will be used.
##                      If 'data' is a vector then:
##                         (only first) 'variable' value will be used as a name of the variable
##                            created from this vector;
##                         if 'variable' is numeric then its values are ignored and new name will be created
##                            from 'data' name; the same holds for NULL.
##  action = "append"   what to do when column with one of the names from 'variable' already exists.
##                      Possible values are "append", "update", "replace".
##
##                      If "append" then the values of 'data' is appended to the 'datfram'
##                      as a new column with name "'var_name'_k"
##                      (k is the lowest integer such that "'var_name'_k" is not in the data frame);
##                      this is the safest option.
##
##                      If "replace" then values of 'data' replaces the values of 'variable' column;
##                      NAs will appear for non-matching ID values of 'data' even if 'datfram' has values for these IDs;
##                      thus it is most danger option.
##
##                      "update" is the same as "replace" but if there are non-matching ID values
##                      then original values from 'datfram' are retained for these records.
#!                      However, NAs in 'data' entries for matching ID values will replace values in 'datfram'.
##
##  join = "left"       the way of joining 'datfram' with 'data';
##                      analog of INNER/LEFT/RIGHT/OUTER JOIN in SQL where
##                      'datfram' is always on the LEFT and 'data' is on the RIGHT.
##                      Technically it sets the arguments 'all.x' and 'all.y' of merge() in the following way
##                                  all.x   all.y     SQL
##                       "inner"    FALSE   FALSE     datfram INNER JOIN data
##                       "left"     TRUE    FALSE     datfram LEFT  JOIN data
##                       "right"    FALSE   TRUE      datfram RIGHT JOIN data
##                       "outer"    TRUE    TRUE      datfram OUTER JOIN data
##                      .
##  na.omit = FALSE     matters only if action = "update"; if TRUE then only those values will
##                      be updated which are NOT NA in 'data';
##                      default is FALSE, thus non NA values in 'datfram' will be always overwritten
##                      with values from 'data' even if there is NA in 'data'.
##
##    Value
##  An updated data frame.
##
##    Limitations
##  You cannot update ID variable.
##  If id='name', variable='name' the function will automatically adjust ID name,
##  and id.variable will be appended even if at the same time action="update" or action="replace".
## ---------------------------------------------------------------------------------------------------------------------•°


rm.id = FALSE

## ------------------------------------
##  id
##

if(is.null(id)){
	warning("Parameter 'id' is not specified and the rownames of 'datfram' is taken to be an ID variable.")
	id = 0
}

## ------------------------------------

datfram0.attr.names = names(attributes(datfram))
datfram0.attr = attributes(datfram)

## ------------------------------------
if(is.numeric(id)){
   if(id == 0){ ## we must use rownames as ID variable BUT we need to add it to datfram's columns before

      datfram = names2id(datfram)     ## ID variable (created from rownames) will have a name "ID" or "ID.k" for some k = 1, 2, ...;
      names.datfram = names(datfram)  #!#! all columns together with ID variable
      id.pos = 1                      ## ID variable is always the first one
      id = names.datfram[id.pos]

      rm.id = TRUE    ## remove added ID variable from final data frame

   }else if(! id %in% 1:length(datfram)){

      stop("Value of 'id', wich is ", id, ", is out of bound 1:ncol(datfram).")

   }else{

      names.datfram = names(datfram)  #! all columns together with ID variable
      id.pos = id
      id = names.datfram[id]

   }

}else if(is.character(id)){

   names.datfram = names(datfram)  #! all columns together with ID variable
   if(!any(names.datfram%in%id)){
   	stop("There is no variable in input data frame having name \"", id, "\".")    ##"
   }
   id.pos = which(names.datfram == id)

}else{
   stop("Parameter 'id' should be of class 'character' (name of the column) or 'numeric' (position of the culumn) or left NULL.")
}

#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!
#! names.datfram  has all column names together with ID variable
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!

## ----------------------------------------------
## needed for retrieving order and rownames #####
id.var = datfram[, id]	#<=>#  id.var = var2vec(datfram, id)
rownams = rownames(datfram) ; names(rownams) = id.var		#<=>#	rownams = var2vec(datfram, variable=0, names=id.var)
## ----------------------------------------------
## By the way:
if(any(table(id.var)>1)){warning("Some values in ID variable are not unique! Result is unpredictable!")}


## ----------------------------------------------
##  variable

if(is.vector(data) | is.factor(data)){

  if( is.character(variable) ){
    variable = variable[1]
  }else if( !is.null(variable) ){
    variable = NULL
    warning("If 'data' is a vector then 'variable' indicates a name to be given to variable made of 'data'.
  If 'variable' is not a character string then the name is created from the name of vector.")
  }

  if( is.null(variable) ){
     variable = deparse(substitute(data))  ## used only when data is a vector NOT a data frame
    												# print(variable)
  }

}

## ------------------------------------------------------------------------------------
##  creating data frame from data with ID from (row)names
##
id.old = id ## == names.datfram[id.pos]  ## it is needed to retrieve original id passed to function in case it will change below
##
repeat{ ## negotiating id between two data frames to not interfere with other column names (in both of them!!!)

  suppressWarnings(
  ll <- names2id( data
                    , name.id = id                ## name of ID variable (created from [row]names(data))
                                                  ## BUT adjusted (if necessary) by suffix ".k", k = 1, 2, ...
                                                  ## such that "id.k" does not exist in names(data)
                                                  #! hence it is not certain what it will be!
                    , variable = variable
                    , rownames = FALSE            ## rownames not necessary here; may be a little faster
						  , name.df = "data.df"
						  , envir = -1
                    )
  )
  names.data = names(data.df)       ## the first value is always id -- name of ID variable (by definition of names2id() )
  ##id = names.data[1]
  id = ll$name.id

if( ! id %in% names.datfram[-id.pos] ) break }
##
## new name of id.var must be introduced into datfram (i.e. replace old id remembered in id.old)
names(datfram)[id.pos] = id
names.datfram = names(datfram)
##
                                  #print(id)
                                  #print(names.datfram)
                                  #print(names.data)

## ----------------------------------------------
## type of   datfram XXX JOIN data
if( join == "outer" ){
  all.x = TRUE ; all.y = TRUE
}else if( join == "left" ){
  all.x = TRUE ; all.y = FALSE
}else if( join == "right" ){
  all.x = FALSE ; all.y = TRUE
}else if( join == "inner" ){
  all.x = FALSE ; all.y = FALSE
}else{
  stop("Parameter 'join' must have only one of the following values \"outer\", \"left\", \"right\", \"inner\".")
}

                                    #  print(data.df)
                                    #  print(datfram)
## ------------------------------------------------------------------------------------
j = which( names.datfram %in% names.data )  ## j >= 1 because ID variable is always in both data frames (with the same name)
   											#print(names.datfram[j])
names.common = names.datfram[setdiff(j, id.pos)]  ## without ID -- may be empty
											## names.datfram[j] == c(id, names.common)
                                   # print(j)
                                   # print(names.common)
                                   # print(setdiff(j, id.pos))
classes = as.list(sapply(datfram[, c(id, names.common), drop=FALSE], class))  ## never empty - ID always exists
   #print(classes)
levs = lapply( datfram[, c(id, names.common), drop=FALSE] , levels )
   #print(levs)
## ------------------------------------------------------------------------------------

if(length(j)>1){

	if(action == "append"){

      ## adjusting names in data.df
      v = which( names.data %in% names.datfram )
      for( nxt in v[-1] ){    ## the first names.data is always ID variable ([row]names turned into it)
        namnxt = names.data[nxt]
          k = suffix_start ; namnxt_k = paste( namnxt , suffix_start , sep="." )
  		  	while( namnxt_k %in% names.datfram ){ k = k+1 ; namnxt_k = paste(namnxt , k , sep=".") }
        names.data[nxt] = namnxt_k
      }
      names(data.df) = names.data

      datfram = merge( datfram , data.df , by = id , all.x = all.x , all.y = all.y	)      ##, all.x = TRUE	)

                                 # print(datfram)

	}else if(action == "replace"){

                                # print(datfram[,-setdiff(j, id.pos), drop=FALSE])

		datfram = merge( datfram[,-setdiff(j, id.pos), drop=FALSE] , data.df , by = id , all.x = all.x , all.y = all.y )   ##, all.x = TRUE	)

      names.data = setdiff( names.data , names.datfram[-id.pos] )

	}else if(action == "update"){  ## replace + filling NAs for nonmatching records of datfram

                                # print(datfram[,-setdiff(j, id.pos), drop=FALSE])

      if( join %in% c("left", "outer")){  ## preparation to fill NAs for nonmatching records of

        id.notin.data.df = ! id.var %in% data.df[, id]

        datfram.tofill = datfram[ id.notin.data.df , setdiff(j, id.pos) , drop=FALSE ]   #!#!  ID is here  #!#!

      }

		if(na.omit){
			datfram.old = datfram[, j]
		}

		datfram = merge( datfram[,-setdiff(j, id.pos), drop=FALSE] , data.df , by = id , all.x = all.x , all.y = all.y )   ##, all.x = TRUE	)

      names.data = setdiff( names.data , names.datfram[-id.pos] )                #!#!  ID is here  #!#!

	}else{
		stop("Parameter 'action' must take one of the values: \"append\", \"replace\" or \"update\".")
	}

}else{  ## no common variables thus simple merge() may be applied

		## vec2df() uses numeric postition of element if names(data) is NULL
	datfram = merge( datfram , data.df , by = id , all.x = all.x , all.y = all.y )          ##, all.x = TRUE	)

}

## ------------------------------------------------------------------------------------

                                  # print(id)
                                  # print(id.old)


## retrieving original order and rownames of 'datfram' with new records (if they appear) at the end
## 1.
rownames(datfram) = datfram[, id]    ## ID variable may have new elements
                                    ## if some names(data) do not match ID variable values from original 'datfram' and
                                    ## all = TRUE (default)  in  merge(...)
## 2.
  ## df.old
id.in  = id.var %in% datfram[, id]
id.var.in = as.character(id.var[id.in])
df.old  = datfram[ id.var.in , ]        ## order of the first argument is preserved
  rownames(df.old) = rownams[ id.in ]

                                  ## print(j)

if( action == "update" && join %in% c("left", "outer") && length(j)>1 ){ #){  ## filling NAs for nonmatching records
	for(nam_k in names(datfram.tofill)){
    values_k = datfram.tofill[, nam_k]
        #print(values_k)
    if(is.factor(values_k)){
      df.old[ , nam_k ] <- as.character( df.old[ , nam_k ] )
      df.old[ id.notin.data.df , nam_k ] <- as.character(values_k)
    #  suppressWarnings( df.old[ id.notin.data.df , nam_k ] <- as.character(values_k) )  ## maybe not elegant but it's enough
    }else{
      df.old[ id.notin.data.df , nam_k ] = values_k
    }
  }
}

## df.new
df.new  = datfram[ as.character( intersect( setdiff( data.df[, id], id.var ) , datfram[, id] ) ), ]
                          ## rownames of df.new are equal to these names(data) which didn't match ID variable from 'datfram'
                          ## if however rownames(datfram) were not used as ID variable then some values may repeat but should be
                          ## automatically adjusted by rbind() by adding some suffix ???
## 3.
datfram = rbind(df.old, df.new)

## 4.
    # print(datfram)
    # print(names.datfram.final); cat("\n")
    # print(names(datfram))

## -----------------------------------------------------------------------------------
## cleaning and ordering
##
names.data = names.data[-1] ## the first of names.data is always ID variable made of rownames(data)

if(rm.id){    ##  iff  id == 0  i.e. when using rownames(datfram)
  names.datfram = names.datfram[-1]  ## -id.pos  in general, but it's always 1 in this case
}else{
  names.data[which( names.data==id.old )] = id  ## recreating ID name passed to function
  names.datfram[id.pos] = id.old                   ## recreating ID name passed to function

  names(datfram)[which( names(datfram)==id.old )] = id
  names(datfram)[which( names(datfram)==id )[1] ] = id.old
}

names.datfram.final = c(names.datfram, names.data)

datfram = datfram[, names.datfram.final]  ## now all names of datfram are retrieved to original in the original order

## -----------------------------------------------------------------------------------
## if "update" and na.omit = TRUE -- refilling NAs from 'data' when they've overwritten proper values in 'datfram'
##

		#	print( action == "update" && na.omit && length(names.common)>0  )

if( action == "update" && na.omit && length(names.common)>0 ){

   for( nam_k in names.common ){

         #print(nam_k)

      if(rm.id){    ##  iff  id == 0
        idx_k = ( rownames(datfram) %in% id.var.in )  &  is.na(datfram[, nam_k])
      }else{
        idx_k = ( datfram[, id.old] %in% id.var.in )  &  is.na(datfram[, nam_k])
      }

        #print(idx_k)

      rownams_k = rownames(datfram)[idx_k]
      if(is.factor(datfram.old[, nam_k])){
         datfram[, nam_k] = as.character(datfram[, nam_k])
         datfram[rownams_k, nam_k] = as.character(datfram.old[rownams_k, nam_k])
      }else{
         datfram[rownams_k, nam_k] = datfram.old[rownams_k, nam_k]
      }

   }

}
## -----------------------------------------------------------------------------------
## retrieveing classes, especially factors
##
## print(names(classes)) ;  print(names(datfram)) ;   print(intersect(names(classes), names(datfram)))

for(nam_k in intersect(names(classes), names(datfram))){
   ##  print(nam_k)
   class_k = classes[[nam_k]]
   if("factor" %in% class_k){
         uniq_k = unique(datfram[, nam_k]) ; uniq_k = uniq_k[!is.na(uniq_k)]
         datfram[, nam_k] = factor( datfram[, nam_k] , levels  = union( levs[[nam_k]] , uniq_k )
                                                   , ordered = "ordered" %in% class_k  )
   } ## else if("character" %in% class_k){ } ## not necessary
}

## -----------------------------------------------------------------------------------
## retrieveing attributes of datfram

attributes(datfram) = c( attributes(datfram)
                       , datfram0.attr[setdiff(datfram0.attr.names, names(attributes(datfram)))]  )

class(datfram) <- union( class(datfram) , datfram0.attr[["class"]] ) 

## -----------------------------------------------------------------------------------
## output
##

datfram

}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
## EXAMPLES ############################################################################################################—•°
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
dummy = function(){
## This is dummy function - it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## - in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
## -------------------------------------------------------------------------------------------------—•°
 ## RELOADER - before it works you need to source("RCanDo.R"); it's best to use DM.R within pack's dir.
 loadPacksAK("DM")
## -------------------------------------------------------------------------------------------------—•°

df1 = data.frame( aa = sample(1:6)
                , bb = 4:9
                , cc = sample(4:9)+10
                , dd = sample(letters[1:6])
                , ee = sample(LETTERS[1:6], 6, replace = TRUE)
                )
df1
   sapply(df1, class)

( v1 = sample(1:6)+100 )
update.df(df1, v1)           ## rownames(df1) used as ID variable, and order of v1 preserved because
                            ## in df1 and v1 an order of elements is an identifier
   sapply(update.df(df1, v1), class)
update.df(df1, v1, variable="variable") ## you may set a name for a variable different from a vector name
update.df(df1, v1, id="aa")  ## now order of v1 is conformed to "aa" variable in df1


v2 = v1; names(v2) = sample(1:6)  ## ordering different from names
  v2
merge(df1, v2, by=0) ## this is how merge() works : joining according to [row]names of both objects, that's OK, BUT:
                   ## variable has automatic name, rownames are not inherited but added as a variable Row.names
                   ## order of df1 not preserved and new one given (but it is not clear what order) -- see example below
merge(df1, v1, by=0) ## works too with element's positions as names(v1) (v1 has no names)
update.df(df1, v2)  ## name of variable preserved, rownames(df2) preserved and are not added to data frame
update.df(df1, v2, id="aa")

## different types of JOINS (using rownames and names)
update.df(df1, v2, join="left")  ## default is join = "left"  (datfram OUTER JOIN data)
## all other joins will give the same result ( set of [row]names is the same for both objects )
update.df(df1, v2, join="right")
update.df(df1, v2, join="outer")
update.df(df1, v2, join="inner")

## -------------------------------------------

df2 = df1; rownames(df2) = sample(101:106)
  df2 ; v2
merge(df2, v2, by=0) ## empty data.frame
merge(df2, v2, by.x="aa") ## ERROR! there's no "aa" in v2
merge(df2, v2, by.x="aa", by.y=0) ## this is what you need using merge() ;
                               ## however, notice the order of rows and what the rownames are...
                               ## it is not what you may expect and may be really annoying
update.df(df2, v2, id="aa")  ## this works!  only (always) [row]names of v2 are considered -- this makes things sometimes easier
									## rownames of the df2 preserved and rows' order too
   sapply(update.df(df2, v2, id="aa"), class)

## -------------------------------------------
## no common names(v2) with rownames(df2)
   df2 ; v2
update.df(df2, v2)    ## it's OK
   sapply(update.df(df2, v2), class)
   	merge(df2, v2) ## == CROSS JOIN
		merge(df2, v2, by=0) ## = INNER JOIN
   merge(df2, v2, by=0, all.x=TRUE, all.y=FALSE) ## = LEFT JOIN ;
                                    ## OK, but rownames changed while the old one bacame a new variable...
                                    ## records' order has changed too

update.df(df2, v2, join="left")   ## default is join = "left"  (datfram LEFT JOIN data)
	merge(df2, v2, by=0, all.x=TRUE, all.y=FALSE)  ## but rownames from v2 which is y in resulting table
update.df(df2, v2, join="inner")  ## no common values of ID variables ( (row)names in this case)
	merge(df2, v2, by=0, all.x=FALSE, all.y=FALSE) ## dafault for merge()
update.df(df2, v2, join="outer")  ##  "
	merge(df2, v2, by=0, all.x=TRUE, all.y=TRUE)  ## notice the rownames
      sapply(update.df(df2, v2, join="outer"), class)  ## OK
update.df(df2, v2, join="right")  ##  "
	merge(df2, v2, by=0, all.x=FALSE, all.y=TRUE) ##
      sapply(update.df(df2, v2, join="right"), class)  ## OK


## -------------------------------------------
df1 ; v2
## no common entries of names(v2) with df1$cc
##
#!#!#!#!#! NOT GOOD :
update.df(df1, v2, id="cc", join="outer")              ## == OUTER JOIN
#!#!#! This is some problem: cc got names(v2) for records obtained from v2
## while rownames for this records are not equal to names(v2)
##  (1 was appended to them as suffix because rownames must be unique
##  and otherwise they would be the same as rownames for records of df1).
      sapply(update.df(df1, v2, join="outer"), class)  ## OK
##
update.df(df1, v2, id="cc", join="inner") ## == INNER JOIN
update.df(df1, v2, id="cc", join="left")  ## == LEFT JOIN
#!#!#! rownames not changed BUT cc column with values inherited from names(v2)
update.df(df1, v2, id="cc", join="right") ## == RIGHT JOIN
  ## cc got values but it rather should not.
#!#!#!#!#!#!#!#!#!#!#! How to mend it? Is it really so bad? Maybe there is nothing to do with it?
      sapply(update.df(df1, v2, join="right"), class)  ## OK

  ## by the way
  rbind(df2, df2, df2)    ## notice how rownames changes
  rbind(df2, rbind(df2, df2))

## -------------------------------------------
## ID as letters
##
df3 = df2 ; rownames(df3) = sample(letters[11:16])
  df3
  rbind(df3, df2)   ## notice the rownames
   sapply(rbind(df3, df2), class)
##
v3 = v2; names(v3) = sample(letters[11:16])
   df3 ; v3
update.df(df3, v3, join="outer") ## == OUTER JOIN
update.df(df3, v3, join="inner") ## == INNER JOIN
update.df(df3, v3, join="left")  ## == LEFT  JOIN
update.df(df3, v3, join="right") ## == RIGHT JOIN
## all the same
      sapply(update.df(df3, v3, join="right"), class)  ## OK


   sapply(df3, class)
#!#!#!#!#! NOT GOOD :
#! the same "problem" as before
(df0 = update.df(df3, v3, id="dd", join="outer") ) ## == OUTER JOIN
   sapply(df0, class) ; lapply(df0, levels)
(df0 = update.df(df3, v3, id="dd", join="inner") ) ## == INNER JOIN
   sapply(df0, class) ; lapply(df0, levels)
(df0 = update.df(df3, v3, id="dd", join="left") ) ## == LEFT  JOIN
   sapply(df0, class) ; lapply(df0, levels)
(df0 = update.df(df3, v3, id="dd", join="right") ) ## == RIGHT JOIN
   sapply(df0, class) ; lapply(df0, levels)
#!#!#!#!#!#!#!#!#!#!#! How to mend it? Maybe there is nothing to do with it?


## mixture
v4 = v3; names(v4) = sample(letters[14:19])
  df3 ; v4
  intersect(rownames(df3), names(v4))  ## 3 common ID values
(df0 = update.df(df3, v4, join = "outer") ) ## == OUTER JOIN ; order of df3 prevails !!!
   sapply(df0, class) ; lapply(df0, levels)
(df0 = update.df(df3, v4, join = "left") )  ## == LEFT JOIN
(df0 = update.df(df3, v4, join = "right") ) ## == RIGHT JOIN
(df0 = update.df(df3, v4, join = "inner") ) ## == INNER JOIN

v5=v1;names(v5) = sample(letters[4:9]) ; v5
  intersect(df3[, "dd"], names(v5))  ## 3 common ID values
(df0 = update.df(df3, v5, id="dd", join = "outer") ) ## == OUTER JOIN ; order of df3 prevails
   sapply(df0, class) ; lapply(df0, levels)
update.df(df3, v5, id="dd", join = "left")   ## == LEFT  JOIN
update.df(df3, v5, id="dd", join = "right")  ## == RIGHT JOIN
update.df(df3, v5, id="dd", join = "inner")  ## == INNER JOIN

## -------------------------------------------
df3 ; v4

update.df(df3, v4)
update.df(df3, v4, variable = "variable")
update.df(df3, v4, variable = "aa")
update.df(df3, v4, variable = "aa", action = "replace") ## we have lost values for records with no match
update.df(df3, v4, variable = "aa", action = "update") ##
update.df(df3, v4, variable = "aa", action = "update", join = "right") ##
update.df(df3, v4, variable = "aa", action = "update", join = "outer") ##
update.df(df3, v4, variable = "aa", action = "update", join = "inner") ##

## -------------------------------------------
## -------------------------------------------
## two data frames
(df0 = update.df(df1, df1))    ## just repeated columns,  join according to rownames
   sapply(df0, class)
update.df(df1, df1, id = "aa")   ## check it carefully : df1 a LEFT JOIN df1 b ON a.aa = b.rownames
  ## joined df1 (this on the right) has the same order as
  df1[df1$aa,]
##
## function is asymetric
df2 ; df1
(df0 = update.df(df2, df1, id = "aa") )  ##
   sapply(df0, class)
(df0 = update.df(df1, df2, id = "aa") )  ## default join is "left" and there is no common df1$aa with rownames(df2)
   sapply(df0, class)
(df0 = update.df(df1, df2, id = "aa", join = "outer") )  ##
   sapply(df0, class)
(df0 = update.df(df1, df2, id = "aa", join = "right") )  ##  "problem" with ID variable for nonmatching records of the second argument
   sapply(df0, class)
(df0 = update.df(df1, df2, id = "aa", join = "inner") )  ##
   sapply(df0, class)

##
df1
  df11 = data.frame( aa = sample(11:16)
                   , bb = sample(-9:-4)
                   , cc = sample(letters[4:9])
                   , dd = sample(4:9)
                   , ee = sample(letters[4:9])
                   )
  df11 ; df1

update.df(df1, df11)
##
intersect(df11$dd, rownames(df1))
update.df(df11, df1, id="dd")       #! warning comes from names2id() used internally -- maybe should be supressed ???
(df0 = update.df(df11, df1, id=4) )         ##  "
   sapply(df0, class)

##
## default action is "append" (the safest one, which do not change any value in a 'datfram', only appends new variables)
update.df( df11 , df1 , id=4 , variable = 1 , action = "append" )  ## only first variable of df1 is used
   #! In case of two data frames parameter 'variable' is not a new name for variable (vector passed two 'data')
   ## but names or numeric positions of variables of 'data' which have to be considered while updating 'datfram'.
   ## Their names will not be changed provided they do not interfere with names of 'datfram' in which case
   ## some number will be appended to them as suffix.
## Now let's try "replace" and "update".
## "update" is probably most useful but changes values of 'datfram' thus is not as safe as append.
update.df( df11 , df1 , id=4 , variable = 1 )  ## default is "append" -- the safest option
update.df( df11 , df1 , id=4 , variable = 1 , action = "replace" ) ## variable in df11 replaced by variable from df1; NA appears for non-matching records
update.df( df11 , df1 , id=4 , variable = 1 , action = "update" )  ## new values appear only for matching records
##
## other joins
update.df( df11 , df1 , id=4 , variable = 1 , action = "update" , join = "outer")  ## new values appear only for matching records
update.df( df11 , df1 , id=4 , variable = 1 , action = "update" , join = "right")  ## new values appear only for matching records
update.df( df11 , df1 , id=4 , variable = 1 , action = "update" , join = "inner")  ## new values appear only for matching records
##
## "replace"  -- danger option -- we completely lose old values
update.df( df11 , df1 , id=4 , variable = 1 , action = "replace" )
update.df( df11 , df1 , id=4 , variable = 1 , action = "replace" , join = "outer")  ## NA appears for non-matching records
update.df( df11 , df1 , id=4 , variable = 1 , action = "replace" , join = "right")  ## NA appears for non-matching records
update.df( df11 , df1 , id=4 , variable = 1 , action = "replace" , join = "inner")  ## NA appears for non-matching records
##
## more variables
df11 ; df1
## "replace"  -- danger option -- we completely lose old values
(df0 = update.df( df11 , df1 , id=4 , variable = 1:3 , action = "update" ) ) #! notice how cc changed !!!
  sapply(df0, class) ; lapply(df0, levels)       #! class of a variable is retained from 'datfram', here 'df11'
##
update.df( df11 , df1 , id=4 , variable = 1:3 , action = "update" , join = "outer")  ## new values appear only for matching records
update.df( df11 , df1 , id=4 , variable = 1:3 , action = "update" , join = "right")  ## new values appear only for matching records
update.df( df11 , df1 , id=4 , variable = 1:3 , action = "update" , join = "inner")  ## new values appear only for matching records

## go back to factors
class(df11$cc) ; as.numeric(df11$cc) ## factor values are stored as integers !!!
df12 = df11 ; df12$cc = as.character(df11$cc)  ## now cc is character variable not a factor
df12 ; sapply(df12, class)
##
(df0 = update.df( df12 , df1 , id=4 , variable = 1:3 , action = "update" ) ) ## notice how cc has changed !!!
  sapply(df0, class) ; lapply(df0, levels)       #! class of a variable is retained from 'datfram', here 'df11'
## now the new values are inserted as characters:
df0$cc   ## new cc is also character

## ---------------
## na.omit
df1 ;   sapply(df1, class) ; lapply(df1, levels)
v5 = c("a", "a", NA, NA, "z", "")
(df0 = update.df(df1, v5) )
   sapply(df0, class) ; lapply(df0, levels)
(df0 = update.df(df1, v5, variable = "dd") )
   sapply(df0, class) ; lapply(df0, levels)
(df0=update.df(df1, v5, variable = "dd", action="update"))
   sapply(df0, class) ; lapply(df0, levels)            ## class of 'dd' is retained from df1
(df0 = update.df(df1, v5, variable = "dd", action="update", na.omit=TRUE) )
   sapply(df0, class) ; lapply(df0, levels)
(df0 = update.df(df1, v5, id="bb", variable = "dd", action="update", na.omit=TRUE) )
   sapply(df0, class) ; lapply(df0, levels)


v6 = as.factor(v5) ; v6
(df0 = update.df(df1, v6) )
   sapply(df0, class) ; lapply(df0, levels)
(df0 = update.df(df1, v6, variable = "dd") )
   sapply(df0, class) ; lapply(df0, levels)
(df0=update.df(df1, v6, variable = "dd", action="update"))
   sapply(df0, class) ; lapply(df0, levels)
(df0 = update.df(df1, v6, variable = "dd", action="update", na.omit=TRUE) )
   sapply(df0, class) ; lapply(df0, levels)
(df0 = update.df(df1, v6, id="bb", variable = "dd", action="update", na.omit=TRUE) )
   sapply(df0, class) ; lapply(df0, levels)

## ------------------------------------------------------------------------------------------
## More on 'variable'
## ---------------

df1

data1 = data.frame( aa = sample(1:6)
                , bb = 4:9
                , cc = sample(4:9)+10
                , pp = sample(1:6)
                , qq = 4:9
                , rr = sample(4:9)+10
                )
data1

update.df(df1, data1)     ## "aa", "bb", "cc" of df1 remained unchanged while "aa", "bb", "cc" of data1 appended whith numeric suffixes
                          ## and "pp", "qq", "rr" appended
update.df(df1, data1, action="update")  ## "aa", "bb", "cc" replaced and "pp", "qq", "rr" appended


## ------------------------------------------------------------------------------------------
## Other problems.
## ---------------

df1 ; df11
## 1. you need to update variable df1$cc with values of df11$bb  according to ID df1$aa
update.df(df1, df11[, "bb"], id="aa", variable="cc", action="update")   ## OK , cc was updated (replaced in fact)

## 2. BUT IF you try replacing ID variable:
update.df(df1, df11[, "bb"], id="aa", variable="aa", action="update")   #!#! ID variable cannot be replaced
## ----------------------------
## when you try to replace ID variable
update.df( df11 , df1 , id=4 , variable = 1:4 , action = "update" )  ## notice how cc has changed !!!
   ## id = 4 is dd and its values were not changed while new variable dd.1 appeared
#!#!#!#!#!#!#!#!#!#!#!
#!  The name of ID variable will be always separated from 'variable's !!!
##  You cannot replace or update values of ID variable (what, in general, is a bad idea)!
#!#!#!#!#!#!#!#!#!#!#!

## 3. reordering one of the variables according to order given by the another
##    e.g. df1$dd according to df1$aa
df1
update.df( df1 , df1[, "dd"] , id="aa" , variable="dd" , action="update" )    ## OK
## BUT   df1$dd according to df1$bb
update.df( df1 , df1[, "dd"] , id="bb" , variable="dd" , action="update" )    ## OK
update.df( df1 , df1[, "dd"] , id="bb" , variable="dd" , action="replace" )   ## OK
update.df( df1 , df1[, "dd"] , id="bb" , variable="dd" , action="replace" , join = "outer")    ## OK

## 4.

## ---------------------------------------------------------------------------------------------------------------------—•°
}; rm(dummy)

