########################################################################################################################•°
## FUNCTIONS HERE
##  split.rand.portion()
##
## DEPENDENCIES
## ...
########################################################################################################################•°
split.rand.portion = function(
		  datfram      			## data frame.
		, portion = .5          ## portion of data to be sampled from the 'datfram' as a first part of it.
		, nrows = NULL     		## number of rows for the first data frame (result of spliting 'datfram');
		##########################
		, variable = NULL       ## factor name (of one in enclosing environment)
										## or vector of character name(s) or numeric position(s) of variables in 'datfram'
										## indicating the factor variable(s) which levels  we want to be all present
                              ## in new data frame with original proportions as closely as possible.
		, first.level = NULL		## if NULL all levels from 'variable' will be present in new data frame with original proportions,
		, balance = NULL        ## balance between first and second level = nr(first.level)/( nr(second.level) + nr(first.level));
		, priority = "balance"  ## which option prevails in case of conflict : "nrows" or "balance";
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
########################################################################################################################•°
## Random spliting data frame in two with constraints on the volume or number of records, balance between some factor levels, etc.
##
##    Arguments
## datfram      	data frame.
## portion = .5   portion of data to be sampled from the 'datfram' as a first part of it.
##						The remnants will be the second data frame in the output.
##						Values should be within [0,1] interval. Negative values will be turned to positive
##						and reciprocal of values greater then 1 is taken.
##						Ignored when 'nrows' is not NULL; notice that 'portion' is internally evalulated to 'nrows' (see below).
## nrows = NULL   number of rows for the first data frame (result of spliting 'datfram');
#!						if not NULL then 'portion' is ignored.
##########################
## variable = NULL    _name_ of a factor or logical variable (of one in enclosing environment)
##						    or vector of character name(s) or numeric position(s) of variables in 'datfram'
##						indicating the factor variable(s) which levels
##						we want to be all present in new data frame with original proportions as closely as possible.
##                Thus, if factor or logical then must have length of nrows(datfram) and consists of values to be processed,
##                while if numeric or character then should be a short vector used as a list of variables (of 'datfram') to be processed.
##
##                If 'priority' = "balance" then it is assumed, that each level must appear at least once,
##                hence in case of rare levels theirs frequency in new data frame may be far from original proportions.
##                This may also result in more records to be chosen then 'portion'/'nrows' says
##                (espacially when there is many rare levels -- the extreme case is when each level appears once --
##                always all records will be chosen then).
##                If 'priority' = "portion" or = "nrows" then number of rows to be drawn is strictly controlled
##                thus some levels may not appear in a first data frame 'datfram.1';
##
##						If 'variable' is NULL then 'first.level' and 'balance' doesn't matter thus are ignored.
##
##						If 'variable' is passed as a character or numeric vector and it has length greater then 1
##						then new factor will be created out of variables indicated in 'variable'
##						where each (existing) unique combination of levels of these factors constitutes a new level of a new factor.
##
##						Only one variable may be passed as a factor name.
##						Notice that only variable within enclosing environments are then searched for
##						and it is not possible to accses variable of 'datfram' via name
##						- it is possible only by character or numeric position.
##						Factor must have length equal to nrows(datfram).
##
## first.level = NULL   if NULL all levels from 'variable' will be present in new data frame with original proportions;
##								if priority = "balance" then additional assumption is that each level will appear at least once
##                what may result with more records then 'portion'/'nrows' implies;
##                if priority = "nrows" or = "portion" then number of records is strictly controlled
##                hence some rare levels may not appear at all;
##                see 'variable' and 'priority' for more details;
##					   'balance' is irrelevant then and is ignored (while priority = "balance" still makes sense as already described).
##
##						If 'first.level' is not NULL then 'variable' is coerced to factor with exactly 2 levels
##						and the value of parameter indicates which level will be preserved (the 'first' one)
##						while all other will be merged into one level (the 'second' level).
##						Indication of the 'first' level may be done in three possible ways, where 'first.level' may take
##							• "levels_name" 	the specified name of the level;
##							• numeric		the level position in the order from list of levels;
##										      values are rounded to integer and "pruned" to the set of {1,...,length(levels(variable))} (if != 0);
##							• 0 			indicating that the most common level is taken to be the first
##								         (if there is more levels with the same number of cases then the first alphabetically is taken);
## balance = NULL   balance between first and second level = nr(first.level)/( nr(second.level) + nr(first.level));
##						  if NULL then attempts to preserve original balance.
##						Similarly to the 'portion': values should be within [0,1] interval;
##						negative values will be turned to positive and reciprocal of values greater then 1 is taken.
##						Irrelevant (ignored) if any of 'variable' or 'first.level' is NULL.
## priority = "balance"   which option prevails in case of conflict : "nrows"/"portion" or "balance";
## 							  matters only when 'variable' is not NULL.
##						If 'nrows' is not specified then 'portion' plays its role as it's translated into relevant value of 'nrows'.
##                Thus "nrows" and "portion" values of 'priority' are effectively synonyms.
##
##                If 'first.level' is specified then demanded balance need to be specified via option 'balance'.
##                It may be in conflict with values of 'portion'/'nrows'.
##                If 'first.level' is not specified then it is assumed that each level of variable will appear at least once
##                if priority = "balance"; otherwise some records may not appear -- see description of 'variable'.
##
## split = FALSE  if split = TRUE then 'datfram' is splitted in two data frames;
##						if FALSE the function adds factor to 'datfram' with two levels
##						indicating splitting group (first or second) to which observation belongs to.
##						Name of this factor is "split.k" where k is the lowest of {1, 2, ...}
##						such that "split_k" is not already present in 'datfram'
##						-- it allows to run splitting many times and write results within 'datfram'.
## envir = NULL   is a numeric position of an environment to which data frame(s) should be returned;
##                is passed to assign() and has the same set of possible values as option 'pos' in assign() plus 0.
##                However the -1 sets environment to parent.frame() i.e. returnes data frame to the environment
##                from which the function was called NOT to the parent environment of the function (executING environment)
##                as in assign() -- this is useful when the function is called within another function.
##
##                If NULL (default) then the function returns only data frame with split variable (if split = FALSE)
##                or a data frame which is a part of 'datfram' for which split variable is TRUE (if split = TRUE).
##                This is minimal possible output.
##
##                If one of -1, 0, 1, 2, ... then the basic output is a list of statistics concerning the split,
##                such as 'names', 'indices.1', 'indices.0', 'portion', etc. (details in the Value section).
##
##                If 0 and split = FALSE then 'datfram' with split variable appended to it is added to the list
##                as its first element.
##                If 0 and split = TRUE then two data frames are added to the list as its first elements,
##                where the first, called 'datfram.1' consists of records randomly drawn from 'datfram'
##                (wrt conditions on random drawing passed to function),
##                while the second, called 'datfram.0' consists of leftovers.
##
##                If -1, 1, 2, ..., then one data frame (if split = FALSE) or two data frames (if split = TRUE)
##                are returned to the environment with 'envir' being its position on the search() list,
##                with exception of -1 which means the parent frame of the call of the function (see parent.frame())
##                what may be useful when the function is called within the body of another function.
##                When one data frame is assigned (if split = FALSE) then it has the same name as 'datfram',
##                i.e. effectively, split variable is appended to 'datfram';
##                When two data frames are assigned (if split = TRUE) then their names are 'new.names' (if properly specified)
##                or "datfram_name".1 (records randomly drawn from 'datfram') and "datfram_name".0 (records left)
##                in case when 'new.names' left NULL or not properly specified.
##
## new.names = NULL  names to be given to new data frames (if split = TRUE)
##							or to factor levels indicating the split (if split = FALSE).
##						If left NULL then is set to default c(TRUE,FALSE) and if at the same time
##						split = TRUE then 'datfram' is splitted in two with names 'datfram' with suffixes ".1" and ".0".
## sort = TRUE    should resulting data frames be sorted wrt to original order of records?
##						This operation may significantly slow down calculations,
##                especially when function is run in a long loop and for large data set.
##						However, it should not be a problem in most cases thus default is TRUE.
## times = 1      only for split = FALSE.
##                How many times splitting procedure will be performed.
##                Results will be written to the splitting variables appended to the data frame.
##                They will get names "split.k" with k a consecutive integers such that "split.k" is not already
##                a name of some 'datfram's variable/column.
##                Notice that a resulting list of statistics will concern only the last split.
##
##    Values
## list & data frame(s) ...
##
##    ToDo
## 1. improve internal loop (option 'times') to ommit all if's ;
##    improve reporting from the loop (now statistics are available only for the last iter.)
## 2. document output
## 3. balance on whole variable as vector of proportions for all levels, eg. we want all levels in equal quantities
## 4. add parameter of the call to the list
########################################################################################################################•°

########################################################################################################################•°
## CHECKing and preparation of arguments values ########################################################################•°
########################################################################################################################•°

###########################################################

##########################
## split
if( ! is.logical(split)  ){
	stop("The argument 'split' must be logical with possible values TRUE or FALSE.")
}

##########################
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

###########################################################
## portion & nrows

if(!is.null(portion)){portion.call = portion}else{ portion.call <- portion <- .5}

if( !is.null(nrows)){

	if( !is.numeric(nrows) ){
		stop("The argument 'nrows' must be of numeric mode.")
	}

	## one of below values may be 0 - no splitting;
	## then formally one of a split product is empty data frame but with specified columns inherited from 'datfram':  dim(datfram.x) -> c(0,Nx); x = 1, 2.
	if( nrows > N ){
		warning("nrows > nrows(datfram). Thus 'nrows' set to nrows(datfram).")
	}
	if( nrows < 0 ){
		warning("nrows < 0. Thus 'nrows' set to 0.")
	}

	N_1 = round( min( max(0,nrows) , N ) )
	N_0 = N - N_1

	portion = N_1/N

}else{

	if( !is.numeric(portion) ){
		stop("The argument 'portion' must be of numeric mode.")
	}else{

		if( portion < 0 ){
			portion = - portion
			warning("Negative value of 'portion'; it was turned to positive.")
		}
	   if( portion > 1 ){
			portion = 1/portion
			warning("Values of 'portion' greater then 1; its reciprocal is taken which is ",portion,".")
		}

		N_1 = round(N*portion)  			## should round() be defered ????
		N_1 = min( max(0,N_1) , N )  		#; print(N_1)
		N_0 = N - N_1                		#; print(N_0)
	}
}


###########################################################
## variable & first.level  preprocessing

if(!is.null(variable)){

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

	}else{
		stop("The 'variable' should be specified as a vector of character names or numeric positions of 'datfram' variables
  or a name of a factor variable (from the enclosing environment - NOT from 'datfram') or left NULL.")
	}

   variable = as.factor(variable) ## is order preserved when variable is already ordered factor ?  YES!!!

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

	if( !is.null(first.level) ){  ## it's OK


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
}

###########################################################
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

########################################################################################################################
## splitting procedure #################################################################################################
########################################################################################################################

if(split){
   times = 1
}else{ ##--checking procedure--
   if(!(is.numeric(times) && times >= 1 && round(times) == times )){
      times = 1
      warning("Parameter 'times' must be integer >= 1. This condition was violated thus it is set to 1.")
   }
}

###################
## indices

for(m in 1:times){

if(is.null(variable)){

	idx_1 = sample(N,N_1) ; if(sort){ idx_1 = sort(idx_1) }
	idx_0 = setdiff(1:N,idx_1)

}else{

	if(is.null(first.level)){
	## 'balance' & 'priority'  ignored
	## each 'variable' level is being preserved with original proportions as closely as possible

		( levs.tab.por = table(variable)*portion )          #!#!#!

      if(priority == "balance"){  ## each level will appear at least once what may result in more records then 'nrows'
         ( levs.sure = pmax(floor(levs.tab.por),1) ) ## may be more then levs.tab.por
         ( levs.rand.prob = pmax(levs.tab.por - levs.sure,0) )
		}else if(priority %in% c("nrows","portion")){  ## rare levels may not appear to meet 'nrows' condition
         ( levs.sure = floor(levs.tab.por) )  ## never more then levs.tab.por
         ( levs.rand.prob = levs.tab.por - levs.sure )
      }

      while(sum(levs.sure) < N_1){
			levs.rand  = as.vector( rmultinom(  1 	## nr of trials
   											, size = 1	## nr of "balls" in each trial
   											, prob = levs.rand.prob  ## probabilities for each "box"
										   ) )
			levs.sure = levs.sure + levs.rand
			levs.rand.prob[as.logical(levs.rand)] = 0
		}

      ###if(priority %in% c("nrows","portion")){ levs.sure = levs.sure[levs.sure>0] }

		## indices for each level:
		idx_1 = integer(0)
		for(lev_k in levels(variable)){      ## k = 3 ; lev_k = levels(variable)[k] ; lev_k
			(idx_k = (1:N)[variable == lev_k])
            ###( idx_k_1 = if(length(idx_k)==1){idx_k}else{sample(idx_k , levs.sure[lev_k])} )
         ( idx_k_1 = as.numeric(sample(as.character(idx_k) , levs.sure[lev_k])) )   ## notice that sample(3,1) == sample(1:3,1)
                                                                                    ## but sample("03",1) --> "3" what we need
			( idx_1 = c(idx_1,idx_k_1) )
		}

      if(sort){ idx_1 = sort(idx_1) }
		idx_0	= setdiff(1:N,idx_1)

	}else{

	   ## it's assumed that variable is factor with only two levels

		idx_a = which( variable ==  first.level )
		idx_b = which( variable == second.level )

		N_a	= length(idx_a)    												#; print(N_a)
		N_b	= length(idx_b)    												#; print(N_b)


	   ##########################
		## balance

		if(is.null(balance)){
			balance  = N_a/N
		}
		if( balance < 0 ){
			balance = -balance
			warning("Negative value of 'balance'; it was turned to positive.")
		}
	   if( balance > 1 ){
			balance = 1/balance
			warning("Values of 'balance' greater then 1; its reciprocal is taken which is ",balance,".")
		}

		N_1_a = round(N_1*balance)   											#; print(N_1_a)
		N_1_b = N_1 - N_1_a          											#; print(N_1_b)


	   ##########################
		## priority

		##########################
		if(priority %in% c("nrows","portion")){

			if( N_1_a > N_a ){
				N_1_a = N_a
				N_1_b = N_1 - N_1_a

				balance = N_1_a/N_1

				warning("The value of 'balance' was in conflict with the value of 'portion' and/or 'nrows'.
  There wasn't enough occurances of the first level of 'variable'.
  The 'priority' is set to \"nrows\" thus the number of rows was retained
   while the value of 'balance' was changed.
  Current value of 'nrows' is ",N_1,".
  Current value of 'portion' is ",round(portion,3),".
  Current value of 'balance' is ",round(balance,3),".")
			}


			if( N_1_b > N_b ){
				N_1_b = N_b
				N_1_a = N_1 - N_1_b

				balance = N_1_a/N_1

				warning("The value of 'balance' was in conflict with the value of 'portion' and/or 'nrows'.
  There wasn't enough occurances of the second level of 'variable'.
  The 'priority' is set to \"nrows\" thus the number of rows was retained
   while the value of 'balance' was changed.
  Current value of 'nrows' is ",N_1,".
  Current value of 'portion' is ",round(portion,3),".
  Current value of 'balance' is ",round(balance,3),".")
			}
		}

		##########################
		if(priority == "balance"){

			if( N_1_a > N_a ){
				N_1 = round( (N_a/N_1_a)*N_1  )                       #; print(N_1)
				N_1_a = N_a                        							#; print(N_1_a)
				N_1_b = N_1 - N_1_a					  							#; print(N_1_b)

				portion = N_1/N                    							#; print(portion)

				warning("The value of 'balance' was in conflict with the value of 'portion' and/or 'nrows'.
  There wasn't enough occurances of the first level of 'variable'.
  The 'priority' is set to \"balance\" thus the number of rows was lessend
   to satisfy the 'balance' restriction.
  Current value of 'nrows' is ",N_1,".
  Current value of 'portion' is ",round(portion,3),".
  Current value of 'balance' is ",round(balance,3),".")
			}

			if( N_1_b > N_b ){
				N_1 = round( (N_b/N_1_b)*N_1  )                       #; print(N_1)
				N_1_b = N_b                        							#; print(N_1_b)
				N_1_a = N_1 - N_1_b					  							#; print(N_1_a)

				portion = N_1/N                    							#; print(portion)

				warning("The value of 'balance' was in conflict with the value of 'portion' and/or 'nrows'.
  There wasn't enough occurances of the second level of 'variable'.
  The 'priority' is set to \"balance\" thus the number of rows was lessend
   to satisfy the 'balance' restriction.
  Current value of 'nrows' is ",N_1,".
  Current value of 'portion' is ",round(portion,3),".
  Current value of 'balance' is ",round(balance,3),".")
			}
		}

		idx_1_a	= sample(idx_a,N_1_a)
		idx_1_b	= sample(idx_b,N_1_b)
		idx_1 	= union(idx_1_a,idx_1_b) ; if(sort){ idx_1 = sort(idx_1) }
		idx_0		= setdiff(1:N,idx_1)

      #######################################
		## only for statistics
		levs.sure = table(variable[c(idx_1_a,idx_1_b)])
      #######################################

	} ## END OF if( is.null( first.level ) ){}else{}


		#######################################
		## statistics
		##
		balance.df = cbind(	balance_start = table(variable)/N      				#!!!
									,	balance_final = levs.sure/N_1		      			#!!!
									,	portion_per_level = levs.sure/table(variable)	#!!!
									,  portion_to_all = levs.sure/N
					 )
		balance.df = rbind( balance.df , "<Sum>" = colSums(balance.df) )
		##
		#######################################

} ## END OF if( is.null( variable ) ){}else{}


###############################################################################
## result

split.factor = 1:N %in% idx_1  ## boolean

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



########################################################################################################################
## output ##############################################################################################################
########################################################################################################################

###############################################################################
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
###############################################################################

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
            final.message = paste("Data frame \""
		             ,name.df,"\" was splitted in two data frames with names \n\"",new.names.df[1],"\" and \"",new.names.df[2]
                  ,"\"\nwhich were returned to environment ",search()[envir],".",sep="")
          	cat("\n")
            cat(final.message)
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

###################

if( !is.null(envir) ){

  	result$split = split.factor
   result$portion = c( "call" = portion.call , "effective" = portion , "final" = N_1/N )
   result$nrows = c("datfram.1" = N_1,"datfram.0" = N-N_1 , "all" = N) # table(split.factor)##
   
   result$call = sys.call()

   if(!is.null(variable)){
      result$priority = priority
      #result$balance_final = balance_final
   	result$variable = variable
      result$balance = balance.df
   	result$var.table = addmargins( table(variable,split.factor)[,2:1] , quiet=TRUE )
   }
}

###################

if(exists("final.message",inherits=FALSE)){

}

return(result)

} ##----END----##

########################################################################################################################
#split.rand.portion = function(...){ vec = df.split.random(...); vec } ## just an alias for var2vec() to retain old name in action
   #!!!!!! may not work properly when using  envir = -1  ????????

########################################################################################################################


########################################################################################################################
## EXAMPLES ############################################################################################################
########################################################################################################################

###################################################################################################
dummy = function(){
## This is dummy function - it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## - in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
###################################################################################################

#######################################
## 0. Data

	N = 20 ## N = 30
	datfram = data.frame(  f1 = factor( sample (letters[1:4], N, replace = TRUE ) )
								, f2 = rnorm(N)
								, f3 = sample(10,N,replace=TRUE)
								, f4 = sample(200,N,replace=TRUE)
								, f5 = 1:N
								, f6 = factor( sample(LETTERS[1:4], N, replace = TRUE) )
								, f7 = gl(2,2,length=N,labels = c("Control", "Treat"))
							  )
	datfram

#######################################
## 1. Basics

#########
## envir = NULL -- minimal output
split.rand.portion( datfram ) ## whole data frame returned with split variable -- split = FALSE
split.rand.portion( datfram , split = FALSE)  ## the same result, split = FALSE is default

split.rand.portion( datfram , split = TRUE)   ## only data frame consisting of records drawn from 'datfram' (1st data frame) is returned
## remnants/leftovers (2nd data frame) are not returned unless you set 'envir' properly

#########
## envir = 0 -- to the list
split.rand.portion( datfram , envir = 0)        ## data frame not splitted but with additional split variable
split.rand.portion( datfram , envir = 0 , split = TRUE)   ## two data frames -- 'datfram' splitted in two
   ## the first data frame are records drawn, the second data frame are records left
   
#########
## envir = 1 -- to the .GlobalEnv
split.rand.portion( datfram , envir = 1)        ## data frame not splitted but with additional split variable
   datfram
      datfram$split.1 = NULL ; datfram

split.rand.portion( datfram , envir = 1 , split = TRUE)   ## two data frames -- 'datfram' splitted in two
   datfram.1 ; datfram.0
   rm("datfram.1","datfram.0")
   
## There is also option envir = -1 -- returning to the parent frame (environment from which function was called).
## More on envir[onments] below in section 3.

###################
## splitting many times
## in a loop
for(k in 1:3)split.rand.portion( datfram , envir = 1 , split = FALSE)
datfram
## statistics
for( k in grep('^split\\.',names(datfram),value=TRUE)){
	print(sd(datfram[ datfram[,k] , "f2" ]))
}

## but you may do internal loop (only if split = FALSE)
   ## generate new data before
split.rand.portion( datfram , split = FALSE , times = 3)
split.rand.portion( datfram , split = FALSE , times = 3 , envir = 0)
split.rand.portion( datfram , split = FALSE , times = 3 , envir = 0 , variable = "f1")
   ## Notice that a resulting list of statistics will concern only the last split.
split.rand.portion( datfram , split = FALSE , times = 3 , envir = 0 , variable = "f1" , first.level = "b")

## split variables do not overwrite already present variables -- it is safe
   ( datfram = split.rand.portion( datfram , split = FALSE , times = 3) )
   datfram[,c("split.1")] = NULL ;    datfram[,c("split.2")] = NULL
   datfram
   ## thus we have 'datfram' with variable split.3 already in it
split.rand.portion( datfram , split = FALSE , times = 3)  ## new split variables are "split.1","split.2","split.4"
   
## internal loop and statistics
   ## generate new data before
split.rand.portion( datfram , envir = 1 , split = FALSE , times = 10)
   ## REMEMBER that informations from the list concern only the last split
datfram
## statistics
for( k in grep('^split\\.',names(datfram),value=TRUE)){
	print(sd(datfram[ datfram[,k] , "f2" ]))
}
## internal loop is faster then external -- many conditions are checked only once

   
## if split = TRUE 'times' doesn't work (how could it?)
split.rand.portion( datfram , split = TRUE ,  times = 3)

###################
## new.names of data frames or levels of split factor
##
split.rand.portion( datfram , new.names = c("df_1","df_2"))
split.rand.portion( datfram , new.names = c("df_1"))      ## not enough arguments to new.names
##
split.rand.portion( datfram , new.names = c("df_1","df_2") , envir = 0)
split.rand.portion( datfram , new.names = c("df_1","df_2") , envir = 1)
   datfram
##
split.rand.portion( datfram , split = TRUE , new.names = c("df_1","df_2")) ## new.names irrelevant when envir = NULL
split.rand.portion( datfram , split = TRUE , new.names = c("df_1","df_2") , envir = 0) ## list entries have fixed names
                                                                                    ## but there is a 'names' entry
split.rand.portion( datfram , split = TRUE , new.names = c("df_1","df_2") , envir = 1)
   df_1 ; df_2
   rm("df_1","df_2")

##
split.rand.portion( datfram , split = TRUE , new.names = c("df_1"), envir = 0)  ## not enough arguments to new.names
##

#######################################
## 2. Splitting parameters

## create a new data at section 0. with N=30
   table(datfram$f1)
   table(datfram$f6)
   table(datfram$f7)

###################
## portion, nrows

## 'portion' -- portion of data to be selected
split.rand.portion( datfram , envir = 0)
split.rand.portion( datfram , envir = 0 , portion =.5) ## portion = .5 is default
split.rand.portion( datfram , envir = 0 , portion =.3)
split.rand.portion( datfram , envir = 0 , portion =.31415)
## you may use 'nrows' instead -- number of rows to be selected
split.rand.portion( datfram , envir = 0 , nrows = 10 , split = TRUE)
## 'nrows' prevails
split.rand.portion( datfram , envir = 0 , portion =.1 , nrows = 10 , split = TRUE)

###################
## Getting variable (factor) of which all levels should be in equal proportions in datfram.1 (rows randomly chosen)
split.rand.portion( datfram , variable = "f1" )  ## f1 taken from datfram ; no statistics, thus result not clear
## use envir = 0 to get more informations
split.rand.portion( datfram , envir = 0, variable = "f1" )  ## f1 taken from datfram
split.rand.portion( datfram , envir = 0, variable = "f1"  , portion = .5)  ## portion = .5 is default
## change the portion value
split.rand.portion( datfram , envir = 0, variable = "f1"  , portion = .3)  ##
split.rand.portion( datfram , envir = 0, variable = "f1"  , portion = .234)  ##

## more on getting variable
split.rand.portion( datfram , envir = 0, variable = 1 )     ## numeric position in datfram
## but you cannot get the variable from 'datfram' via its name
split.rand.portion( datfram , envir = 0, variable = f1 )
## this will work only for variables in environment
   f1 = datfram$f7    ## read it carefully !!  f1 in environment is equal to f7 in 'datfram'
   f1
split.rand.portion( datfram , envir = 0, variable = f1 )  ## f1 taken form .GlobalEnv

f1 = f1[1:20]  ## shorter then nrows(datfram)
split.rand.portion( datfram , variable = f1 )  ## f1 taken form .GlobalEnv;
   ## error:   Number of elements of 'variable' differs from nrows(datfram).
   rm(f1)
   
###################
## balance & first.level -- if you are interested only in preserving proportion of one particular level of 'variable'

	N = 20 ## N = 30
	datfram = data.frame(  f1 = factor( c(rep("a",6) , rep("b",10) , rep("c",4))  )
								, f2 = rnorm(N)
								, f3 = sample(10,N,replace=TRUE)
								, f4 = sample(200,N,replace=TRUE)
								, f5 = 1:N
								, f6 = factor( sample(LETTERS[1:4], N, replace = TRUE) )
								, f7 = gl(2,2,length=N,labels = c("Control", "Treat"))
							  )
	datfram

   table(datfram$f1)

## try each option few times
##
## balance not specified thus preserved from original proportions of levels { b , b'}
split.rand.portion( datfram , variable = "f1" , first.level = "b" , envir = 0 , split = TRUE )
split.rand.portion( datfram , variable = "f1" , first.level = "a" , envir = 0 , split = TRUE )

## 'portion = .5 (deafault) --> 10 records ; balance = .25 --> 10*.25 = 2.5 records --
## BUT portion / nrows prevails thus balance need to be corrected to give an integer number of rows:
## round(nrows*balance) is used
split.rand.portion( datfram , variable = "f1" , first.level = "b" , balance = .25 , envir = 0 , split = TRUE )
split.rand.portion( datfram , variable = "f1" , first.level = "b" , balance = .25000001, envir = 0 , split = TRUE )

## if first.level = 0  then the first.level is set to the most common in the factor
split.rand.portion( datfram , variable = "f1" , first.level = 0 , balance = .3, envir = 0 , split = TRUE )

## you may also use numeric position of the level
split.rand.portion( datfram , variable = "f1" , first.level = 3 , balance = .3 , envir = 0 , split = TRUE )

###################
## conflict betweeen 'portion'/'nrows' and 'balance' -- 'priority' parameter
## balance set automatically to the original proportions of levels
split.rand.portion( datfram , variable = "f1" , first.level = "a" , envir = 0 , split = TRUE)
split.rand.portion( datfram , variable = "f1" , first.level = "a" , balance = .3 , envir = 0 , split = TRUE )   ## the same
## first.level = "a" ; portion = .5 --> 10 records ; balance = .7 --> 10*.7 = 7 records of "a"
## but there are only 6 of all
split.rand.portion( datfram , variable = "f1" , first.level = "a" , balance = .7 , envir = 0 , split = TRUE )
## priority = "balance" is default
split.rand.portion( datfram , variable = "f1" , first.level = "a" , balance = .7 , priority = "balance" , envir = 0 , split = TRUE )
split.rand.portion( datfram , variable = "f1" , first.level = "a" , balance = .7 , priority = "nrows" , envir = 0 , split = TRUE )
## Remember that 'priority' = "portion" and  = "nrows" means exactly the same
split.rand.portion( datfram , variable = "f1" , first.level = "a" , balance = .7 , priority = "portion" , envir = 0 , split = TRUE )

## another example
## nrows = 16 ; first.level = 0 --> level "b" (the most common), 10 of all ; balance = .25 --> 4 of "b" --> 12 of b'
## but there are only 10 b'
split.rand.portion( datfram , variable = "f1" , first.level = 0 , balance = .25 , nrows = 16 , envir = 0 , split = TRUE )
split.rand.portion( datfram , variable = "f1" , first.level = 0 , balance = .25 , nrows = 16 , priority = "nrows" , envir = 0 , split = TRUE )

###################
## More variables
## When using many variables, say variable = c("f1","f7"), then the factor of its interactions is created : f1*f7
split.rand.portion( datfram , variable = c("f1","f7") , envir = 0 , split = TRUE)
split.rand.portion( datfram , variable = c(1,7) , envir = 0 , split = TRUE)
split.rand.portion( datfram , variable = c("f1","f7") , first.level = 0 , envir = 0 , split = TRUE)
## be careful with levels
split.rand.portion( datfram , variable = c("f1","f7") , first.level = "a" , envir = 0 , split = TRUE)
                                                ## there's no level "a" in f1*f7
split.rand.portion( datfram , variable = c("f1","f7") , first.level = "a:Treat" , envir = 0 , split = TRUE) ## OK

## factor*integer
split.rand.portion( datfram , variable = c("f1","f3") , envir = 0 , split = TRUE)

###################
## for larger data frame
	N = 50   ## N = 20
	datfram = data.frame(  f1 = factor( sample (letters[1:4], N, replace = TRUE ) )
								, f2 = rnorm(N)
								, f3 = sample(10,N,replace=TRUE)
								, f4 = sample(200,N,replace=TRUE)
								, f5 = 1:N
								, f6 = factor( sample(LETTERS[1:4], N, replace = TRUE) )
								, f7 = gl(2,2,length=N,labels = c("Control", "Treat"))
							  )
	datfram

split.rand.portion( datfram , variable = c("f1","f7") , envir = 0 , split = TRUE)

split.rand.portion( datfram , variable = c("f1","f6","f7") , envir = 0 , split = TRUE) ##
   ## priority = "balance" is default what means that each level MUST appear at least once in datfram.1
   ## what may result in more records than implied by 'nrows'/'portion'
   ## especially when there are many rare levels
split.rand.portion( datfram , variable = c("f1","f6","f7") , priority = "balance" , envir = 0 , split = TRUE) ## as above
## priority = "nrows" or = "portion" forces number of records, thus some levels may not appear
split.rand.portion( datfram , variable = c("f1","f6","f7") , priority = "nrows" , envir = 0 , split = TRUE) ##
split.rand.portion( datfram , variable = c("f1","f6","f7") , priority = "portion" , envir = 0 , split = TRUE) ## the same

split.rand.portion( datfram , variable = c("f1","f6","f7") , nrows = 20 , envir = 0 , split = TRUE) ## OK
split.rand.portion( datfram , variable = c("f1","f6","f7") , nrows = 20 , priority = "nrows" , envir = 0 , split = TRUE) ## OK
split.rand.portion( datfram , variable = c("f1","f6","f7") , nrows = 20, priority = "portion"  , envir = 0 , split = TRUE) ## OK

split.rand.portion( datfram , variable = c("f1","f6","f7") , nrows = 30 , envir = 0 , split = TRUE) ## OK
split.rand.portion( datfram , variable = c("f1","f6","f7") , nrows = 30, priority = "nrows"  , envir = 0 , split = TRUE) ## OK
split.rand.portion( datfram , variable = c("f1","f6","f7") , nrows = 30, priority = "portion"  , envir = 0 , split = TRUE) ## OK


###################
## more variables from environment
f1 = datfram$f1 ; f6 = datfram$f6 ; f7 = datfram$f7
   ## this will not work
split.rand.portion( datfram , variable = c(f1,f6,f7) , priority ="nrows" , envir = 0 , split = TRUE)
   ## strange result appeard, because concatenating of factors gives numeric vector
   c(f1,f6,f7)
   ## while split.rand.portion() reads it as position of variables in data frame... a mess is inevitable
## you need
variable = as.factor(paste(paste(f1,f6,sep=":"),f7,sep=":" ))
split.rand.portion( datfram , variable = variable , priority ="nrows" , envir = 0 , split = TRUE)  ## OK !!!
   class(variable)
## Notice that
variable = paste(paste(f1,f6,sep=":"),f7,sep=":" )   ## IS NOT ENOUGH:
split.rand.portion( datfram , variable = variable , priority ="nrows" , envir = 0 , split = TRUE) ## error because
   class(variable)
   ## character strings are interpreted as names of variables in 'datfram'


#######################################
## 3. More on envir[onments].

##
split.rand.portion( datfram , envir = 1)
datfram

split.rand.portion( datfram , envir = 1 , split  = TRUE)
datfram
datfram.1
datfram.0
rm(datfram.1,datfram.0)

split.rand.portion( datfram , envir = 1 , split = FALSE)
datfram
datfram.1   ## does not exist
datfram.0   ## does not exist

## repeat
split.rand.portion( datfram , envir = 1 , split = FALSE)
datfram

#########
## output may be written to some variable
ll = split.rand.portion( datfram , envir = 1 , split = TRUE)
ll          ## no data frames
datfram     ## unchanged
datfram.1   ## does exist
datfram.0   ## does exist
rm(datfram.1,datfram.0)

ll = split.rand.portion( datfram , envir = 1 , split = FALSE)  ## split = FALSE is default
ll
datfram     ## 'split.X' added
datfram.1   ## does not exist
datfram.0   ## does not exist

#########
## names of data frames are served
 mydf = datfram
split.rand.portion( datfram = mydf , envir = 1 , split  = TRUE)
 mydf
mydf.1
mydf.0
rm(mydf.1,mydf.0)

ll = split.rand.portion( datfram = mydf , envir = 1 , split  = TRUE)
ll["names"]
 mydf
mydf.1
mydf.0
rm(mydf.1,mydf.0)


#############################
## envir = NULL -- minimal output
## build a new datfram
split.rand.portion( datfram , envir = NULL , split = FALSE)
   datfram  ## not changed
## envir = NULL is default
split.rand.portion( datfram , split = FALSE)
   datfram  ## not changed
   ## split = FALSE is default
   split.rand.portion( datfram )
   datfram
## if split = TRUE only the first data frame is returned
split.rand.portion( datfram , split = TRUE)
   datfram  ## not changed

#############################
## envir = 0 -- to the list
## build a new datfram

ll = split.rand.portion( datfram , envir = 0 )
ll
datfram
datfram.1   ## does not exist
datfram.0   ## does not exist
ll["datfram"]

ll = split.rand.portion( datfram , split = TRUE , envir = 0 )
ll
datfram     ## unchanged
datfram.1   ## does not exist
datfram.0   ## does not exist
ll["datfram.1"]
ll["datfram.0"]

## other names of data frame
 mydf = datfram
ll = split.rand.portion( datfram = mydf , split = TRUE , envir = 0 )
ll
ll["names"]
 mydf     ## unchanged
mydf.1   ## does not exist
mydf.0   ## does not exist
ll["names"]
ll["datfram.1"]
ll["datfram.0"]
ll["names"]

#############################
## envir = -1  --  returning resulting data frames to the parent frame -- environment from which the function was called
##   see ?parent.frame

###################
## parent.frame -> .GlobalEnv
split.rand.portion( datfram , envir = -1 , split = FALSE)
   datfram  ## changed

## if split = TRUE
split.rand.portion( datfram , envir = -1 , split = TRUE)  ## returns only a first data frame
   datfram  ## not changed
   datfram.1
   datfram.0
   rm("datfram.1","datfram.0")
   
## Notice lack of any message about new variables or data frames

###################
## But the reason for this option is returning results to the function calling split.rand.portion()

ff = function(df){
   ll = split.rand.portion(datfram = df , split = TRUE , envir = -1)
   vv = c(nrow(df.1),nrow(df.0))
   names(vv) = ll$names
   vv
}

ff(datfram)
##!!! Notice that INTERNAL NAME of data frame is used i.e. "df" not "datfram".
## Moreover, df.1, df.0 do not exist in .GlobalEnv
df.1   ## does not exist
df.0   ## does not exist

#########
## envir = 1 will leave results in .GlobalEnv
gg = function(df){
   ll = split.rand.portion(datfram = df , split = TRUE , envir = 1)
   vv = c(nrow(df.1),nrow(df.0))
   names(vv) = ll$names
   vv
}

gg(datfram)
df.1
df.0
rm("df.1","df.0")

#########
## returning records not drawn, datfram.0, rather then drawn, datfram.1
hh = function(df,portion = .5){
   split.rand.portion(datfram = df, portion = portion , split = TRUE , envir = -1)
   df.0
}

   nrow(datfram) ; portion = .7; nrow(datfram)*portion
hh(datfram,portion)

###################
## n-fold division for cross-validation purposes, with balancing for one of the variable

nfold = function(datfram,fold,variable){
   N = nrow(datfram)
   k = ceiling(N/fold)
   division = rep(0,N)
   df0 = datfram
   for(j in 1:fold){
      ll = split.rand.portion( df0 , nrows = k
               , new.names = c("df1","df0")
               , variable = variable ,priority = "nrows"
               , envir = -1 , split = TRUE)
      division[rownames(datfram) %in% ll$rownames.1] = j
      print(rownames(df0))
   }
   datfram = cbind(datfram,division)
   datfram
}

datfram ## generate a new one with N=50
   df.div = nfold(datfram,fold=5,variable="f7")
   df.div
table(df.div$division)
table(df.div$division,df.div$f7)   ## OK!!!

##########################################################
## END OF EXAMPLES
##########################################################


#############################################################################################################
## some "trials & errors"


ll = split.rand.portion( datfram , portion = .5	, nrows = NULL	, variable = NULL	, first.level = NULL	, balance = NULL
		, priority = "balance"	, split = FALSE , envir = 0 , new.names = NULL )
ll

ls()

df_1
df_0

datfram.1
datfram.0


v_f6 = variable
portion = .6


a = as.name("x")
a
b = as.name("y")
b

nn = c(a,b)

x = 1:3; y = rnorm(3)

nn

nn[[1]]
eval(nn[[1]])

###################################################
environment()
parent.env(environment())
search()

fun1 = function(){
   print(environment())
   print(parent.env(environment()))
   assign("aa",11,pos=parent.env(environment()))
   print(aa)
   ##
   print(sys.parent())
   print(sys.frame())
   print(parent.frame())
   assign("bb",22,parent.frame())
}

fun1()

fun2 = function(){
   fun1()
   cat("\n")
   print(aa)
   cat("\n")
   print(environment())
   print(parent.env(environment()))
   cat("\n")
   bb
}

fun2()
bb

}

rm(dummy)